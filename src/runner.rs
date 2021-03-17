use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
    vec,
};

use anyhow::Result;
use codec::{Decode, Encode};
use wasmtime::unix::StoreExt;
use wasmtime::{Memory as WasmMemory, Module};

use crate::{
    env::{Environment, Ext as EnvExt},
    memory::{Allocations, MemoryContext, PageNumber},
    message::{IncomingMessage, Message, MessageContext, OutgoingMessage},
    program::{Program, ProgramId},
    storage::{AllocationStorage, MessageQueue, ProgramStorage, Storage},
};

#[derive(Clone, Debug, Decode, Encode)]
pub struct Config {
    static_pages: PageNumber,
    max_pages: PageNumber,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            static_pages: BASIC_PAGES.into(),
            max_pages: MAX_PAGES.into(),
        }
    }
}

fn handle_sigsegv(
    program_id: u64,
    allocations: Vec<PageNumber>,
    static_pages: PageNumber,
    mut touched: RefMut<Vec<(PageNumber, PageAction)>>,
    base: *mut u8,
    signum: libc::c_int,
    siginfo: *const libc::siginfo_t,
) -> bool {
    // SIGSEGV on Linux, SIGBUS on Mac
    if libc::SIGSEGV == signum || libc::SIGBUS == signum {
        let si_addr: *mut libc::c_void = unsafe { (*siginfo).si_addr() };

        // Any signal from within module's memory we handle ourselves
        let page = ((si_addr as usize) - (base as usize)) / BASIC_PAGE_SIZE;

        // Set the base address of the page that the program is trying to access
        let base = base.wrapping_add(page * BASIC_PAGE_SIZE);
        let length = BASIC_PAGE_SIZE;

        if let Ok(q) = region::query(si_addr as *mut u8) {
            match q.protection {
                region::Protection::NONE => {
                    // Set READ prrotection
                    unsafe {
                        libc::mprotect(base as *mut libc::c_void, length, libc::PROT_READ);
                    }
                    // log::debug!(
                    //     "MEMORY: #{} ACCESS PAGE {}",
                    //     program_id,
                    //     BASIC_PAGES as usize + page
                    // );
                    touched.push((static_pages + (page as u32).into(), PageAction::Read));
                    true
                }
                region::Protection::READ => {
                    if allocations.contains(&(static_pages + (page as u32).into())) {
                        // Remove protections so the execution may resume
                        unsafe {
                            libc::mprotect(
                                base as *mut libc::c_void,
                                length,
                                libc::PROT_READ | libc::PROT_WRITE,
                            );
                        }
                        log::debug!(
                            "MEMORY: #{} ACCESS PAGE {} WRITE",
                            program_id,
                            static_pages.raw() as usize + page
                        );
                        touched.push((static_pages + (page as u32).into(), PageAction::Write));
                        true
                    } else {
                        false
                    }
                }
                _ => true,
            }
        } else {
            false
        }
    } else {
        // Otherwise, we forward to wasmtime's signal handler.
        false
    }
}

pub struct Runner<AS: AllocationStorage + 'static, MQ: MessageQueue, PS: ProgramStorage> {
    pub(crate) program_storage: PS,
    pub(crate) message_queue: MQ,
    pub(crate) memory: WasmMemory,
    pub(crate) allocations: Allocations<AS>,
    pub(crate) config: Config,
    env: Environment<Ext<AS>>,
}

impl<AS: AllocationStorage + 'static, MQ: MessageQueue, PS: ProgramStorage> Runner<AS, MQ, PS> {
    pub fn new(config: &Config, storage: Storage<AS, MQ, PS>, persistent_memory: &[u8]) -> Self {
        // memory need to be at least static_pages + persistent_memory length (in pages)
        let persistent_pages = persistent_memory.len() / BASIC_PAGE_SIZE;
        let total_pages = config.static_pages.raw() + persistent_pages as u32;

        let env = Environment::new();
        let memory = env.create_memory(total_pages);

        let persistent_region_start = config.static_pages.raw() as usize * BASIC_PAGE_SIZE;

        memory
            .write(persistent_region_start, persistent_memory)
            .map_err(|_e| log::error!("Write memory err: {}", _e))
            .ok();

        let Storage {
            allocation_storage,
            message_queue,
            program_storage,
        } = storage;

        Self {
            program_storage,
            message_queue,
            memory,
            allocations: Allocations::new(allocation_storage),
            config: config.clone(),
            env,
        }
    }

    pub fn run_next(&mut self) -> Result<usize> {
        let next_message = match self.message_queue.dequeue() {
            Some(msg) => msg,
            None => {
                return Ok(0);
            }
        };

        if next_message.dest() == 0.into() {
            match String::from_utf8(next_message.payload().to_vec()) {
                Ok(s) => log::debug!("UTF-8 msg to /0: {}", s),
                Err(_) => {
                    log::debug!("msg to /0: {:?}", next_message.payload())
                }
            }
            Ok(1)
        } else {
            let mut context = self.create_context();
            let program = self
                .program_storage
                .get_mut(next_message.dest())
                .expect("Program not found");

            run(
                &mut self.env,
                &mut context,
                program,
                EntryPoint::Handle,
                &next_message.into(),
            )?;
            self.message_queue
                .queue_many(context.message_buf.drain(..).collect());
            Ok(1)
        }
    }

    pub fn complete(self) -> (Storage<AS, MQ, PS>, Vec<u8>) {
        let persistent_memory = {
            let non_static_region_start = self.static_pages().raw() as usize * BASIC_PAGE_SIZE;
            let mut persistent_memory = vec![0; self.memory.data_size() - non_static_region_start];
            self.memory
                .read(non_static_region_start, persistent_memory.as_mut_slice())
                .map_err(|_e| log::error!("Read memory err: {}", _e))
                .ok();
            persistent_memory
        };

        let Runner {
            program_storage,
            message_queue,
            allocations,
            ..
        } = self;

        let allocation_storage = match allocations.drain() {
            Ok(v) => v,
            Err(e) => {
                panic!("Panic finalizing allocations: {:?}", e)
            }
        };

        (
            Storage {
                allocation_storage,
                message_queue,
                program_storage,
            },
            persistent_memory,
        )
    }

    pub fn static_pages(&self) -> PageNumber {
        self.config.static_pages
    }

    pub fn max_pages(&self) -> PageNumber {
        self.config.max_pages
    }

    pub fn create_context(&self) -> RunningContext<AS> {
        RunningContext::new(&self.config, self.memory.clone(), self.allocations.clone())
    }

    pub fn init_program(
        &mut self,
        program_id: ProgramId,
        code: Vec<u8>,
        init_msg: Vec<u8>,
    ) -> Result<()> {
        if let Some(program) = self.program_storage.get_mut(program_id) {
            program.set_code(code.to_vec());
            program.clear_static();
        } else {
            self.program_storage
                .set(Program::new(program_id, code, vec![]));
        }

        self.allocations.clear(program_id);

        let mut context = self.create_context();
        let program = self
            .program_storage
            .get_mut(program_id)
            .expect("Added above; cannot fail");
        let msg = IncomingMessage::new_system(init_msg.into());
        run(&mut self.env, &mut context, program, EntryPoint::Init, &msg)?;
        self.message_queue
            .queue_many(context.message_buf.drain(..).collect());
        Ok(())
    }

    pub fn queue_message(&mut self, destination: ProgramId, payload: Vec<u8>) {
        self.message_queue
            .queue(Message::new_system(destination, payload.into()))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum EntryPoint {
    Handle,
    Init,
}

impl From<EntryPoint> for &'static str {
    fn from(e: EntryPoint) -> &'static str {
        match e {
            EntryPoint::Handle => "handle",
            EntryPoint::Init => "init",
        }
    }
}

static BASIC_PAGES: u32 = 256;
static BASIC_PAGE_SIZE: usize = 65536;
static MAX_PAGES: u32 = 16384;

pub struct RunningContext<AS: AllocationStorage> {
    config: Config,
    memory: WasmMemory,
    allocations: Allocations<AS>,
    message_buf: Vec<Message>,
}

impl<AS: AllocationStorage> RunningContext<AS> {
    pub fn new(config: &Config, memory: WasmMemory, allocations: Allocations<AS>) -> Self {
        Self {
            config: config.clone(),
            message_buf: vec![],
            memory,
            allocations,
        }
    }

    pub fn wasmtime_memory(&self) -> wasmtime::Memory {
        self.memory.clone()
    }

    pub fn static_pages(&self) -> PageNumber {
        self.config.static_pages
    }

    pub fn max_pages(&self) -> PageNumber {
        self.config.max_pages
    }

    pub fn push_message(&mut self, msg: Message) {
        self.message_buf.push(msg)
    }
}

#[derive(Clone, Debug, Decode, Encode, derive_more::From)]
enum PageAction {
    Read,
    Write,
}

#[derive(Clone, Debug, Decode, Default, Encode, derive_more::From)]
pub struct RunResult {
    allocations: Vec<PageNumber>,
    touched: Vec<(PageNumber, PageAction)>,
    messages: Vec<OutgoingMessage>,
}

struct Ext<AS: AllocationStorage + 'static> {
    memory_context: MemoryContext<AS>,
    messages: MessageContext,
}

impl<AS: AllocationStorage + 'static> EnvExt for Ext<AS> {
    fn alloc(&mut self, pages: PageNumber) -> Result<PageNumber, &'static str> {
        self.memory_context
            .alloc(pages)
            .map_err(|_e| "Allocation error")
    }

    fn send(&mut self, msg: OutgoingMessage) -> Result<(), &'static str> {
        self.messages.send(msg).map_err(|_e| "Message send error")
    }

    fn source(&mut self) -> Option<ProgramId> {
        self.messages.current().source()
    }

    fn free(&mut self, ptr: PageNumber) -> Result<(), &'static str> {
        self.memory_context.free(ptr).map_err(|_e| "Free error")
    }

    fn debug(&mut self, data: &str) -> Result<(), &'static str> {
        log::debug!("DEBUG: {}", data);
        Ok(())
    }

    fn set_mem(&mut self, ptr: usize, val: &[u8]) -> Result<(), &'static str> {
        self.memory_context
            .memory()
            .write(ptr, val)
            .map_err(|_e| "Set mem error")
    }

    fn get_mem(&mut self, ptr: usize, val: &mut [u8]) -> Result<(), &'static str> {
        self.memory_context
            .memory()
            .read(ptr, val)
            .map_err(|_e| "Set mem error")
    }

    fn msg(&mut self) -> &[u8] {
        self.messages.current().payload()
    }
}

fn run<AS: AllocationStorage + 'static>(
    env: &mut Environment<Ext<AS>>,
    context: &mut RunningContext<AS>,
    program: &mut Program,
    entry_point: EntryPoint,
    message: &IncomingMessage,
) -> Result<RunResult> {
    let module = Module::new(env.engine(), program.code())?;

    let mut ext = Ext {
        memory_context: MemoryContext::new(
            program.id(),
            Box::new(context.wasmtime_memory()),
            context.allocations.clone(),
            context.static_pages(),
            context.max_pages(),
        ),
        messages: MessageContext::new(message.clone()),
    };

    // Set static pages from saved program state.

    let static_area = program.static_pages().to_vec();

    let static_pages = context.static_pages();

    // Lock access to memory
    let (shared_base, _) = ext
        .memory_context
        .memory()
        .lock(static_pages, context.max_pages() - context.static_pages());
    let program_id = program.id().0;

    let allocations = context.allocations.clone().get_program_pages(program.id());
    let touched: Rc<RefCell<Vec<(PageNumber, PageAction)>>> = Rc::new(RefCell::new(Vec::new()));
    let touched_clone = touched.clone();

    // Set signal handler
    unsafe {
        env.store().set_signal_handler(move |signum, siginfo, _| {
            handle_sigsegv(
                program_id,
                allocations.clone(),
                static_pages,
                touched_clone.borrow_mut(),
                shared_base,
                signum,
                siginfo,
            )
        });
    }

    let (res, mut ext) = env.setup_and_run(
        ext,
        module,
        static_area,
        context.wasmtime_memory(),
        move |instance| {
            instance
                .get_func(entry_point.into())
                .ok_or_else(|| {
                    anyhow::format_err!(
                        "failed to find `{}` function export",
                        Into::<&'static str>::into(entry_point)
                    )
                })
                .and_then(|entry_func| entry_func.call(&[]))
                .map(|_| ())
        },
    );

    // Unlock memory for future use
    ext.memory_context.memory().unlock(
        context.static_pages(),
        context.max_pages() - context.static_pages(),
    );

    res.map(move |_| {
        program
            .static_pages_mut()
            .resize(context.static_pages().raw() as usize * BASIC_PAGE_SIZE, 0);
        ext.get_mem(0, program.static_pages_mut())
            .map_err(|_e| log::error!("Read memory err: {}", _e))
            .ok();

        let mut messages = vec![];
        for outgoing_msg in ext.messages.drain() {
            messages.push(outgoing_msg.clone());
            context.push_message(outgoing_msg.into_message(program.id()));
        }

        RunResult {
            allocations: context.allocations.get_program_pages(program.id()),
            touched: touched.borrow().clone(),
            messages,
        }
    })
}
