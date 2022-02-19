#![no_main]
use libfuzzer_sys::fuzz_target;
use economic_checks::*;

fuzz_target!(|params: SimpleParams| {
    economic_checks::run_target(&Params::Simple(params), simple_target);
});
