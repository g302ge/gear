#![no_main]
use libfuzzer_sys::fuzz_target;
use economic_checks::*;

fuzz_target!(|params: ComposerParams| {
    economic_checks::run_target(&Params::Composer(params), composer_target);
});
