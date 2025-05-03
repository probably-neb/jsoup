#![no_main]

extern crate json_inc;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    let _ = json_inc::parse(data);
});
