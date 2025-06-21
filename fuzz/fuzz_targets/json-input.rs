#![no_main]

extern crate jsoup;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    let _ = jsoup::parse(data);
});
