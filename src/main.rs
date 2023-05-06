mod cli;
mod uxn;

use std::env;
use std::fs;
use std::io::Read;

fn main() {
    let mut args = env::args();
    let mut rom = vec![];
    let mut file = fs::File::open(args.nth(1).unwrap()).unwrap();
    file.read_to_end(&mut rom).unwrap();
    cli::run_uxncli(&rom, std::env::args());
}
