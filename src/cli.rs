use crate::uxn;

use std::io::Read;

#[repr(u8)]
enum ConsoleType {
    NoQueue = 0,
    StdIn = 1,
    Argument = 2,
    ArgumentSeparator = 3,
    ArgumentEnd = 4,
}

macro_rules! device_u8_getter {
    ($name: ident, $offset: literal) => {
        fn $name(uxn: &uxn::Uxn) -> u8 {
            uxn.io[$offset]
        }
    };
}

macro_rules! device_u8_setter {
    ($name: ident, $offset: literal) => {
        fn $name(uxn: &mut uxn::Uxn, value: u8) {
            uxn.io[$offset] = value;
        }
    };
}

macro_rules! device_u16_getter {
    ($name: ident, $offset: literal) => {
        fn $name(uxn: &uxn::Uxn) -> u16 {
            uxn::bytes_to_short([uxn.io[$offset], uxn.io[$offset + 1]])
        }
    };
}

device_u8_getter!(system_state, 0x0f);
device_u16_getter!(console_vector, 0x10);
device_u8_setter!(console_read, 0x12);

// #[repr(packed(1))]
// struct System {
//     halt: u16,
//     expansion: u16,
//     pad_: u16,
//     metadata: u16,
//     red: u16,
//     green: u16,
//     blue: u16,
//     debug: u8,
//     state: u8,
// }
//
// #[repr(packed(1))]
// struct Console {
//     vector: u16,
//     read: u8,
//     pad_: u32,
//     console_type: ConsoleType,
//     write: u8,
//     error: u8,
//     pad__: [u8; 6],
// }
//
// #[repr(packed(1))]
// struct File {
//     vector: u16,
//     success: u16,
//     stat: u16,
//     delete: u8,
//     append: u8,
//     name: u16,
//     length: u16,
//     read: u16,
//     write: u16,
// }
//
// #[repr(packed(1))]
// struct IOMap {
//     system: System,
//     console: Console,
//     pad_: [u8; 0x80],
//     file: [File; 2],
//     pad__: [u8; 0x40],
// }

struct Cli {}

impl uxn::Host for Cli {
    fn dei(&mut self, cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        None
    }
    fn deo(&mut self, _cpu: &mut uxn::Uxn, target: u8, value: u16, short_mode: bool) -> Option<()> {
        println!(
            "target {:x} value {:x} short_mode {}",
            target, value, short_mode
        );
        Some(())
    }
}

impl Cli {}

pub fn run_uxncli(rom: &[u8]) {
    let mut vm = uxn::Uxn::boot(rom);
    let mut host = Cli {};
    vm.eval(&mut host, uxn::PAGE_PROGRAM as u16).unwrap();

    while system_state(&vm) == 0 {
        let mut byte = [0];
        match std::io::stdin().read(&mut byte) {
            Ok(amount_read) => {
                if amount_read != 0 {
                    console_read(&mut vm, byte[0]);
                    let entry = console_vector(&vm);
                    vm.eval(&mut host, entry).unwrap();
                }
            }
            _ => break,
        }
    }
}
