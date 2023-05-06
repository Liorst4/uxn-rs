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

impl Default for ConsoleType {
    fn default() -> Self {
        ConsoleType::NoQueue
    }
}

#[derive(Default)]
#[repr(packed(1))]
struct System {
    halt: u16,
    expansion: u16,
    _pad: u16,
    metadata: u16,
    _red: u16,
    _green: u16,
    _blue: u16,
    debug: u8,
    state: u8,
}

#[derive(Default)]
#[repr(packed(1))]
struct Console {
    vector: u16,
    read: u8,
    _pad: u32,
    console_type: ConsoleType,
    write: u8,
    error: u8,
    __pad: [u8; 6],
}

#[derive(Default)]
#[repr(packed(1))]
struct File {
    vector: u16,
    success: u16,
    stat: u16,
    delete: u8,
    append: u8,
    name: u16,
    length: u16,
    read: u16,
    write: u16,
}

/// A subset of the varvara machine
/// Supports the system, console, file and datetime devices
#[repr(packed(1))]
struct UxnCli {
    system: System,
    console: Console,
    _pad: [u8; 0x80],
    file: [File; 2],
    __pad: [u8; 0x40],
}

impl Default for UxnCli {
    fn default() -> UxnCli {
        UxnCli {
            system: Default::default(),
            console: Default::default(),
            _pad: [0; 0x80],
            file: Default::default(),
            __pad: [0; 0x40],
        }
    }
}

impl UxnCli {
    unsafe fn as_raw_bytes(&self) -> &[u8; uxn::IO_BYTE_COUNT] {
        std::mem::transmute(self)
    }

    unsafe fn read8(&self, offset: u8) -> Option<u8> {
        self.as_raw_bytes().get(offset as usize).map(|x| *x)
    }

    unsafe fn read16(&self, offset: u8) -> Option<u16> {
        let low = self.read8(offset)?;
        let high = self.read8(offset + 1)?;
        return Some(uxn::bytes_to_short([low, high]));
    }

    unsafe fn as_raw_bytes_mut(&mut self) -> &mut [u8; uxn::IO_BYTE_COUNT] {
        std::mem::transmute(self)
    }

    unsafe fn write8(&mut self, offset: u8, value: u8) -> Option<()> {
        *self.as_raw_bytes_mut().get_mut(offset as usize)? = value;
        Some(())
    }

    unsafe fn write16(&mut self, offset: u8, value: u16) -> Option<()> {
        let value = uxn::short_to_bytes(value);

        let high = self.as_raw_bytes_mut().get_mut((offset + 1) as usize)?;
        *high = value[1];

        let low = self.as_raw_bytes_mut().get_mut(offset as usize)?;
        *low = value[0];

        Some(())
    }
}

impl uxn::Host for UxnCli {
    fn dei(&mut self, _cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        unsafe {
            if short_mode {
                self.read8(target).map(|x| x as u16)
            } else {
                self.read16(target)
            }
        }
    }

    fn deo(&mut self, _cpu: &mut uxn::Uxn, target: u8, value: u16, short_mode: bool) -> Option<()> {
        unsafe {
            if short_mode {
                self.write16(target, value)?;
            } else {
                self.write8(target, value as u8)?;
            }
        }

        // TODO: Macro for offset_of

        if target == 0x18 {
            // self.console.write
            print!("{}", self.console.write as char);
        }

        Some(())
    }
}

fn inject_console_byte(
    vm: &mut uxn::Uxn,
    host: &mut UxnCli,
    byte: u8,
    kind: ConsoleType,
) -> Result<(), uxn::UxnError> {
    host.console.console_type = kind;
    host.console.read = byte;
    let entry = uxn::short_to_host_byte_order(host.console.vector);
    return vm.eval(host, entry);
}

pub fn run_uxncli(rom: &[u8], args: std::env::Args) {
    let mut vm = uxn::Uxn::boot(rom);
    let mut host = UxnCli::default();
    vm.eval(&mut host, uxn::PAGE_PROGRAM as u16).unwrap();

    // Process arguments
    let args_len = args.len();
    for (i, arg) in args.enumerate() {
        if i == 0 {
            continue;
        }
        for c in arg.as_bytes() {
            inject_console_byte(&mut vm, &mut host, *c, ConsoleType::Argument).unwrap();
        }
        inject_console_byte(
            &mut vm,
            &mut host,
            '\n' as u8,
            if i == (args_len - 1) {
                ConsoleType::ArgumentEnd
            } else {
                ConsoleType::ArgumentSeparator
            },
        )
        .unwrap();
    }

    // Process input
    while host.system.state == 0 {
        let mut byte = [0];
        match std::io::stdin().read(&mut byte) {
            Ok(amount_read) => {
                if amount_read != 0 {
                    inject_console_byte(&mut vm, &mut host, byte[0], ConsoleType::StdIn).unwrap();
                }
            }
            _ => break,
        }
    }
}
