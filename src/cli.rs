use crate::uxn;

use std::io::{Read, Write};

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

#[derive(Default)]
#[repr(packed(1))]
struct DateTime {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    dotw: u8,
    doty: u16,
    isdst: u8,
    _pad: [u8; 5],
}

/// A subset of the varvara machine
/// Supports the system, console, file and datetime devices
#[repr(packed(1))]
struct UxnCli {
    system: System,
    console: Console,
    _pad: [u8; 0x80],
    file: [File; 2],
    datetime: DateTime,
    __pad: [u8; 0x30],
}

impl Default for UxnCli {
    fn default() -> UxnCli {
        UxnCli {
            system: Default::default(),
            console: Default::default(),
            _pad: [0; 0x80],
            file: Default::default(),
            datetime: Default::default(),
            __pad: [0; 0x30],
        }
    }
}

impl<'a> UxnCli {
    fn as_raw_bytes(&'a self) -> &'a [u8; uxn::IO_BYTE_COUNT] {
        unsafe { std::mem::transmute(self) }
    }

    fn read8(&self, offset: u8) -> Option<u8> {
        self.as_raw_bytes().get(offset as usize).map(|x| *x)
    }

    fn read16(&self, offset: u8) -> Option<u16> {
        let low = self.read8(offset)?;
        let high = self.read8(offset + 1)?;
        return Some(uxn::bytes_to_short([low, high]));
    }

    fn as_raw_bytes_mut(&'a mut self) -> &'a mut [u8; uxn::IO_BYTE_COUNT] {
        unsafe { std::mem::transmute(self) }
    }

    fn write8(&mut self, offset: u8, value: u8) -> Option<()> {
        *self.as_raw_bytes_mut().get_mut(offset as usize)? = value;
        Some(())
    }

    fn write16(&mut self, offset: u8, value: u16) -> Option<()> {
        let value = uxn::short_to_bytes(value);

        let high = self.as_raw_bytes_mut().get_mut((offset + 1) as usize)?;
        *high = value[1];

        let low = self.as_raw_bytes_mut().get_mut(offset as usize)?;
        *low = value[0];

        Some(())
    }
}

macro_rules! targeted_device_field {
    ($target:expr, $short_mode:expr, $device:ident, $field: ident) => {{
        let base: *const UxnCli = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).$device.$field) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;

        if $short_mode {
            ($target == offset) || (($target - 1) == offset)
        } else {
            $target == offset
        }
    }};
}

impl uxn::Host for UxnCli {
    fn dei(&mut self, _cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        if short_mode {
            self.read8(target).map(|x| x as u16)
        } else {
            self.read16(target)
        }
    }

    fn deo(&mut self, _cpu: &mut uxn::Uxn, target: u8, value: u16, short_mode: bool) -> Option<()> {
        if short_mode {
            self.write16(target, value)?;
        } else {
            self.write8(target, value as u8)?;
        }

        if targeted_device_field!(target, short_mode, console, write) {
            let bytes = [self.console.write];
            std::io::stdout().write(&bytes).unwrap();
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
