mod uxn;

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
    friend: u16,
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

fn not_in_current_directory(path: &std::path::Path) -> bool {
    return || -> Option<bool> {
        let path = path.canonicalize().ok()?;
        let parent = path.parent()?;
        let cwd = std::env::current_dir().ok()?;
        return Some(parent == cwd);
    }()
    .unwrap_or(false);
}

impl File {
    fn get_name(&self, uxn: &uxn::Uxn) -> Option<String> {
        let address_of_name_in_uxn = uxn::short_to_host_byte_order(self.name);
        let mut name_bytes = vec![];
        loop {
            let byte = uxn.read8(address_of_name_in_uxn + name_bytes.len() as u16)?;
            if byte == 0 {
                break;
            }

            name_bytes.push(byte);
        }

        let name = String::from_utf8(name_bytes).ok()?;

        if !not_in_current_directory(std::path::Path::new(&name)) {
            eprintln!("{} violated sandbox rules", name);
            return None;
        }

        return Some(name);
    }

    fn get_operation_length(&self) -> u16 {
        uxn::short_to_host_byte_order(self.length)
    }
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

#[repr(packed(1))]
struct DeviceIOMemory {
    system: System,
    console: Console,
    _pad: [u8; 0x80],
    file: [File; 2],
    datetime: DateTime,
    __pad: [u8; 0x30],
}

impl Default for DeviceIOMemory {
    fn default() -> Self {
        DeviceIOMemory {
            system: Default::default(),
            console: Default::default(),
            _pad: [0; 0x80],
            file: Default::default(),
            datetime: Default::default(),
            __pad: [0; 0x30],
        }
    }
}

impl<'a> DeviceIOMemory {
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

enum OpenedPath {
    None,
    File(std::fs::File),
    Directory(std::fs::ReadDir),
}

impl Default for OpenedPath {
    fn default() -> Self {
        OpenedPath::None
    }
}

/// A subset of the varvara machine
/// Supports the system, console, file and datetime devices
#[derive(Default)]
struct UxnCli {
    io_memory: DeviceIOMemory,
    open_files: [OpenedPath; 2],
}

macro_rules! targeted_device_field {
    ($target:expr, $short_mode:expr, $device:ident, $field: ident) => {{
        let base: *const UxnCli = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).io_memory.$device.$field) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;

        if $short_mode {
            ($target == offset) || (($target - 1) == offset)
        } else {
            $target == offset
        }
    }};
    ($target:expr, $short_mode:expr, $device:ident, $device_index:expr, $field: ident) => {{
        let base: *const UxnCli = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).io_memory.$device[$device_index].$field) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;

        if $short_mode {
            ($target == offset) || (($target - 1) == offset)
        } else {
            $target == offset
        }
    }};
}

impl std::fmt::Display for uxn::Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let head = self.head as usize;
        let data = &self.data[0..head];
        write!(f, "{:x?}", data)
    }
}

impl uxn::Host for UxnCli {
    fn dei(&mut self, _cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        if short_mode {
            self.io_memory.read16(target)
        } else {
            self.io_memory.read8(target).map(|x| x as u16)
        }
    }

    fn deo(&mut self, cpu: &mut uxn::Uxn, target: u8, value: u16, short_mode: bool) -> Option<()> {
        if short_mode {
            self.io_memory.write16(target, value)?;
        } else {
            self.io_memory.write8(target, value as u8)?;
        }

        if targeted_device_field!(target, short_mode, console, write) {
            let bytes = [self.io_memory.console.write];
            std::io::stdout().write(&bytes).unwrap();
        }

        if targeted_device_field!(target, short_mode, console, error) {
            let bytes = [self.io_memory.console.error];
            std::io::stderr().write(&bytes).unwrap();
        }

        if targeted_device_field!(target, short_mode, system, debug)
            && self.io_memory.system.debug != 0
        {
            eprintln!("Working stack: {}", cpu.working_stack);
            eprintln!("Return stack: {}", cpu.return_stack);
            std::io::stderr().flush().unwrap();
        }

        for i in 0..self.io_memory.file.len() {
            if targeted_device_field!(target, short_mode, file, i, name) {
                || -> Option<()> {
                    let name = self.io_memory.file[i].get_name(cpu)?;
                    let path = std::path::Path::new(&name);
                    if !path.exists() {
                        let file = std::fs::File::create(path).ok()?;
                        self.open_files[i] = OpenedPath::File(file);
                    } else if path.is_file() {
                        let file = std::fs::File::open(path).ok()?;
                        self.open_files[i] = OpenedPath::File(file);
                    } else if path.is_dir() {
                        let dir = std::fs::read_dir(path).ok()?;
                        self.open_files[i] = OpenedPath::Directory(dir);
                    } else {
                        return None;
                    }
                    Some(())
                }();
            }

            if targeted_device_field!(target, short_mode, file, i, read) {
                let bytes_read = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File(file) => file
                            .read(cpu.slice_mut(
                                uxn::short_to_host_byte_order(self.io_memory.file[i].read),
                                self.io_memory.file[i].get_operation_length(),
                            )?)
                            .ok()
                            .map(|x| x as u16),
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = uxn::short_from_host_byte_order(bytes_read);
            }

            // TODO: if targeted_device_field!(target, short_mode, file, i, write) {}

            // TODO: if targeted_device_field!(target, short_mode, file, i, stat) {}

            // TODO: if targeted_device_field!(target, short_mode, file, i, delete) {}
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
    host.io_memory.console.console_type = kind;
    host.io_memory.console.read = byte;
    let entry = uxn::short_to_host_byte_order(host.io_memory.console.vector);
    return vm.eval(host, entry);
}

fn main() {
    let mut args = std::env::args();
    let mut rom = vec![];
    let mut file = std::fs::File::open(args.nth(1).unwrap()).unwrap();
    file.read_to_end(&mut rom).unwrap();

    let mut vm = uxn::Uxn::boot(&rom);
    let mut host = UxnCli::default();
    vm.eval(&mut host, uxn::PAGE_PROGRAM as u16).unwrap();

    // Process arguments
    let args_len = args.len();
    for (i, arg) in args.enumerate() {
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
    while host.io_memory.system.state == 0 {
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
