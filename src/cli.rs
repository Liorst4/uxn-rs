mod uxn;

use std::io::{Read, Seek, Write};

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

fn read_dir_entry(path: &std::path::Path) -> Option<Vec<u8>> {
    let metadata = path.metadata().ok()?;
    let size_str: String;
    if metadata.is_file() {
        let size = metadata.len();
        if size <= u16::MAX as u64 {
            size_str = format!("{:04x}", size);
        } else {
            size_str = "????".to_string();
        }
    } else {
        size_str = "----".to_string();
    }

    return Some(format!("{} {}", size_str, path.to_str()?).into_bytes());
}

fn read_dir(path: &std::path::Path) -> Option<Vec<Vec<u8>>> {
    let mut entries = vec![];
    for entry in std::fs::read_dir(path).ok()? {
        entries.push(read_dir_entry(&entry.ok()?.path())?);
    }
    return Some(entries);
}

enum OpenedPath {
    None,
    File {
        path: std::path::PathBuf,
        handle: std::fs::File,
    },
    Directory {
        path: std::path::PathBuf,
        entries: Vec<Vec<u8>>,
        read_index: usize,
    },
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
                        self.open_files[i] = OpenedPath::File {
                            path: path.to_owned(),
                            handle: file,
                        };
                    } else if path.is_file() {
                        let file = std::fs::File::open(path).ok()?;
                        self.open_files[i] = OpenedPath::File {
                            path: path.to_owned(),
                            handle: file,
                        };
                    } else if path.is_dir() {
                        self.open_files[i] = OpenedPath::Directory {
                            path: path.to_owned(),
                            entries: read_dir(path)?,
                            read_index: 0,
                        };
                    } else {
                        return None;
                    }
                    Some(())
                }();
            }

            if targeted_device_field!(target, short_mode, file, i, read) {
                let bytes_read = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File { path: _, handle } => handle
                            .read(cpu.slice_mut(
                                uxn::short_to_host_byte_order(self.io_memory.file[i].read),
                                self.io_memory.file[i].get_operation_length(),
                            )?)
                            .ok()
                            .map(|x| x as u16),
                        OpenedPath::Directory {
                            path: _,
                            entries,
                            read_index,
                        } => {
                            let max_length = self.io_memory.file[i].get_operation_length() as usize;
                            let mut entries_read = vec![];
                            while *read_index < entries.len() {
                                let bytes_to_append = entries[*read_index].len() + 1;
                                if bytes_to_append + entries_read.len() > max_length {
                                    break;
                                }
                                entries_read.extend(&entries[*read_index]);
                                entries_read.push('\n' as u8);
                                *read_index += 1;
                            }
                            cpu.slice_mut(
                                uxn::short_to_host_byte_order(self.io_memory.file[i].read),
                                entries_read.len() as u16,
                            )?
                            .copy_from_slice(&entries_read);
                            Some(entries_read.len() as u16)
                        }
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = uxn::short_from_host_byte_order(bytes_read);
            }

            if targeted_device_field!(target, short_mode, file, i, write) {
                let bytes_written = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File { path: _, handle } => {
                            let length = self.io_memory.file[i].get_operation_length();
                            let src = cpu.slice_mut(
                                uxn::short_to_host_byte_order(self.io_memory.file[i].write),
                                length,
                            )?;

                            if self.io_memory.file[i].append == 0 {
                                handle.rewind().ok()?;
                            }

                            return handle.write(src).map(|x| x as u16).ok();
                        }
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = bytes_written;
            }

            if targeted_device_field!(target, short_mode, file, i, stat) {
                let bytes_written = || -> Option<u16> {
                    let entry = read_dir_entry(match &self.open_files[i] {
                        OpenedPath::File { path, handle: _ } => Some(path),
                        OpenedPath::Directory {
                            path,
                            entries: _,
                            read_index: __,
                        } => Some(path),
                        OpenedPath::None => None,
                    }?)?;
                    let length = std::cmp::min(
                        self.io_memory.file[i].get_operation_length(),
                        entry.len() as u16,
                    );
                    let dst = cpu.slice_mut(
                        uxn::short_to_host_byte_order(self.io_memory.file[i].stat),
                        length,
                    )?;
                    dst.copy_from_slice(&entry);
                    Some(length)
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = bytes_written;
            }

            if targeted_device_field!(target, short_mode, file, i, delete)
                && self.io_memory.file[i].delete != 0
            {
                match &mut self.open_files[i] {
                    OpenedPath::File { path, handle: _ } => {
                        if std::fs::remove_file(&path).is_err() {
                            eprintln!("Could not remove {:?}", path);
                        }
                    }
                    OpenedPath::Directory {
                        path,
                        entries: _,
                        read_index: __,
                    } => {
                        if std::fs::remove_dir(&path).is_err() {
                            eprintln!("Could not remove {:?}", path);
                        }
                    }
                    OpenedPath::None => {}
                }

                self.open_files[i] = Default::default();
            }
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
