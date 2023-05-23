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
    _pad: [u8; 6], // TODO: expansion, friend and metadata
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
    _vector: u16,
    success: u16,
    stat: u16,
    delete: u8,
    append: u8,
    name: u16,
    length: u16,
    read: u16,
    write: u16,
}

fn complies_with_sandbox_rules(path: &std::path::Path) -> bool {
    if path.is_absolute() {
        eprintln!("ABSOLUTE PATHS AREN'T ALLOWED");
        return false;
    }

    if path.is_symlink() {
        eprintln!("SYMLINKS AREN'T ALLOWED");
        return false;
    }

    for i in path {
        if i == ".." {
            eprintln!("RELATIVE PATHS WITH .. AREN'T ALLOWED");
            return false;
        }
    }

    return true;
}

impl File {
    fn path<'a>(&self, uxn: &'a uxn::Uxn) -> Option<&'a std::path::Path> {
        let address_of_name_in_uxn = uxn::uxn_short_to_host_short(self.name);
        let mut name_byte_count: u16 = 0;
        while uxn.read8(address_of_name_in_uxn + name_byte_count)? != 0 {
            name_byte_count += 1;
        }
        let name = uxn.slice(address_of_name_in_uxn, name_byte_count)?;
        let name = std::str::from_utf8(name).ok()?;
        let path = std::path::Path::new(name);
        if !complies_with_sandbox_rules(path) {
            eprintln!("{} violated sandbox rules", name);
            return None;
        }
        return Some(path);
    }

    fn get_operation_length(&self) -> u16 {
        uxn::uxn_short_to_host_short(self.length)
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

impl DateTime {
    fn update(&mut self) {
        let mut calendar_time = libc::tm {
            tm_sec: 0,
            tm_min: 0,
            tm_hour: 0,
            tm_mday: 0,
            tm_mon: 0,
            tm_year: 0,
            tm_wday: 0,
            tm_yday: 0,
            tm_isdst: 0,
            tm_gmtoff: 0,
            tm_zone: std::ptr::null(),
        };
        let mut now: libc::time_t = 0;
        unsafe {
            libc::time(&mut now);
            libc::localtime_r(&now, &mut calendar_time);
        }

        self.year = calendar_time.tm_year as u16;
        self.month = calendar_time.tm_mon as u8;
        self.day = calendar_time.tm_mday as u8;
        self.hour = calendar_time.tm_hour as u8;
        self.minute = calendar_time.tm_min as u8;
        self.second = calendar_time.tm_sec as u8;
        self.dotw = calendar_time.tm_wday as u8;
        self.doty = calendar_time.tm_yday as u16;
        self.isdst = calendar_time.tm_isdst as u8;
    }
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
        return Some(uxn::uxn_bytes_to_host_short([low, high]));
    }

    fn as_raw_bytes_mut(&'a mut self) -> &'a mut [u8; uxn::IO_BYTE_COUNT] {
        unsafe { std::mem::transmute(self) }
    }

    fn write8(&mut self, offset: u8, value: u8) -> Option<()> {
        *self.as_raw_bytes_mut().get_mut(offset as usize)? = value;
        Some(())
    }

    fn write16(&mut self, offset: u8, value: u16) -> Option<()> {
        let value = uxn::host_short_to_uxn_bytes(value);

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
        let base: *const DeviceIOMemory = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).$device.$field) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;
        // TODO: Make `offset` a constant

        if $short_mode {
            ($target == offset) || (($target - 1) == offset)
        } else {
            $target == offset
        }
    }};
    ($target:expr, $short_mode:expr, $device:ident, $device_index:expr, $field: ident) => {{
        let base: *const DeviceIOMemory = std::ptr::null();
        let offset = unsafe { std::ptr::addr_of!((*base).$device[$device_index].$field) };
        let offset: usize = unsafe { std::mem::transmute(offset) };
        let offset: u8 = offset as u8;
        // TODO: Make `offset` a constant

        if $short_mode {
            ($target == offset) || (($target - 1) == offset)
        } else {
            $target == offset
        }
    }};
}

impl uxn::Host for UxnCli {
    fn dei(&mut self, _cpu: &mut uxn::Uxn, target: u8, short_mode: bool) -> Option<u16> {
        if targeted_device_field!(target, short_mode, datetime, year)
            || targeted_device_field!(target, short_mode, datetime, month)
            || targeted_device_field!(target, short_mode, datetime, day)
            || targeted_device_field!(target, short_mode, datetime, hour)
            || targeted_device_field!(target, short_mode, datetime, minute)
            || targeted_device_field!(target, short_mode, datetime, second)
            || targeted_device_field!(target, short_mode, datetime, dotw)
            || targeted_device_field!(target, short_mode, datetime, doty)
            || targeted_device_field!(target, short_mode, datetime, isdst)
        {
            self.io_memory.datetime.update();
        }

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
                    let path = self.io_memory.file[i].path(cpu)?;
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
                                uxn::uxn_short_to_host_short(self.io_memory.file[i].read),
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
                                uxn::uxn_short_to_host_short(self.io_memory.file[i].read),
                                entries_read.len() as u16,
                            )?
                            .copy_from_slice(&entries_read);
                            Some(entries_read.len() as u16)
                        }
                        _ => None,
                    }
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = uxn::host_short_to_uxn_short(bytes_read);
            }

            if targeted_device_field!(target, short_mode, file, i, write) {
                let bytes_written = || -> Option<u16> {
                    match &mut self.open_files[i] {
                        OpenedPath::File { path: _, handle } => {
                            let length = self.io_memory.file[i].get_operation_length();
                            let src = cpu.slice(
                                uxn::uxn_short_to_host_short(self.io_memory.file[i].write),
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
                self.io_memory.file[i].success = uxn::host_short_to_uxn_short(bytes_written);
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
                        uxn::uxn_short_to_host_short(self.io_memory.file[i].stat),
                        length,
                    )?;
                    dst.copy_from_slice(&entry);
                    Some(length)
                }()
                .unwrap_or(0);
                self.io_memory.file[i].success = uxn::host_short_to_uxn_short(bytes_written);
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

fn eval_with_fault_handling(vm: &mut uxn::Uxn, host: &mut UxnCli, entry_point: u16) {
    match vm.eval(host, entry_point) {
        uxn::UxnEvalResult::Ok => {}
        uxn::UxnEvalResult::Fault {
            where_the_error_occured,
            instruction_that_faulted,
            error_code,
        } => {
            let fault_handler = uxn::uxn_short_to_host_short(host.io_memory.system.halt);
            if fault_handler != 0 {
                // Empty the stacks
                vm.working_stack.head = 0;
                vm.return_stack.head = 0;
                vm.working_stack.push16(where_the_error_occured).unwrap();
                vm.working_stack.push8(instruction_that_faulted).unwrap();
                vm.working_stack.push8(error_code.clone() as u8).unwrap();
                match vm.eval(host, fault_handler) {
                    uxn::UxnEvalResult::Fault {
                        where_the_error_occured,
                        instruction_that_faulted,
                        error_code,
                    } => {
                        panic!(
                            "Failed with {:?} ({:02x}) at {:04x} ({:02x}) during fault handling",
                            error_code.clone(),
                            error_code.clone() as u8,
                            where_the_error_occured,
                            instruction_that_faulted
                        );
                    }
                    _ => {}
                }
            } else {
                panic!(
                    "Failed with {:?} ({:02x}) at {:04x} ({:02x})",
                    error_code.clone(),
                    error_code.clone() as u8,
                    where_the_error_occured,
                    instruction_that_faulted
                );
            }
        }
    }
}

fn inject_console_byte(vm: &mut uxn::Uxn, host: &mut UxnCli, byte: u8, kind: ConsoleType) {
    host.io_memory.console.console_type = kind;
    host.io_memory.console.read = byte;
    let entry = uxn::uxn_short_to_host_short(host.io_memory.console.vector);
    eval_with_fault_handling(vm, host, entry);
}

fn main() {
    let mut args = std::env::args();
    let mut rom = vec![];
    let mut file = std::fs::File::open(args.nth(1).unwrap()).unwrap();
    file.read_to_end(&mut rom).unwrap();

    let mut vm = uxn::Uxn::boot(&rom);
    let mut host = UxnCli::default();
    eval_with_fault_handling(&mut vm, &mut host, uxn::PAGE_PROGRAM as u16);

    // Process arguments
    let args_len = args.len();
    for (i, arg) in args.enumerate() {
        for c in arg.as_bytes() {
            inject_console_byte(&mut vm, &mut host, *c, ConsoleType::Argument);
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
    }

    // Process input
    while host.io_memory.system.state == 0 {
        let mut byte = [0];
        match std::io::stdin().read(&mut byte) {
            Ok(amount_read) => {
                if amount_read != 0 {
                    inject_console_byte(&mut vm, &mut host, byte[0], ConsoleType::StdIn);
                }
            }
            _ => break,
        }
    }
}

// TODO: Move to another file
#[cfg(test)]
mod sandbox_tests {
    use super::complies_with_sandbox_rules;

    fn with_tempdir<F>(function: F)
    where
        F: FnOnce() -> () + std::panic::UnwindSafe,
    {
        let test_result;

        {
            static CURRENT_DIR_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());
            let _currect_dir_guard = CURRENT_DIR_MUTEX.lock().unwrap();
            let current_dir_backup = std::env::current_dir().unwrap();

            let test_directory = std::env::temp_dir().join("uxncli-sandbox-tests");
            assert!(!test_directory.exists());
            std::fs::create_dir(&test_directory).unwrap();

            let working_directory = test_directory.as_path().join("working_dir");
            assert!(!working_directory.exists());
            std::fs::create_dir(&working_directory).unwrap();

            std::env::set_current_dir(&working_directory).unwrap();
            test_result = std::panic::catch_unwind(|| function());
            std::env::set_current_dir(current_dir_backup).unwrap();
            std::fs::remove_dir_all(&test_directory).unwrap();
        }

        assert!(test_result.is_ok());
    }

    #[test]
    fn test_file_in_current_working_directory() {
        with_tempdir(|| {
            let file_name = "test.txt";
            std::fs::File::create(file_name).unwrap();
            assert!(complies_with_sandbox_rules(std::path::Path::new(file_name)));
        });
    }

    #[test]
    fn test_file_outside_current_working_directory() {
        with_tempdir(|| {
            let file_name = std::path::PathBuf::from("../test.txt");
            std::fs::File::create(&file_name).unwrap();
            assert!(!complies_with_sandbox_rules(&file_name));
        });
    }

    #[test]
    fn test_file_in_subdirectory() {
        with_tempdir(|| {
            let dir_path = std::path::PathBuf::from("dir");
            std::fs::create_dir(&dir_path).unwrap();
            let file_path = dir_path.join("file.txt");
            std::fs::File::create(file_path.as_path()).unwrap();
            assert!(complies_with_sandbox_rules(file_path.as_path()));
        });
    }

    #[cfg(target_family = "unix")]
    #[test]
    fn test_symlink_that_points_to_something_in_the_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("file1.txt");
            std::fs::File::create(file1_path.as_path()).unwrap();
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }

    #[cfg(target_family = "unix")]
    #[test]
    fn test_symlink_that_points_to_something_outside_the_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("../file1.txt");
            std::fs::File::create(file1_path.as_path()).unwrap();
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }

    #[test]
    fn test_file_that_doesnt_exist_in_current_working_directory() {
        with_tempdir(|| {
            let file_path = std::path::PathBuf::from("file1.txt");
            assert!(!file_path.exists());
            assert!(complies_with_sandbox_rules(file_path.as_path()));
        });
    }

    #[test]
    fn test_file_that_doesnt_exist_outside_of_current_working_directory() {
        with_tempdir(|| {
            let file1_path = std::path::PathBuf::from("../file1.txt");
            assert!(!file1_path.exists());
            let file2_path = std::path::PathBuf::from("file2.txt");
            std::os::unix::fs::symlink(file1_path.as_path(), file2_path.as_path()).unwrap();
            assert!(!complies_with_sandbox_rules(file2_path.as_path()));
        });
    }
}
