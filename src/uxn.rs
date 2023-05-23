#[derive(Debug, Clone)]
#[repr(u8)]
pub enum UxnError {
    StackUnderflow = 1,
    StackOverflow = 2,
    MathError = 3,

    // TODO: Remove these? Looks like uxn.c doesn't have it
    InvalidAddress,
    IOError,
}

pub enum UxnEvalResult {
    Ok,
    Fault {
        where_the_error_occured: u16,
        instruction_that_faulted: u8, /* TODO: Change to `Instruction` */
        error_code: UxnError,
    },
}

/// The address to start executing code from
pub const PAGE_PROGRAM: usize = 0x0100;
const STACK_BYTE_COUNT: usize = 255;
const RAM_BYTE_COUNT: usize = 64 * 1024;
pub const IO_BYTE_COUNT: usize = 256;

// TODO: Use numbered enum (without repeating the numbers in `From<u8>::from`
/// Instruction without different modes
enum SimpleOperation {
    Break,                  // BRK
    JumpConditionalInstant, // JCI
    JumpInstant,            // JMI
    JumpStashReturnIsntant, // JSI
}

struct ComplexOperationFlags {
    keep_mode: bool,
    short_mode: bool,
    return_mode: bool,
}

// TODO: Use numbered enum (without repeating the numbers in `From<u8>::from`
/// Instructions that support all the modes
enum ComplexOperation {
    Increase,        // INC
    Pop,             // POP
    Nip,             // NIP
    Swap,            // SWP
    Rotate,          // ROT
    Duplicate,       // DUP
    Over,            // OVR
    Equal,           // EQU
    NotEqual,        // NEQ
    GreaterThan,     // GTH
    LesserThan,      // LTH
    Jump,            // JMP
    JumpConditional, // JCN
    JumpStashReturn, // JSR
    Stash,           // STH
    LoadZeroPage,    // LDZ
    StoreZeroPage,   // STZ
    LoadRelative,    // LDR
    StoreRelative,   // STR
    LoadAbsolute,    // LDA
    StoreAbsolute,   // STA
    DeviceInput,     // DEI
    DeviceOutput,    // DEO
    Add,             // ADD
    Subtract,        // SUB
    Multiply,        // MUL
    Divide,          // DIV
    And,             // AND
    Or,              // ORA
    ExclusiveOr,     // EOR
    Shift,           // SFT
}

// TODO: Represent as a single byte in memory
enum Instruction {
    Simple(SimpleOperation),
    Literal { short_mode: bool, return_mode: bool }, // LIT (supports only 2 modes)
    Complex(ComplexOperation, ComplexOperationFlags),
}

impl From<u8> for Instruction {
    fn from(byte: u8) -> Self {
        return match byte {
            0x00 => Instruction::Simple(SimpleOperation::Break),
            0x20 => Instruction::Simple(SimpleOperation::JumpConditionalInstant),
            0x40 => Instruction::Simple(SimpleOperation::JumpInstant),
            0x60 => Instruction::Simple(SimpleOperation::JumpStashReturnIsntant),
            _ => {
                const KEEP_MODE_MASK: u8 = 0b10000000;
                const SHORT_MODE_MASK: u8 = 0b00100000;
                const RETURN_MODE_MASK: u8 = 0b01000000;
                const OPCODE_MASK: u8 = !(KEEP_MODE_MASK | SHORT_MODE_MASK | RETURN_MODE_MASK);

                let short_mode = (byte & SHORT_MODE_MASK) != 0;
                let return_mode = (byte & RETURN_MODE_MASK) != 0;
                let opcode = byte & OPCODE_MASK;

                match opcode {
                    0x00 => Instruction::Literal {
                        short_mode,
                        return_mode,
                    },
                    complex_opcode => {
                        let keep_mode = (byte & KEEP_MODE_MASK) != 0;
                        Instruction::Complex(
                            match complex_opcode {
                                0x01 => ComplexOperation::Increase,
                                0x02 => ComplexOperation::Pop,
                                0x03 => ComplexOperation::Nip,
                                0x04 => ComplexOperation::Swap,
                                0x05 => ComplexOperation::Rotate,
                                0x06 => ComplexOperation::Duplicate,
                                0x07 => ComplexOperation::Over,
                                0x08 => ComplexOperation::Equal,
                                0x09 => ComplexOperation::NotEqual,
                                0x0A => ComplexOperation::GreaterThan,
                                0x0B => ComplexOperation::LesserThan,
                                0x0C => ComplexOperation::Jump,
                                0x0D => ComplexOperation::JumpConditional,
                                0x0E => ComplexOperation::JumpStashReturn,
                                0x0F => ComplexOperation::Stash,
                                0x10 => ComplexOperation::LoadZeroPage,
                                0x11 => ComplexOperation::StoreZeroPage,
                                0x12 => ComplexOperation::LoadRelative,
                                0x13 => ComplexOperation::StoreRelative,
                                0x14 => ComplexOperation::LoadAbsolute,
                                0x15 => ComplexOperation::StoreAbsolute,
                                0x16 => ComplexOperation::DeviceInput,
                                0x17 => ComplexOperation::DeviceOutput,
                                0x18 => ComplexOperation::Add,
                                0x19 => ComplexOperation::Subtract,
                                0x1A => ComplexOperation::Multiply,
                                0x1B => ComplexOperation::Divide,
                                0x1C => ComplexOperation::And,
                                0x1D => ComplexOperation::Or,
                                0x1E => ComplexOperation::ExclusiveOr,
                                0x1F => ComplexOperation::Shift,
                                _ => unreachable!(),
                            },
                            ComplexOperationFlags {
                                keep_mode,
                                short_mode,
                                return_mode,
                            },
                        )
                    }
                }
            }
        };
    }
}

pub struct Stack {
    pub head: u8,
    pub data: [u8; STACK_BYTE_COUNT],
}

pub fn uxn_bytes_to_host_short(bytes: [u8; 2]) -> u16 {
    u16::from_be_bytes(bytes)
}

pub fn host_short_to_uxn_bytes(short: u16) -> [u8; 2] {
    u16::to_be_bytes(short)
}

pub fn host_short_to_uxn_short(short: u16) -> u16 {
    u16::to_be(short)
}

pub fn uxn_short_to_host_short(short: u16) -> u16 {
    u16::from_be(short)
}

impl Stack {
    pub fn push8(&mut self, value: u8) -> Result<(), UxnError> {
        if self.head == STACK_BYTE_COUNT as u8 {
            return Err(UxnError::StackOverflow);
        }

        self.data[self.head as usize] = value;
        self.head += 1;
        return Ok(());
    }
    fn pop8(&mut self) -> Result<u8, UxnError> {
        if self.head == 0 {
            return Err(UxnError::StackUnderflow);
        }
        let result = self.data[(self.head - 1) as usize];
        self.head -= 1;
        return Ok(result);
    }

    pub fn push16(&mut self, value: u16) -> Result<(), UxnError> {
        if self.head >= (STACK_BYTE_COUNT as u8) - 2 {
            return Err(UxnError::StackOverflow);
        }
        let value = host_short_to_uxn_bytes(value);
        self.data[self.head as usize] = value[0];
        self.data[(self.head + 1) as usize] = value[1];
        self.head += 2;
        return Ok(());
    }
    fn pop16(&mut self) -> Result<u16, UxnError> {
        if self.head < 2 {
            return Err(UxnError::StackUnderflow);
        }
        let result = uxn_bytes_to_host_short([
            self.data[(self.head - 2) as usize],
            self.data[(self.head - 1) as usize],
        ]);
        self.head -= 2;
        return Ok(result);
    }

    fn push(&mut self, data: u16, short_mode: bool) -> Result<(), UxnError> {
        if short_mode {
            return self.push16(data);
        }
        return self.push8(data as u8);
    }
    fn pop(&mut self, short_mode: bool) -> Result<u16, UxnError> {
        if short_mode {
            return self.pop16();
        }

        return self.pop8().map(|x| x as u16);
    }
}

impl Default for Stack {
    fn default() -> Self {
        Stack {
            head: 0,
            data: [0; STACK_BYTE_COUNT],
        }
    }
}

impl core::fmt::Display for Stack {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
        let head = self.head as usize;
        let data = &self.data[0..head];
        write!(f, "{:x?}", data)
    }
}

pub struct Uxn {
    ram: [u8; RAM_BYTE_COUNT],
    pub working_stack: Stack,
    pub return_stack: Stack,
}

/// Interface for interacting with the hosting environment
pub trait Host {
    /// Device input. Read data from a device
    ///
    /// * cpu - State of the machine
    /// * target - device memory offset
    /// * short mode - 16 bit or 8 bit
    fn dei(&mut self, cpu: &mut Uxn, target: u8, short_mode: bool) -> Option<u16>;

    /// Device output. Write data to a device
    ///
    /// * cpu - State of the machine
    /// * target - device memory offset
    /// * value - data to write
    /// * short mode - 16 bit or 8 bit
    fn deo(&mut self, cpu: &mut Uxn, target: u8, value: u16, short_mode: bool) -> Option<()>;
}

enum StepResult {
    ProgramCounter(u16),
    Break,
}

impl Uxn {
    pub fn read8(&self, address: u16) -> Option<u8> {
        self.ram.get(address as usize).map(|x| *x)
    }

    pub fn write8(&mut self, address: u16, value: u8) -> Option<()> {
        *self.ram.get_mut(address as usize)? = value;
        Some(())
    }

    pub fn read16(&self, address: u16) -> Option<u16> {
        let address = address as usize;

        if address + 1 >= self.ram.len() {
            return None;
        }

        return Some(uxn_bytes_to_host_short([
            self.ram[address],
            self.ram[address + 1],
        ]));
    }

    pub fn write16(&mut self, address: u16, value: u16) -> Option<()> {
        let address = address as usize;
        let value = host_short_to_uxn_bytes(value);

        if address + 1 >= self.ram.len() {
            return None;
        }

        self.ram[address] = value[0];
        self.ram[address + 1] = value[1];

        return Some(());
    }

    pub fn slice(&self, begin: u16, length: u16) -> Option<&[u8]> {
        if length == 0 {
            return None;
        }

        return self
            .ram
            .as_slice()
            .get(begin as usize..(begin + length) as usize);
    }

    pub fn slice_mut(&mut self, begin: u16, length: u16) -> Option<&mut [u8]> {
        if length == 0 {
            return None;
        }

        return self
            .ram
            .as_mut_slice()
            .get_mut(begin as usize..(begin + length) as usize);
    }

    pub fn read(&self, address: u16, short_mode: bool) -> Option<u16> {
        if short_mode {
            self.read16(address)
        } else {
            self.read8(address).map(|x| x as u16)
        }
    }

    pub fn write(&mut self, address: u16, value: u16, short_mode: bool) -> Option<()> {
        if short_mode {
            self.write16(address, value)
        } else {
            self.write8(address, value as u8)
        }
    }

    fn step(&mut self, host: &mut dyn Host, program_counter: u16) -> Result<StepResult, UxnError> {
        let short_mode: bool;
        let return_mode: bool;

        let instruction = Instruction::from(
            self.read8(program_counter)
                .ok_or(UxnError::InvalidAddress)?,
        );

        macro_rules! stack_to_use {
            () => {
                if return_mode {
                    &mut self.return_stack
                } else {
                    &mut self.working_stack
                }
            };
        }
        macro_rules! push {
            ($value:expr) => {
                stack_to_use!().push($value, short_mode)
            };
        }
        macro_rules! read {
            ($address:expr) => {
                self.read($address, short_mode)
                    .ok_or(UxnError::InvalidAddress)
            };
        }

        match instruction {
            Instruction::Simple(opcode) => {
                macro_rules! jmp_to_literal {
                    () => {
                        let distance = self
                            .read16(program_counter + 1)
                            .ok_or(UxnError::InvalidAddress)?;
                        let destination = (program_counter + 3).overflowing_add(distance).0;
                        return Ok(StepResult::ProgramCounter(destination));
                    };
                }

                match opcode {
                    SimpleOperation::Break => {
                        return Ok(StepResult::Break);
                    }
                    SimpleOperation::JumpConditionalInstant => {
                        let cond8 = self.working_stack.pop8()?;
                        if cond8 == 0 {
                            return Ok(StepResult::ProgramCounter(program_counter + 3));
                        }
                        jmp_to_literal!();
                    }
                    SimpleOperation::JumpInstant => {
                        jmp_to_literal!();
                    }
                    SimpleOperation::JumpStashReturnIsntant => {
                        self.return_stack.push16(program_counter + 3)?;
                        jmp_to_literal!();
                    }
                }
            }
            Instruction::Literal {
                short_mode: short_mode_,
                return_mode: return_mode_,
            } => {
                short_mode = short_mode_;
                return_mode = return_mode_;
                let literal = read!(program_counter + 1)?;
                push!(literal)?;
                if short_mode {
                    return Ok(StepResult::ProgramCounter(program_counter + 3));
                }
                return Ok(StepResult::ProgramCounter(program_counter + 2));
            }
            Instruction::Complex(opcode, flags) => {
                short_mode = flags.short_mode;
                return_mode = flags.return_mode;
                let head_backup = stack_to_use!().head;

                // TODO: apply when something fails
                /// Undo pop side effects if we are in keep_mode
                macro_rules! done_taking_args {
                    () => {
                        if flags.keep_mode {
                            stack_to_use!().head = head_backup;
                        }
                    };
                }

                macro_rules! pop {
                    () => {
                        stack_to_use!().pop(short_mode)
                    };
                }

                macro_rules! write {
                    ($address:expr, $value:expr) => {
                        self.write($address, $value, short_mode)
                            .ok_or(UxnError::InvalidAddress)
                    };
                }

                macro_rules! dei {
                    ($target:expr) => {
                        host.dei(self, $target, short_mode).ok_or(UxnError::IOError)
                    };
                }
                macro_rules! deo {
                    ($target:expr, $value:expr) => {
                        host.deo(self, $target, $value, short_mode)
                            .ok_or(UxnError::IOError)
                    };
                }

                macro_rules! jmp {
                    ($address: expr) => {
                        if short_mode {
                            return Ok(StepResult::ProgramCounter($address));
                        }

                        let diff: u8 = $address as u8;
                        let diff: i8 = diff as i8;
                        let diff: i16 = diff as i16;
                        let destination = program_counter
                            .overflowing_add(1)
                            .0
                            .overflowing_add_signed(diff as i16)
                            .0;

                        return Ok(StepResult::ProgramCounter(destination));
                    };
                }

                match opcode {
                    ComplexOperation::Increase => {
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a.overflowing_add(1).0)?;
                    }
                    ComplexOperation::Pop => {
                        pop!()?;
                        done_taking_args!();
                    }
                    ComplexOperation::Nip => {
                        let b = pop!()?;
                        pop!()?; // a
                        done_taking_args!();
                        push!(b)?;
                    }
                    ComplexOperation::Swap => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(b)?;
                        push!(a)?;
                    }
                    ComplexOperation::Rotate => {
                        let c = pop!()?;
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(b)?;
                        push!(c)?;
                        push!(a)?;
                    }
                    ComplexOperation::Duplicate => {
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a)?;
                        push!(a)?;
                    }
                    ComplexOperation::Over => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a)?;
                        push!(b)?;
                        push!(a)?;
                    }
                    ComplexOperation::Equal => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        stack_to_use!().push8((a == b) as u8)?;
                    }
                    ComplexOperation::NotEqual => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        stack_to_use!().push8((a != b) as u8)?;
                    }
                    ComplexOperation::GreaterThan => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        stack_to_use!().push8((a > b) as u8)?;
                    }
                    ComplexOperation::LesserThan => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        stack_to_use!().push8((a < b) as u8)?;
                    }
                    ComplexOperation::Jump => {
                        let address = pop!()?;
                        done_taking_args!();
                        jmp!(address);
                    }
                    ComplexOperation::JumpConditional => {
                        let address = pop!()?;
                        let cond8 = stack_to_use!().pop8()?;
                        done_taking_args!();
                        if cond8 != 0 {
                            jmp!(address);
                        }
                    }
                    ComplexOperation::JumpStashReturn => {
                        let address = pop!()?;
                        done_taking_args!();
                        self.return_stack.push16(program_counter + 1)?;
                        jmp!(address);
                    }
                    ComplexOperation::Stash => {
                        let a = pop!()?;
                        done_taking_args!();
                        if return_mode {
                            self.working_stack.push(a, short_mode)?;
                        } else {
                            self.return_stack.push(a, short_mode)?;
                        }
                    }
                    ComplexOperation::LoadZeroPage => {
                        let addr8 = stack_to_use!().pop8()?;
                        done_taking_args!();
                        let value = read!(addr8 as u16)?;
                        push!(value)?;
                    }
                    ComplexOperation::StoreZeroPage => {
                        let addr8 = stack_to_use!().pop8()?;
                        done_taking_args!();
                        let val = pop!()?;
                        write!(addr8 as u16, val)?;
                    }
                    ComplexOperation::LoadRelative => {
                        let distance = stack_to_use!().pop8()?;
                        done_taking_args!();
                        let addr = program_counter - (PAGE_PROGRAM as u16) + (distance as u16);
                        let value = read!(addr)?;
                        push!(value)?;
                    }
                    ComplexOperation::StoreRelative => {
                        let distance = stack_to_use!().pop8()?;
                        let addr = program_counter - (PAGE_PROGRAM as u16) + (distance as u16);
                        let val = pop!()?;
                        done_taking_args!();
                        write!(addr, val)?;
                    }
                    ComplexOperation::LoadAbsolute => {
                        let addr = stack_to_use!().pop16()?;
                        done_taking_args!();
                        let value = read!(addr)?;
                        push!(value)?;
                    }
                    ComplexOperation::StoreAbsolute => {
                        let addr = stack_to_use!().pop16()?;
                        let val = pop!()?;
                        done_taking_args!();
                        write!(addr, val)?;
                    }
                    ComplexOperation::DeviceInput => {
                        let target = stack_to_use!().pop8()?;
                        done_taking_args!();
                        let value = dei!(target)?;
                        push!(value)?;
                    }
                    ComplexOperation::DeviceOutput => {
                        let target = stack_to_use!().pop8()?;
                        let val = pop!()?;
                        done_taking_args!();
                        deo!(target, val)?;
                    }
                    ComplexOperation::Add => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a.overflowing_add(b).0)?;
                    }
                    ComplexOperation::Subtract => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a.overflowing_sub(b).0)?;
                    }
                    ComplexOperation::Multiply => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a.overflowing_mul(b).0)?;
                    }
                    ComplexOperation::Divide => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        if b == 0 {
                            return Err(UxnError::MathError);
                        }
                        push!(a.overflowing_div(b).0)?;
                    }
                    ComplexOperation::And => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a & b)?;
                    }
                    ComplexOperation::Or => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a | b)?;
                    }
                    ComplexOperation::ExclusiveOr => {
                        let b = pop!()?;
                        let a = pop!()?;
                        done_taking_args!();
                        push!(a ^ b)?;
                    }
                    ComplexOperation::Shift => {
                        let shift8 = stack_to_use!().pop8()?;
                        let a = pop!()?;
                        done_taking_args!();

                        let high_nibble = (shift8 & 0xf0) >> 4;
                        let low_nibble = shift8 & 0x0f;

                        let result = (a >> low_nibble) << high_nibble;
                        push!(result)?;
                    }
                }
            }
        }

        return Ok(StepResult::ProgramCounter(program_counter + 1));
    }

    pub fn eval(&mut self, host: &mut dyn Host, program_counter: u16) -> UxnEvalResult {
        if program_counter == 0 {
            return UxnEvalResult::Ok;
        }

        let mut program_counter = program_counter;
        loop {
            let instruction_byte_backup = self.read8(program_counter).unwrap();
            match self.step(host, program_counter) {
                Ok(StepResult::ProgramCounter(next)) => {
                    program_counter = next;
                }
                Ok(StepResult::Break) => {
                    return UxnEvalResult::Ok;
                }
                Err(code) => {
                    return UxnEvalResult::Fault {
                        where_the_error_occured: program_counter,
                        instruction_that_faulted: instruction_byte_backup,
                        error_code: code,
                    };
                }
            }
        }
    }

    pub fn boot(rom: &[u8]) -> Uxn {
        let mut ram = [0; RAM_BYTE_COUNT];
        ram[PAGE_PROGRAM..PAGE_PROGRAM + rom.len()].copy_from_slice(rom);
        Uxn {
            ram,
            working_stack: Default::default(),
            return_stack: Default::default(),
        }
    }
}

#[test]
fn test_opcodes() {
    struct MiniHost {}
    impl Host for MiniHost {
        fn dei(&mut self, _cpu: &mut Uxn, _target: u8, _short_mode: bool) -> Option<u16> {
            eprintln!("Tried to DEI");
            None
        }

        fn deo(&mut self, cpu: &mut Uxn, target: u8, value: u16, _short_mode: bool) -> Option<()> {
            const FAIL_CHAR: u8 = '0' as u8;
            const CONSOLE_WRITE_OFFSET: u8 = 0x18;
            const SYSTEM_DEBUG_OFFSET: u8 = 0x0e;
            const SYSTEM_STATE_OFFSET: u8 = 0x0f;

            if target == CONSOLE_WRITE_OFFSET {
                if value as u8 == FAIL_CHAR {
                    eprintln!("Reported a failure");
                    return None;
                }

                return Some(());
            }

            if target == SYSTEM_DEBUG_OFFSET {
                if value as u8 != 0x1 {
                    eprintln!("Unexpected value sent to System.debug {:02x}", value as u8);
                    return None;
                }

                if cpu.working_stack.head != 0 {
                    eprintln!("Working stack isn't empty {}", cpu.working_stack);
                    return None;
                }

                if cpu.return_stack.head != 0 {
                    eprintln!("Return stack isn't empty {}", cpu.return_stack);
                    return None;
                }

                return Some(());
            }

            if target == SYSTEM_STATE_OFFSET {
                if value as u8 != 0x1 {
                    eprintln!("Unexpected value sent to System.state{:02x}", value as u8);
                    return None;
                }
                return Some(());
            }

            eprintln!("Unsupported port for tests {:02x}", target);
            return None;
        }
    }

    const ENCODED_TEST_ROM: &str = include_str!("../etc/tests.rom.hex");
    let mut test_rom = vec![];

    for line in ENCODED_TEST_ROM.lines() {
        if line.starts_with('#') {
            continue;
        }

        for hex in line.split(' ') {
            test_rom.push(u8::from_str_radix(hex, 16).unwrap());
        }
    }

    let mut cpu = Uxn::boot(&test_rom);
    let mut host = MiniHost {};
    match cpu.eval(&mut host, PAGE_PROGRAM as u16) {
        UxnEvalResult::Ok => {}
        UxnEvalResult::Fault {
            where_the_error_occured,
            instruction_that_faulted,
            error_code,
        } => {
            panic!(
                "Opcode test failed with {:?} at {:04x} ({:02x})",
                error_code, where_the_error_occured, instruction_that_faulted
            );
        }
    }
}
