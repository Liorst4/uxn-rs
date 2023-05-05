#[derive(Debug)]
pub enum UxnError {
    StackUnderflow,
    StackOverflow,
    MathError,
    InvalidAddress,
    IOError,
    Unsupported,
}

pub const PAGE_PROGRAM: usize = 0x0100;
const STACK_BYTE_COUNT: usize = 255;
const RAM_BYTE_COUNT: usize = 64 * 1024;
pub const IO_BYTE_COUNT: usize = 256;

#[repr(u8)]
#[derive(PartialEq, Debug)]
enum OpCode {
    BRK = 0x00,
    INC = 0x01,
    POP = 0x02,
    NIP = 0x03,
    SWP = 0x04,
    ROT = 0x05,
    DUP = 0x06,
    OVR = 0x07,
    EQU = 0x08,
    NEQ = 0x09,
    GTH = 0x0A,
    LTH = 0x0B,
    JMP = 0x0C,
    JCN = 0x0D,
    JSR = 0x0E,
    STH = 0x0F,
    LDZ = 0x10,
    STZ = 0x11,
    LDR = 0x12,
    STR = 0x13,
    LDA = 0x14,
    STA = 0x15,
    DEI = 0x16,
    DEO = 0x17,
    ADD = 0x18,
    SUB = 0x19,
    MUL = 0x1A,
    DIV = 0x1B,
    AND = 0x1C,
    ORA = 0x1D,
    EOR = 0x1E,
    SFT = 0x1F,

    // BRK derivatives
    JCI = 0x20,
    JMI = 0x40,
    JSI = 0x60,
    LIT = 0x80,
}

#[derive(Debug)]
struct Instruction {
    keep_mode: bool,
    short_mode: bool,
    return_mode: bool,
    operation: OpCode,
}

impl From<u8> for Instruction {
    fn from(byte: u8) -> Self {
        const KEEP_MODE_MASK: u8 = 0b10000000;
        const SHORT_MODE_MASK: u8 = 0b00100000;
        const RETURN_MODE_MASK: u8 = 0b01000000;
        const OPCODE_MASK: u8 = !(KEEP_MODE_MASK | SHORT_MODE_MASK | RETURN_MODE_MASK);

        let keep_mode = (byte & KEEP_MODE_MASK) != 0;
        let short_mode = (byte & SHORT_MODE_MASK) != 0;
        let return_mode = (byte & RETURN_MODE_MASK) != 0;

        // TODO: Without unsafe
        let mut operation: OpCode = unsafe { std::mem::transmute(byte & OPCODE_MASK) };
        if operation == OpCode::BRK {
            if keep_mode {
                operation = OpCode::LIT;
            } else {
                // BRK JCI JMI JSI
                operation = unsafe { std::mem::transmute(byte) }
            }
        }

        return Instruction {
            keep_mode,
            short_mode,
            return_mode,
            operation,
        };
    }
}

pub struct Stack {
    head: u8,
    data: [u8; STACK_BYTE_COUNT],
}

pub fn bytes_to_short(bytes: [u8; 2]) -> u16 {
    u16::from_le_bytes(bytes)
}

pub fn short_to_bytes(short: u16) -> [u8; 2] {
    u16::to_le_bytes(short)
}

pub fn short_from_host_byte_order(short: u16) -> u16 {
    u16::to_le(short)
}

pub fn short_to_host_byte_order(short: u16) -> u16 {
    u16::from_le(short)
}

impl Stack {
    fn push8(&mut self, value: u8) -> Result<(), UxnError> {
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

    fn push16(&mut self, value: u16) -> Result<(), UxnError> {
        if self.head >= (STACK_BYTE_COUNT as u8) - 2 {
            return Err(UxnError::StackOverflow);
        }
        let value = short_to_bytes(value);
        self.data[self.head as usize] = value[0];
        self.data[(self.head + 1) as usize] = value[1];
        self.head += 2;
        return Ok(());
    }
    fn pop16(&mut self) -> Result<u16, UxnError> {
        if self.head < 2 {
            return Err(UxnError::StackUnderflow);
        }
        let result = bytes_to_short([
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

pub struct Uxn {
    ram: [u8; RAM_BYTE_COUNT],
    working_stack: Stack,
    return_stack: Stack,
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
    fn read8(&self, address: u16) -> Option<u8> {
        self.ram.get(address as usize).map(|x| *x)
    }

    fn write8(&mut self, address: u16, value: u8) -> Option<()> {
        *self.ram.get_mut(address as usize)? = value;
        Some(())
    }

    fn read16(&self, address: u16) -> Option<u16> {
        let address = address as usize;

        if address + 1 >= self.ram.len() {
            return None;
        }

        return Some(bytes_to_short([self.ram[address], self.ram[address + 1]]));
    }

    fn write16(&mut self, address: u16, value: u16) -> Option<()> {
        let address = address as usize;
        let value = short_to_bytes(value);

        if address + 1 >= self.ram.len() {
            return None;
        }

        self.ram[address] = value[0];
        self.ram[address + 1] = value[1];

        return Some(());
    }

    fn read(&self, address: u16, short_mode: bool) -> Option<u16> {
        if short_mode {
            self.read16(address)
        } else {
            self.read8(address).map(|x| x as u16)
        }
    }

    fn write(&mut self, address: u16, value: u16, short_mode: bool) -> Option<()> {
        if short_mode {
            self.write16(address, value)
        } else {
            self.write8(address, value as u8)
        }
    }

    fn step(&mut self, host: &mut dyn Host, program_counter: u16) -> Result<StepResult, UxnError> {
        let instruction = Instruction::from(
            self.read8(program_counter)
                .ok_or(UxnError::InvalidAddress)?,
        );

        macro_rules! stack_to_use {
            () => {
                if instruction.return_mode {
                    &mut self.return_stack
                } else {
                    &mut self.working_stack
                }
            };
        }
        macro_rules! pop {
            () => {
                stack_to_use!().pop(instruction.short_mode)
            };
        }
        macro_rules! push {
            ($value:expr) => {
                stack_to_use!().push($value, instruction.short_mode)
            };
        }
        macro_rules! write {
            ($address:expr, $value:expr) => {
                self.write($address, $value, instruction.short_mode)
                    .ok_or(UxnError::InvalidAddress)
            };
        }
        macro_rules! read {
            ($address:expr) => {
                self.read($address, instruction.short_mode)
                    .ok_or(UxnError::InvalidAddress)
            };
        }
        macro_rules! dei {
            ($target:expr) => {
                host.dei(self, $target, instruction.short_mode)
                    .ok_or(UxnError::IOError)
            };
        }
        macro_rules! deo {
            ($target:expr, $value:expr) => {
                host.deo(self, $target, $value, instruction.short_mode)
                    .ok_or(UxnError::IOError)
            };
        }
        macro_rules! jmp {
            ($address: expr) => {
                if instruction.short_mode {
                    return Ok(StepResult::ProgramCounter($address));
                }
                return Ok(StepResult::ProgramCounter(program_counter + $address));
            };
        }

        let head_backup = stack_to_use!().head;
        // TODO: apply when something fails
        /// Undo pop side effects if we are in keep_mode
        macro_rules! done_taking_args {
            () => {
                if instruction.keep_mode {
                    stack_to_use!().head = head_backup;
                }
            };
        }

        match instruction.operation {
            OpCode::BRK => {
                return Ok(StepResult::Break);
            }
            OpCode::INC => {
                let a = pop!()?;
                done_taking_args!();
                push!(a + 1)?;
            }
            OpCode::POP => {
                pop!()?;
                done_taking_args!();
            }
            OpCode::NIP => {
                let b = pop!()?;
                pop!()?; // a
                done_taking_args!();
                push!(b)?;
            }
            OpCode::SWP => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(b)?;
                push!(a)?;
            }
            OpCode::ROT => {
                let c = pop!()?;
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(b)?;
                push!(c)?;
                push!(a)?;
            }
            OpCode::DUP => {
                let a = pop!()?;
                done_taking_args!();
                push!(a)?;
                push!(a)?;
            }
            OpCode::OVR => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a)?;
                push!(b)?;
                push!(a)?;
            }
            OpCode::EQU => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                stack_to_use!().push8((a == b) as u8)?;
            }
            OpCode::NEQ => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                stack_to_use!().push8((a != b) as u8)?;
            }
            OpCode::GTH => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                stack_to_use!().push8((a > b) as u8)?;
            }
            OpCode::LTH => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                stack_to_use!().push8((a < b) as u8)?;
            }
            OpCode::JMP => {
                let address = pop!()?;
                done_taking_args!();
                jmp!(address);
            }
            OpCode::JCN => {
                let cond8 = stack_to_use!().pop8()?;
                let address = pop!()?;
                done_taking_args!();
                if cond8 != 0 {
                    jmp!(address);
                }
            }
            OpCode::JSR => {
                let address = pop!()?;
                done_taking_args!();
                self.return_stack.push16(program_counter)?;
                jmp!(address);
            }
            OpCode::STH => {
                let a = pop!()?;
                done_taking_args!();
                self.return_stack.push(a, instruction.short_mode)?;
            }
            OpCode::LDZ => {
                let addr8 = stack_to_use!().pop8()?;
                done_taking_args!();
                let value = read!(addr8 as u16)?;
                push!(value)?;
            }
            OpCode::STZ => {
                let addr8 = stack_to_use!().pop8()?;
                done_taking_args!();
                let val = pop!()?;
                write!(addr8 as u16, val)?;
            }
            OpCode::LDR => {
                let distance = stack_to_use!().pop8()?;
                done_taking_args!();
                let addr = program_counter + (distance as u16);
                let value = read!(addr)?;
                push!(value)?;
            }
            OpCode::STR => {
                let distance = stack_to_use!().pop8()?;
                let addr = program_counter + (distance as u16);
                let val = pop!()?;
                done_taking_args!();
                write!(addr, val)?;
            }
            OpCode::LDA => {
                let addr = stack_to_use!().pop16()?;
                done_taking_args!();
                let value = read!(addr)?;
                push!(value)?;
            }
            OpCode::STA => {
                let addr = stack_to_use!().pop16()?;
                let val = pop!()?;
                done_taking_args!();
                write!(addr, val)?;
            }
            OpCode::DEI => {
                let target = stack_to_use!().pop8()?;
                done_taking_args!();
                let value = dei!(target)?;
                push!(value)?;
            }
            OpCode::DEO => {
                let target = stack_to_use!().pop8()?;
                let val = pop!()?;
                done_taking_args!();
                deo!(target, val)?;
            }
            OpCode::ADD => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a.overflowing_add(b).0)?;
            }
            OpCode::SUB => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a.overflowing_sub(b).0)?;
            }
            OpCode::MUL => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a.overflowing_mul(b).0)?;
            }
            OpCode::DIV => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                if b == 0 {
                    return Err(UxnError::MathError);
                }
                push!(a.overflowing_div(b).0)?;
            }
            OpCode::AND => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a & b)?;
            }
            OpCode::ORA => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a | b)?;
            }
            OpCode::EOR => {
                let b = pop!()?;
                let a = pop!()?;
                done_taking_args!();
                push!(a ^ b)?;
            }
            OpCode::SFT => {
                let shift8 = stack_to_use!().pop8()?;
                let a = pop!()?;
                done_taking_args!();

                let high_nibble = (shift8 & 0xf0) >> 4;
                let low_nibble = shift8 & 0x0f;

                let result = (a >> low_nibble) << high_nibble;
                push!(result)?;
            }
            OpCode::JCI => {
                // TODO
                return Err(UxnError::Unsupported);
            }
            OpCode::JMI => {
                // TODO
                return Err(UxnError::Unsupported);
            }
            OpCode::JSI => {
                // TODO
                return Err(UxnError::Unsupported);
            }
            OpCode::LIT => {
                let literal = read!(program_counter + 1)?;
                push!(literal)?;
                if instruction.short_mode {
                    return Ok(StepResult::ProgramCounter(program_counter + 3));
                }
                return Ok(StepResult::ProgramCounter(program_counter + 2));
            }
        }

        return Ok(StepResult::ProgramCounter(program_counter + 1));
    }

    pub fn eval(&mut self, mm: &mut dyn Host, program_counter: u16) -> Result<(), UxnError> {
        let mut program_counter = program_counter;
        loop {
            match self.step(mm, program_counter)? {
                StepResult::ProgramCounter(next) => {
                    program_counter = next;
                }
                StepResult::Break => {
                    return Ok(());
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
