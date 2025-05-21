#[derive(Debug)]
struct NesEmulator {
    program_counter: u16,
    accumulator: u8,
    x: u8,
    y: u8,
    stack: u8,
    status: Status,
    bus: Bus
}

#[derive(Debug)]
struct Bus {
    ram: [u8; 0x800],
    ppu: [u8; 0x8],
    apu_and_io: [u8; 0x18],
    apu_and_io_normally_disabled: [u8; 0x8],
    cartridge: [u8; 0xbfe0]
}

impl Bus {
    fn new() -> Self {
        // Completely Fine since it just sets all buffers to zeroes at initialization
        unsafe { std::mem::zeroed() }
    }
    fn set_rom(mut self: Self, rom_data: &[u8]) -> Self {
        self.cartridge[0..rom_data.len()].copy_from_slice(rom_data);
        self
    }
    fn get_u16_at_address(self: &Self, address: u16) -> u16 {
        // Most Significant Byte (MSB) is at the n+1 address since the nes uses a little endian
        // architecture
        (self.get_u8_at_address(address) as u16) | ((self.get_u8_at_address(address + 1) as u16) << 8)
    }
    fn get_u8_at_address(self: &Self, address: u16) -> u8 {
        match address {
            0..=0x1FFF => {
                return self.ram[(address % 0x800) as usize]
            }, 0x2000..=0x3FFF => {
                // The 0x2000 % 8 is 0 and could be ommitted, i'm trusting the compiler on this
                // cause it makes more sense to put it like this and for consistency with other
                // memory indexes
                return self.ppu[((address - 0x2000) % 8) as usize]
            },
            0x4000..=0x4017 => {
                return self.apu_and_io[(address - 0x4000) as usize];
            }
            0x4018..=0x401F => {
                return self.apu_and_io_normally_disabled[(address - 0x4018) as usize];
            }
            0x4020..=0xFFFF => {
                // cartridges can have ram of their own, i'm assumming that making the distinction 
                // between the read only memory and the writeable parts is unnecessary for now. 
                return self.cartridge[(address - 0x4020) as usize]
            }
        }
    }
    fn write_u8_at_address(self: &mut Self, address: u16, byte: u8){
        match address {
            0..=0x1FFF => {
                self.ram[(address % 0x800) as usize] = byte;
            }, 0x2000..=0x3FFF => {
                self.ppu[((address - 0x2000) % 8) as usize] = byte;
            },
            0x4000..=0x4017 => {
                self.apu_and_io[(address - 0x4000) as usize] = byte;
            }
            0x4018..=0x401F => {
                self.apu_and_io_normally_disabled[(address - 0x4018) as usize] = byte; 
            }
            0x4020..=0xFFFF => {
                self.cartridge[(address - 0x4020) as usize] = byte
            }
        }
    }

}

impl NesEmulator {
    pub fn new(bus: Bus) -> NesEmulator {
        NesEmulator {
            program_counter: bus.get_u16_at_address(0xFFFC) ,
            accumulator: 0,
            x: 0,
            y: 0,
            stack: 0xFD,
            status: Status(0x34),
            bus,
        }
    }
    pub fn get_current_opcode_operand(self: &Self) -> (u8, u8) {
        let state = self;
        let opcode = state.bus.get_u8_at_address(state.program_counter);
        let (_, addressing, instruction_size, _) = OpCodeInfo[opcode as usize];
        let operand = match addressing {
            // Doesn't use an operand and works on registers directly instead
            AddressingMode::Implied => 0,
            // Just gets the byte next to the opcode
            AddressingMode::Immediate => state.bus.get_u8_at_address(1 + state.program_counter),
            // Just operates on the accumulator
            AddressingMode::Accumulator => state.accumulator,
            // Gets the byte on the zero'th page indexed by the first byte after the opcode
            AddressingMode::ZeroPage => {
                let address = state.bus.get_u8_at_address(1 + state.program_counter) as u16;
                state.bus.get_u8_at_address(address)
            },
            // Gets the byte on the zero'th page indexed by the first byte after the opcode and
            // with an offset of X
            AddressingMode::ZeroPageX => {
                // we use u8 address so it wraps around properly when added to X
                let address = state.bus.get_u8_at_address(1 + state.program_counter);
                state.bus.get_u8_at_address((address + state.x) as u16)
            },
            // same concept but using Y
            AddressingMode::ZeroPageY => {
                let address = state.bus.get_u8_at_address(1 + state.program_counter);
                state.bus.get_u8_at_address((address + state.y) as u16)
            },
            // Uses the next 2 bytes to the opcode as an absolute address
            AddressingMode::Absolute => {
                let address = state.bus.get_u16_at_address(1 + state.program_counter);
                state.bus.get_u8_at_address(address)
            },
            // same concept as absolute but adds the x register before indexing memory
            AddressingMode::AbsoluteX => {
                let address = state.bus.get_u16_at_address(1 + state.program_counter);
                state.bus.get_u8_at_address(address + state.x as u16)
            },
            // same concept as absolute but adds the y register before indexing memory
            AddressingMode::AbsoluteY => {
                let address = state.bus.get_u16_at_address(1 + state.program_counter);
                state.bus.get_u8_at_address(address + state.y as u16)
            },
            // Indirect memory access is like a double pointer dereference so its the value at
            // the address pointed to by the address just next to the opcode
            // basically address -> address -> operand
            // also it is using Zero page, and so the address is just one byte
            AddressingMode::Indirect => {
                //gets the address
                let mut address = state.bus.get_u8_at_address(1 + state.program_counter);
                //gets address at address 
                address = state.bus.get_u8_at_address(address as u16);
                // returns value
                state.bus.get_u8_at_address(address as u16)
            },
            // Indirect access with X offset at the first step of indirection
            AddressingMode::IndirectX => {
                //gets the address. We use an u8 address because the address should wrap around
                //in case of an overflow (without adding anything to the high bits) (this keeps
                //us in the zero page)
                let mut address: u8 = state.bus.get_u8_at_address(1 + state.program_counter);
                //adds offset
                address = address.wrapping_add(state.x);
                //gets address at address 
                address = state.bus.get_u8_at_address(address as u16);
                // returns value
                state.bus.get_u8_at_address(address as u16)
            },
            // Indirect access with Y offset at the first step of indirection. Y indirection
            // adds as u16 unlike X indirect access.
            AddressingMode::IndirectY => {
                //gets the address as u16 so that addition with Y can work like using the carry
                //bit for addition
                let mut address: u16 = state.bus.get_u8_at_address(1 + state.program_counter) as u16;
                //gets address at address 
                address = state.bus.get_u8_at_address(address) as u16;
                //adds offset
                address += state.y as u16;
                // returns value
                state.bus.get_u8_at_address(address)
            },
            // Used for branching, uses a signed 8 bit relative offset to add to the
            // program_counter. For purposes of this function it will return the same u8
            // as an immediate function would
            AddressingMode::Relative => state.bus.get_u8_at_address(1 + state.program_counter)
        };
        (opcode, operand)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum StatusFlag {
    Carry = 1 << 0,      
    Zero = 1 << 1,       
    InterruptDisable = 1 << 2,  
    Decimal = 1 << 3,    
    Break = 1 << 4,      
    Unused = 1 << 5,     
    Overflow = 1 << 6,   
    Negative = 1 << 7,   
}

#[derive(Debug)]
pub struct Status(u8);

impl Status {
    pub fn set(&mut self, flag: StatusFlag){
        self.0 = self.0 | (flag as u8)
    }
    pub fn unset(&mut self, flag: StatusFlag){
        self.0 = self.0 & !(flag as u8)
    }
    pub fn get(&mut self, flag: StatusFlag) -> bool {
        return self.0 & (flag as u8) != 0
    }
    pub fn set_from_bool(&mut self, flag: StatusFlag, value: bool) {
        if value {
            self.set(flag)
        } else {
            self.unset(flag)
        }
    }
}


#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum AddressingMode {
    Implied,
    Immediate,
    Accumulator,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
}

#[derive(Debug, Clone, Copy)]
pub enum CpuInstruction {
    ADC,AND,ASL,BCC,BCS,BEQ,BIT,BMI,BNE,BPL,BRK,BVC,BVS,CLC,
    CLD,CLI,CLV,CMP,CPX,CPY,DEC,DEX,DEY,EOR,INC,INX,INY,JMP,
    JSR,LDA,LDX,LDY,LSR,NOP,ORA,PHA,PHP,PLA,PLP,ROL,ROR,RTI,
    RTS,SBC,SEC,SED,SEI,STA,STX,STY,TAX,TAY,TSX,TXA,TXS,TYA
}

// Instruction type, addressing mode, size, minimum cycle count
const OpCodeInfo: [(CpuInstruction, AddressingMode, u16, u8); 256] = {
    use AddressingMode::*;
    use CpuInstruction::*;
    //not safe, but we expect that the unitialized parts of the array are never accessed as they
    //would have invalid addressing modes (probably they count as the Immediate addressing mode
    //since its 0 most likely.
    let mut temp: [(CpuInstruction, AddressingMode, u16, u8); 256] = unsafe {std::mem::zeroed()};

    // arithmetic instructions
    temp[0x69] = (ADC, Immediate, 2, 2);
    temp[0x65] = (ADC, ZeroPage, 2, 3);
    temp[0x75] = (ADC, ZeroPageX, 2, 4);
    temp[0x6D] = (ADC, Absolute, 3, 4);
    temp[0x7D] = (ADC, AbsoluteX, 3, 4);
    temp[0x79] = (ADC, AbsoluteY, 3, 4);
    temp[0x61] = (ADC, IndirectX, 2, 6);
    temp[0x71] = (ADC, IndirectY, 2, 5);
    temp[0xE9] = (SBC, Immediate, 2, 2);
    temp[0xE5] = (SBC, ZeroPage, 2, 3);
    temp[0xF5] = (SBC, ZeroPageX, 2, 4);
    temp[0xED] = (SBC, Absolute, 3, 4);
    temp[0xFD] = (SBC, AbsoluteX, 3, 4);
    temp[0xF9] = (SBC, AbsoluteY, 3, 4);
    temp[0xE1] = (SBC, IndirectX, 2, 6);
    temp[0xF1] = (SBC, IndirectY, 2, 5);

    temp[0x29] = (AND, Immediate,	2, 2);
    temp[0x25] = (AND, ZeroPage,	2, 3);
    temp[0x35] = (AND, ZeroPageX,	2, 4);
    temp[0x2D] = (AND, Absolute,	3, 4);
    temp[0x3D] = (AND, AbsoluteX,	3, 4);
    temp[0x39] = (AND, AbsoluteY,	3, 4);
    temp[0x21] = (AND, IndirectX,	2, 6);
    temp[0x31] = (AND, IndirectY,	2, 5);

    // shift left
    temp[0x0A] = (ASL, Accumulator, 1, 2);
    temp[0x06] = (ASL, ZeroPage,		2, 5);
    temp[0x16] = (ASL, ZeroPageX,		2, 6);
    temp[0x0E] = (ASL, Absolute,		3, 6);
    temp[0x1E] = (ASL, AbsoluteX,		3, 7);
    // branching instructions
    temp[0x90] = (BCC, Relative, 2, 2);
    temp[0xB0] = (BCS, Relative, 2, 2);
    temp[0xF0] = (BEQ, Relative, 2, 2);
    temp[0xB0] = (BCS, Relative, 2, 2);
    temp[0x30] = (BMI, Relative, 2, 2);
    temp[0xD0] = (BNE, Relative, 2, 2);
    temp[0x10] = (BPL, Relative, 2, 2);
    temp[0x50] = (BVC, Relative, 2, 2);
    temp[0x70] = (BVS, Relative, 2, 2);
    // clear instructions 
    temp[0x18] = (CLC, Implied, 1, 2);
    temp[0xD8] = (CLD, Implied, 1, 2);
    temp[0x58] = (CLI, Implied, 1, 2);
    temp[0xB8] = (CLV, Implied, 1, 2);
    // Break instruction. It has weird documentation, seems like this is the best info i can put
    temp[0x00] = (BRK, Implied, 2, 7);

    // Comparison instructions
    temp[0xc9] = (CMP, Immediate, 2, 2);
    temp[0xc5] = (CMP, ZeroPage, 2, 3);
    temp[0xd5] = (CMP, ZeroPageX, 2, 4);
    temp[0xcd] = (CMP, Absolute, 3, 4);
    temp[0xdd] = (CMP, AbsoluteX, 3, 4);
    temp[0xd9] = (CMP, AbsoluteY, 3, 4);
    temp[0xc1] = (CMP, IndirectX, 2, 6);
    temp[0xd1] = (CMP, IndirectY, 2, 5);
    temp[0xE0] = (CPX, Immediate, 2, 2);
    temp[0xE4] = (CPX, ZeroPage, 2, 3);
    temp[0xEC] = (CPX, Absolute, 3, 4);
    temp[0xC0] = (CPY, Immediate, 2, 2);
    temp[0xC4] = (CPY, ZeroPage, 2, 3);
    temp[0xCC] = (CPY, Absolute, 3, 4);

    // Decrement Instructions 
    temp[0xC6] = (DEC, ZeroPage, 2, 5);
    temp[0xD6] = (DEC, ZeroPageX, 2, 6);
    temp[0xCE] = (DEC, Absolute, 3, 6);
    temp[0xDE] = (DEC, AbsoluteX, 3, 7);
    temp[0xCA] = (DEX, Implied, 1, 2);
    temp[0x88] = (DEY, Implied, 1, 2);

    // Increment Instructions
    temp[0xE6] = (INC, ZeroPage, 2, 5);
    temp[0xF6] = (INC, ZeroPageX, 2, 6);
    temp[0xEE] = (INC, Absolute, 3, 6);
    temp[0xFE] = (INC, AbsoluteX, 3, 7);
    temp[0xE8] = (INX, Implied, 1, 2);
    temp[0xC8] = (INY, Implied, 1, 2);

    // Jumping instructions
    temp[0x4C] = (JMP, Absolute, 3, 3);
    temp[0x6C] = (JMP, Indirect, 3, 5);
    temp[0x20] = (JSR, Absolute, 3, 6);

    // Load instructions
    temp[0xA9] = (LDA, Immediate, 2, 2);
    temp[0xA5] = (LDA, ZeroPage, 2, 3);
    temp[0xB5] = (LDA, ZeroPageX, 2, 4);
    temp[0xAD] = (LDA, Absolute, 3, 4);
    temp[0xBD] = (LDA, AbsoluteX, 3, 4);
    temp[0xB9] = (LDA, AbsoluteY, 3, 4);
    temp[0xA1] = (LDA, IndirectX, 2, 6);
    temp[0xB1] = (LDA, IndirectY, 2, 5);
    temp[0xA2] = (LDX, Immediate, 2, 2);
    temp[0xA6] = (LDX, ZeroPage, 2, 3);
    temp[0xB6] = (LDX, ZeroPageY, 2, 4);
    temp[0xAE] = (LDX, Absolute, 3, 4);
    temp[0xBE] = (LDX, AbsoluteY, 3, 4);
    temp[0xA0] = (LDY, Immediate, 2, 2);
    temp[0xA4] = (LDY, ZeroPage, 2, 3);
    temp[0xB4] = (LDY, ZeroPageX, 2, 4);
    temp[0xAC] = (LDY, Absolute, 3, 4);
    temp[0xBC] = (LDY, AbsoluteX, 3, 4);

    // Logical shift
    temp[0x4A] = (LSR, Accumulator, 1, 2);
    temp[0x46] = (LSR, ZeroPage, 2, 5);
    temp[0x56] = (LSR, ZeroPageX, 2, 6);
    temp[0x4E] = (LSR, Absolute, 3, 6);
    temp[0x5E] = (LSR, AbsoluteX, 3, 7);

    // Exclusive OR
    temp[0x49] = (EOR, Immediate, 2, 2);
    temp[0x45] = (EOR, ZeroPage, 2, 3);
    temp[0x55] = (EOR, ZeroPageX, 2, 4);
    temp[0x4D] = (EOR, Absolute, 3, 4);
    temp[0x5D] = (EOR, AbsoluteX, 3, 4);
    temp[0x59] = (EOR, AbsoluteY, 3, 4);
    temp[0x41] = (EOR, IndirectX, 2, 6);
    temp[0x51] = (EOR, IndirectY, 2, 5);

    // Bitwise OR
    temp[0x09] = (ORA, Immediate, 2, 2);
    temp[0x05] = (ORA, ZeroPage, 2, 3);
    temp[0x15] = (ORA, ZeroPageX, 2, 4);
    temp[0x0D] = (ORA, Absolute, 3, 4);
    temp[0x1D] = (ORA, AbsoluteX, 3, 4);
    temp[0x19] = (ORA, AbsoluteY, 3, 4);
    temp[0x01] = (ORA, IndirectX, 2, 6);
    temp[0x11] = (ORA, IndirectY, 2, 5);

    // stack push instructions
    temp[0x48] = (PHA, Implied, 1, 3);
    temp[0x08] = (PHP, Implied, 1, 3);
    // stack pull instructions
    temp[0x68] = (PLA, Implied, 1, 4);
    temp[0x28] = (PLP, Implied, 1, 4);

    // Rotate instrunctions
    temp[0x2A] = (ROL, Accumulator, 1, 2);
    temp[0x26] = (ROL, ZeroPage, 2, 5);
    temp[0x36] = (ROL, ZeroPageX, 2, 6);
    temp[0x2E] = (ROL, Absolute, 3, 6);
    temp[0x3E] = (ROL, AbsoluteX, 3, 7);
    temp[0x6A] = (ROR, Accumulator, 1, 2);
    temp[0x66] = (ROR, ZeroPage, 2, 5);
    temp[0x76] = (ROR, ZeroPageX, 2, 6);
    temp[0x6E] = (ROR, Absolute, 3, 6);
    temp[0x7E] = (ROR, AbsoluteX, 3, 7);


    // Return from Interrupt
    temp[0x40] = (RTI, Implied, 1, 6);
    temp[0x60] = (RTS, Implied, 1, 6);

    // Flag setting
    temp[0x38] = (SEC, Implied, 1, 6);
    temp[0xf8] = (SED, Implied, 1, 6);
    temp[0x78] = (SEI, Implied, 1, 6);
    
    // memory write
    temp[0x85] = (STA, ZeroPage, 2, 3);
    temp[0x95] = (STA, ZeroPageX, 2, 4);
    temp[0x8D] = (STA, Absolute, 3, 4);
    temp[0x9D] = (STA, AbsoluteX, 3, 5);
    temp[0x99] = (STA, AbsoluteY, 3, 5);
    temp[0x81] = (STA, IndirectX, 2, 6);
    temp[0x91] = (STA, IndirectY, 2, 6);
    temp[0x86] = (STX, ZeroPage, 2, 3);
    temp[0x96] = (STX, ZeroPageY, 2, 4);
    temp[0x8E] = (STX, Absolute, 3, 4);
    temp[0x84] = (STY, ZeroPage, 2, 3);
    temp[0x94] = (STY, ZeroPageX, 2, 4);
    temp[0x8C] = (STY, Absolute, 3, 4);

    //register transfer instructions
    temp[0xAA] = (TAX, Implied, 1, 2);
    temp[0xA8] = (TAY, Implied, 1, 2);
    temp[0xBA] = (TSX, Implied, 1, 2);
    temp[0x8A] = (TXA, Implied, 1, 2);
    temp[0x9A] = (TXS, Implied, 1, 2);
    temp[0x98] = (TYA, Implied, 1, 2);

    //NOP
    temp[0xEA] = (NOP, Implied, 1, 2);


    temp[0x24] = (BIT, ZeroPage, 2, 3);
    temp[0x2C] = (BIT, Absolute, 3, 4);

    temp
};


fn execute_step(state: &mut NesEmulator) {
    let (opcode, operand) = state.get_current_opcode_operand();
    let (_, addressing, instruction_size, _) = OpCodeInfo[opcode as usize];

    match opcode {
        // BRK break or interrupt request
        0x00 => {
            unimplemented!("BRK")
        },
        // RTI return from interrupt
        0x40 => {
            unimplemented!("RTI")
        },
        // RTS return from subroutine
        0x60 => {
            let mut program_counter = 0;
            // 0x1000 offsets the value to the 01 page rather than the zero page.
            // get first byte
            state.stack += 1;
            program_counter |= state.bus.get_u8_at_address(0x0100 + state.stack as u16) as u16;
            // get MSB shift it and OR it with the program counter
            state.stack += 1;
            program_counter |= (state.bus.get_u8_at_address(0x0100 + state.stack as u16) as u16) << 8;
            // move program_counter one byte for whatever reason and early return.
            state.program_counter = program_counter + 1;
            return;
        }
        // SBC subtract with carry
        0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
            // Using the formula A = A - operand - ~C = A + ~operand + C
            // checking for overflow by doing the addition with 16 bits first
            let acc = state.accumulator as u16;
            let op = (!operand) as u16;
            let carry = state.status.get(StatusFlag::Carry) as u8;
            // setting the flag if overflow would occur
            state.status.set_from_bool(StatusFlag::Carry, (acc + op + carry as u16) > u8::MAX.into());

            let result = state.accumulator.wrapping_add(operand).wrapping_add(carry);

            // set the appropiate flags based on result
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);
            // Inverted values from the logic in ADC
            state.status.set_from_bool(StatusFlag::Overflow, !((state.accumulator ^ result) & (result ^ operand) & 0x80 != 0));
            state.accumulator = result;
        }


        // ADC. Add with carry instructions
        0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
            // checking for overflow by doing the addition with 16 bits first
            let acc = state.accumulator as u16;
            let op = operand as u16;
            let carry = state.status.get(StatusFlag::Carry) as u8;
            // setting the flag if overflow would occur
            state.status.set_from_bool(StatusFlag::Carry, (acc + op + carry as u16) > u8::MAX.into());

            let result = state.accumulator.wrapping_add(operand).wrapping_add(carry);

            // set the appropiate flags based on result
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);
            // This flag is set if the seventh bit of the accumulator and the operand are different
            state.status.set_from_bool(StatusFlag::Overflow, (state.accumulator ^ result) & (result ^ operand) & 0x80 != 0);

            state.accumulator = result;
        },
        //Bitwise AND/OR/EOR between accumulator and operand
        0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 |
        0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 |
        0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51
        => {
            //let (addressing, instruction_size) = OpCodeInfo[opcode as usize];
            //could do this directly on accumulator, but im trusting the compiler again and using
            //the result variable for consistency
            let result = match operand {
                // AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                    state.accumulator & operand
                },
                // ORA
                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
                    state.accumulator | operand
                },
                // EOR
                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
                    state.accumulator ^ operand
                },
                _ => unreachable!()
            };
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);
            state.accumulator = result;
        },
        // ASL Arithmetic Shift Left and LSR logical shift right.
        // Mutates a register, or memory.
        0x0A | 0x06 | 0x16 | 0x0E | 0x1E |
        0x4A | 0x46 | 0x56 | 0x4E | 0x5E
        => {
            let result = match operand {
                // ASL 
                0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
                    // sets the 7th bit of the operand into the carry bit before it is lost after shifting
                    state.status.set_from_bool(StatusFlag::Carry, (operand >> 7) != 0);
                    operand << 1
                },
                // LSR
                0x4A | 0x46 | 0x56 | 0x4E | 0x5E => {
                    // sets all bits except for the first one to 0 in the operand
                    state.status.set_from_bool(StatusFlag::Carry, (operand & 1) != 0);
                    operand >> 1
                },
                _ => {
                    unreachable!()
                }
            };

            // set flags based on result
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);

            match addressing {
                AddressingMode::Accumulator => {
                    state.accumulator = result;
                },
                AddressingMode::ZeroPage => {
                    let address = state.bus.get_u8_at_address(state.program_counter + 1) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::ZeroPageX => {
                    let address = (state.bus.get_u8_at_address(state.program_counter + 1) + state.x) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::Absolute => {
                    let address = state.bus.get_u16_at_address(state.program_counter + 1) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::AbsoluteX => {
                    let address = state.bus.get_u16_at_address(state.program_counter + 1) + state.x as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                _ => {
                    unreachable!()
                }
            }
        },
        // PHA | PHP. Pushes accumulator or status into the stack.
        0x48 | 0x08 => {
            let value_to_store = if opcode == 0x48 {state.accumulator} else {state.status.0};
            // 0x1000 offsets the value to the 01 page rather than the zero page.
            state.bus.write_u8_at_address(0x0100 + state.stack as u16, value_to_store);
            state.stack -= 1;
        },
        // PLA | PLP. Pulls values from the stack
        0x68 | 0x28 => {
            let register = if opcode == 0x48 {&mut state.accumulator} else {&mut state.status.0};
            // 0x1000 offsets the value to the 01 page rather than the zero page.
            state.stack += 1;
            // Changing the interrupt disable flag in the processor status flags should take one
            // more cycle but i will assume it does not matter for now
            *register = state.bus.get_u8_at_address(0x0100 + state.stack as u16);
        },
        // ROR
        0x6A | 0x66 | 0x76 | 0x6E | 0x7E |
        // ROL
        0x2A | 0x26 | 0x36 | 0x2E | 0x3E
        => {
            let carry_mask = state.status.get(StatusFlag::Carry) as u8;
            let result = match opcode {
                0x6A | 0x66 | 0x76 | 0x6E | 0x7E => {
                    state.status.set_from_bool(StatusFlag::Carry, (operand << 7) != 0);
                    operand >> 1 | (carry_mask << 7)
                },
                0x2A | 0x26 | 0x36 | 0x2E | 0x3E => {
                    state.status.set_from_bool(StatusFlag::Carry, (operand >> 7) != 0);
                    operand << 1 | carry_mask
                },
                _ => unreachable!()
            };

            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);

            match addressing {
                AddressingMode::Accumulator => {
                    state.accumulator = result;
                },
                AddressingMode::ZeroPage => {
                    let address = state.bus.get_u8_at_address(state.program_counter + 1) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::ZeroPageX => {
                    let address = (state.bus.get_u8_at_address(state.program_counter + 1) + state.x) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::Absolute => {
                    let address = state.bus.get_u16_at_address(state.program_counter + 1) as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                AddressingMode::AbsoluteX => {
                    let address = state.bus.get_u16_at_address(state.program_counter + 1) + state.x as u16;
                    state.bus.write_u8_at_address(address, result);
                },
                _ => {
                    unreachable!()
                }
            }
        }
        //jumping instructions. 
        op @ (0x90 | 0xB0 | 0xF0 | 0x30 | 0xD0 | 0x10| 0x50 | 0x70) => {
            let condition = match op {
                // BCC jumps if carry is clear
                0x90 => {
                    !state.status.get(StatusFlag::Carry)
                },
                // BCS jumps if carry is set
                0xB0 => {
                    state.status.get(StatusFlag::Carry)
                },
                // BEQ branches if the zero flag is set
                0xF0 => {
                    state.status.get(StatusFlag::Zero)
                },
                // BMI branches if the Negative flag is set
                0x30 => {
                    state.status.get(StatusFlag::Negative)
                },
                // BNE branches if not equal. (branches if zero flag is clear)
                0xD0 => {
                    !state.status.get(StatusFlag::Zero)
                },
                // BPL branches if the Negative flag is clear.
                0x10 => {
                    !state.status.get(StatusFlag::Negative)
                },
                // BVC branches if overflow is clear
                0x70 => {
                    !state.status.get(StatusFlag::Overflow)
                },
                // BVS branches if overflow is set
                0x50 => {
                    state.status.get(StatusFlag::Overflow)
                },
                _ => {
                    unreachable!()
                }
            };
            state.program_counter += 2;
            if condition {
                // converts operand to i8 so that it is signed, then converts it to i16 so that it has
                // the same size as the u16 in the bus. Finally convert both to i16 to perform the
                // signed addition, then back to u16 to set it as the program_counter
                state.program_counter = (state.program_counter as i16 + (operand as i8) as i16) as u16;
            }
            return
        },
        // Bit clearing instructions
        op @ (0x18 | 0xd8 | 0x58 | 0xb8) => {
            let flag = match op {
                // CLC
                0x18 => StatusFlag::Carry,
                // CLD
                0xd8 => StatusFlag::Decimal,
                // CLI
                0x58 => StatusFlag::InterruptDisable,
                // CLV
                0xb8 => StatusFlag::Overflow,
                _ => {
                    unreachable!()
                }
            };
            state.status.set_from_bool(flag, false);
        },
        //CMP. Compares the A register with operand
        (0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1) => {
            // Subtraction of A with operand using some bit trickery to do with u8s.
            let result = state.accumulator.wrapping_add((!operand).wrapping_add(1));
            state.status.set_from_bool(StatusFlag::Carry, state.accumulator >= operand);
            state.status.set_from_bool(StatusFlag::Zero, state.accumulator == operand);
            state.status.set_from_bool(StatusFlag::Negative, (state.accumulator >> 7) != 0);
        },
        // CPX Compare with X
        0xE0 | 0xE4 | 0xEC => {
            let register = state.x;
            let result = register.wrapping_add(1 + !operand);
            state.status.set_from_bool(StatusFlag::Carry, register >= operand);
            state.status.set_from_bool(StatusFlag::Zero, register == operand);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) != 0);
        },
        // CPY Compare with Y
        0xC0 | 0xC4 | 0xCC => {
            let register = state.x;
            let result = register.wrapping_add(1 + !operand);
            state.status.set_from_bool(StatusFlag::Carry, register >= operand);
            state.status.set_from_bool(StatusFlag::Zero, register == operand);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) != 0);
        },
        // INC and DEC. Decrements or Increments memory
        0xC6 | 0xD6 | 0xCE | 0xDE | 0xE6 | 0xF6 | 0xEE | 0xFE => {
            
            let result = match operand {
                0xC6 | 0xD6 | 0xCE | 0xDE  => operand.wrapping_sub(1),
                0xE6 | 0xF6 | 0xEE | 0xFE  => operand.wrapping_add(1),
                _ => unreachable!()
            };
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) != 0);

            let address = match addressing {
                AddressingMode::ZeroPage => state.bus.get_u8_at_address(1 + state.program_counter) as u16,
                AddressingMode::ZeroPageX => (state.bus.get_u8_at_address(1 + state.program_counter) + state.x) as u16,
                AddressingMode::Absolute => state.bus.get_u16_at_address(1 + state.program_counter),
                AddressingMode::AbsoluteX => state.bus.get_u16_at_address(1 + state.program_counter) + state.x as u16,
                _=> unreachable!()
            };
            state.bus.write_u8_at_address(address, result);
        },
        // INX and DEX Decrements the X register
        0xCA | 0xE8 => {
            let result = if opcode == 0xCA {state.x.wrapping_sub(1)} else {state.x.wrapping_add(1)};
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) != 0);
            state.x = result;
        },
        // INY and DEY Decrements the Y register
        0x88 | 0xC8 => {
            let result = if opcode == 0xC8 {state.x.wrapping_sub(1)} else {state.x.wrapping_add(1)};
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) != 0);
            state.y = result;
        },

        // NOP/
        0xEA => {
        },

        // Loading instructions. LDA, LDX, LDY
        0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1
        | 0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE
        | 0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC
        => {
            let register = match opcode {
                0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                    &mut state.accumulator
                },
                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
                    &mut state.x
                },
                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
                    &mut state.y
                },
                _ => {
                    unreachable!()
                }
            };
            *register = operand;
            state.status.set_from_bool(StatusFlag::Zero, operand == 0);
            state.status.set_from_bool(StatusFlag::Negative, (operand >> 7) != 0);
        },



        // BIT bit test instructions 
        0x24 | 0x2c => {
            let result = state.accumulator & operand;
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Overflow, (operand & 0b01000000) != 0);
            state.status.set_from_bool(StatusFlag::Negative, (operand & 0b10000000) != 0);
        },
        // JMP
        0x4c | 0x6c => { 
            //gets the 2 bytes that follow the opcode as the address to jump to
            let address: u16 = state.bus.get_u16_at_address(1 + state.program_counter);
            match addressing {
                AddressingMode::Absolute => {state.program_counter = address;},
                AddressingMode::Indirect => {state.program_counter = state.bus.get_u16_at_address(address);},
                _ => unreachable!()
            }
        },
        // JSR jump to subroutine
        0x20 => {
            // stores the address of the next instruction into the stack
            // address should be stored by pushing the high bits first and then the low bits
            state.bus.write_u8_at_address(0x0100 + state.stack as u16, ((state.program_counter + instruction_size) >> 8) as u8);
            state.stack -= 1;

            state.bus.write_u8_at_address(0x0100 + state.stack as u16, (state.program_counter + instruction_size) as u8);
            state.stack -= 1;
            // get address 
            let address: u16 = state.bus.get_u16_at_address(1 + state.program_counter);
            state.program_counter = address;

        },
        // SEI, SED, SEC. Instructions for setting flags of the status register
        0x78 | 0xF8 | 0x38 => { 
            let flag = match  opcode {
                0x78 => StatusFlag::InterruptDisable,
                0xF8 => StatusFlag::Decimal,
                0x38 => StatusFlag::Carry,
                _ => unreachable!()
            };
            state.status.set(flag);
            state.program_counter += 1;
        },
        // STA, STX, STY store register instructions
        0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 |
        0x86 | 0x96 | 0x8E |
        0x84 | 0x94 | 0x8C
        => {
            let address: u16 = match addressing {
                AddressingMode::ZeroPage => state.bus.get_u8_at_address(1 + state.program_counter) as u16,
                AddressingMode::ZeroPageX => state.bus.get_u8_at_address(1 + state.program_counter).wrapping_add(state.x) as u16,
                AddressingMode::Absolute => state.bus.get_u16_at_address(1 + state.program_counter),
                AddressingMode::AbsoluteX => state.bus.get_u16_at_address(1 + state.program_counter) + state.x as u16,
                AddressingMode::AbsoluteY => state.bus.get_u16_at_address(1 + state.program_counter) + state.y as u16,
                AddressingMode::IndirectX => {
                    let address = state.bus.get_u8_at_address(1 + state.program_counter).wrapping_add(state.x) as u16;
                    state.bus.get_u16_at_address(address)
                },
                AddressingMode::IndirectY => {
                    let address = state.bus.get_u8_at_address(1 + state.program_counter) as u16;
                    state.bus.get_u16_at_address(address) + state.y as u16
                },
                _ => unreachable!(),
            };
            let register_value = match opcode {
                0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => state.accumulator,
                0x86 | 0x96 | 0x8E => state.x,
                0x84 | 0x94 | 0x8C => state.y,
                _ => unreachable!()
            };
            state.bus.write_u8_at_address(address, register_value);
        }
        // Transfer between registers instructions 
        // TAX | TAY | TSX | TXA | TXS | TYA
        0xAA | 0xA8 | 0xBA | 0x8A | 0x9A | 0x98 => {
            let source = match opcode {
                0xAA | 0xA8 => state.accumulator,
                0xBA  => state.stack,
                0x8A | 0x9A => state.x,
                0x98 => state.y,
                _ => unreachable!()
            };
            let destination = match opcode {
                0xAA | 0xBA => &mut state.x,
                0xA8 => &mut state.y,
                0x8A | 0x98 => &mut state.accumulator,
                0x9A => &mut state.stack,
                _ => unreachable!()
            };
            *destination = source;
            state.status.set_from_bool(StatusFlag::Zero, *destination == 0);
            state.status.set_from_bool(StatusFlag::Negative, (*destination >> 7) != 0);
            
        }
        _ => {
            panic!("This is a bad opcode")
        }
    };
    state.program_counter += instruction_size;
}

fn main() {
    let file = std::fs::read("rom").unwrap();
    let (header, content) = (&file[0..16], &file[16..]);
    let mut bus = Bus::new().set_rom(content);
    // check header
    if header[0..4] == [0x4E, 0x45, 0x53, 0x1A] {
        println!("Header is correct!")
    }

    let mut emulator = NesEmulator::new(bus);

    print!("{:#x}\n", emulator.bus.get_u8_at_address(emulator.program_counter));
    execute_step(&mut emulator);
    print!("{:#x}\n", emulator.bus.get_u8_at_address(emulator.program_counter));
    execute_step(&mut emulator);
    print!("{:#x}\n", emulator.bus.get_u8_at_address(emulator.program_counter));
    execute_step(&mut emulator);
    print!("{:#x}\n", emulator.bus.get_u8_at_address(emulator.program_counter));
    execute_step(&mut emulator);
    print!("{:#x}\n", emulator.bus.get_u8_at_address(emulator.program_counter));
}
