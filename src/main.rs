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
        let (addressing, instruction_size) = OpCodeInfo[opcode as usize];
        let operand = match addressing {
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
                address += state.x;
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
                //adds offset
                address += state.y as u16;
                //gets address at address 
                address = state.bus.get_u8_at_address(address) as u16;
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

const OpCodeInfo: [(AddressingMode, u16); 256] = {
    use AddressingMode::*;
    //not safe, but we expect that the unitialized parts of the array are never accessed as they
    //would have invalid addressing modes (probably they count as the Immediate addressing mode
    //since its 0 most likely.
    let mut temp: [(AddressingMode, u16); 256] = unsafe {std::mem::zeroed()};
    // ADC Add with carry to accumulator 
    temp[0x69] = (Immediate, 2);
    temp[0x65] = (ZeroPage, 2);
    temp[0x75] = (ZeroPageX, 2);
    temp[0x6D] = (Absolute, 3);
    temp[0x7D] = (AbsoluteX, 3);
    temp[0x79] = (AbsoluteY, 3);
    temp[0x61] = (IndirectX, 3);
    temp[0x71] = (IndirectY, 3);
    // AND with accumulator
    temp[0x29] = (Immediate,	2);
    temp[0x25] = (ZeroPage,	2);
    temp[0x35] = (ZeroPageX,	2);
    temp[0x2D] = (Absolute,	3);
    temp[0x3D] = (AbsoluteX,	3);
    temp[0x39] = (AbsoluteY,	3);
    temp[0x21] = (IndirectX,	2);
    temp[0x31] = (IndirectY,	2);
    // ASL Shift Left one bit
    temp[0x0A] = (Accumulator, 1);
    temp[0x06] = (ZeroPage,		2);
    temp[0x16] = (ZeroPageX,		2);
    temp[0x0E] = (Absolute,		3);
    temp[0x1E] = (AbsoluteX,		3);
    // BCC
    temp[0x90] = (Relative, 2);

    temp
};


fn execute_step(state: &mut NesEmulator) {
    let (opcode, operand) = state.get_current_opcode_operand();
    let (addressing, instruction_size) = OpCodeInfo[opcode as usize];

    match opcode {
        // Add with carry instructions
        0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
            // checking for overflow by doing the addition with 16 bits first
            let acc = state.accumulator as u16;
            let op = operand as u16;
            // setting the flag if overflow would occur
            state.status.set_from_bool(StatusFlag::Carry, acc + op > u8::MAX.into());

            let result = state.accumulator + operand;

            // set the appropiate flags based on result
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);
            // This flag is set if the seventh bit of the accumulator and the operand are different
            state.status.set_from_bool(StatusFlag::Overflow, (state.accumulator ^ result) & (result ^ operand) & 0x80 != 0);

            state.accumulator = result;
            state.program_counter += instruction_size;
            //state.status |= 
        },
        //Bitwise AND between accumulator and operand
        0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
            let (addressing, instruction_size) = OpCodeInfo[opcode as usize];
            //could do this directly on accumulator, but im trusting the compiler again and using
            //the result variable for consistency
            let result = state.accumulator & operand;
            state.status.set_from_bool(StatusFlag::Zero, result == 0);
            state.status.set_from_bool(StatusFlag::Negative, (result >> 7) == 0);

            state.accumulator = result;
            state.program_counter += instruction_size;

        },
        0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
            let result = operand << 1;
            // sets the 7th bit of the operand into the carry bit before it is lost after shifting
            state.status.set_from_bool(StatusFlag::Carry, (operand >> 7) != 0);
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
            state.program_counter += instruction_size;
        },
        //jumping instructions
        op @ (0x90 | 0xB0 | 0xF0) => {
            // converts operand to i8 so that it is signed, then converts it to i16 so that it has
            // the same size as the u16 in the bus. Finally convert both to i16 to perform the
            // signed addition, then back to u16 to set it as the program_counter
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
                _ => {
                    unreachable!()
                }
            };
            state.program_counter += 2;
            if condition {
                state.program_counter = (state.program_counter as i16 + (operand as i8) as i16) as u16;
            }
        },
        0x4c => { 
            //gets the 2 bytes that follow the opcode
            let address: u16 = state.bus.get_u16_at_address(1 + state.program_counter);
            state.program_counter = address;
        },
        0x78 => { 
            //state.status &= StatusFlag::InterruptDisable as u8;
            state.program_counter += 1;
        },
        0x8d => {
            let address: u16 = state.bus.get_u16_at_address(1 + state.program_counter);
            // state.bus[address as usize] = state.accumulator;
            print!("address modified {:#x}\n", address);
            state.program_counter += 3;
        }
        0xA9 => { 
            state.accumulator = state.bus.get_u8_at_address(state.program_counter + 1);
            /*
            if state.accumulator == 0 {
            state.status |= StatusFlag::Zero as u8
            }
            if state.accumulator >> 7 ==1 {
            state.status |= StatusFlag::Negative as u8
            }
            */
            state.program_counter += 2;
        },
        _ => {

        }
    };
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
