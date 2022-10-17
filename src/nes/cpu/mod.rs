mod memory;

use crate::nes::cartridge::Cartridge;
use memory::Memory;

#[derive(Debug)]
pub struct Cpu {
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    sp: u8,
    pc: u16,
    flags: u8,
    memory: Memory,
}

mod flags {
    pub const CARRY: u8 = 0b00000001;
    pub const ZERO: u8 = 0b00000010;
    pub const INTERRUPT_DISABLE: u8 = 0b00000100;
    pub const DECIMAL: u8 = 0b00001000;
    pub const OVERFLOW: u8 = 0b01000000;
    pub const NEGATIVE: u8 = 0b10000000;
}

impl Cpu {
    pub fn new(cartridge: Cartridge) -> Self {
        let mut cpu = Cpu {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            sp: 0xfd,
            pc: 0,
            flags: 0,
            memory: Memory::new(cartridge),
        };
        cpu.reset();
        cpu
    }

    fn set_b_flag(&mut self, bit_5: bool, bit_4: bool) {
        let mut mask = 0b11001111;
        if bit_4 {
            mask = mask | 0b00010000;
        }
        if bit_5 {
            mask = mask | 0b00100000;
        }
        self.flags = self.flags & mask;
    }

    fn reset(&mut self) {
        self.sp = 0xfd;
        self.pc = self.memory.read_u16(0xfffc);
        self.flags = self.flags | flags::INTERRUPT_DISABLE;
        self.set_b_flag(true, false);
    }

    pub fn step(&mut self) {
        println!("program counter at {:#06x}", self.pc);
        panic!();
    }
}
