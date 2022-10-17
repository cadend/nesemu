mod instruction;
mod memory;

use log;

use crate::nes::cartridge::Cartridge;
use memory::Memory;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    flags: u8,
    memory: Memory,
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "CPU \n  a: {:#04X} x: {:#04X} y: {:#04X}",
            self.a, self.x, self.y
        )?;
        writeln!(f, "  sp: {:#04X} pc: {:#06X}", self.sp, self.pc)?;
        write!(
            f,
            "  C: {} Z: {} I: {} D: {} V: {} N: {}",
            self.flags & flags::CARRY > 1,
            self.flags & flags::ZERO > 1,
            self.flags & flags::INTERRUPT_DISABLE > 1,
            self.flags & flags::DECIMAL > 1,
            self.flags & flags::OVERFLOW > 1,
            self.flags & flags::NEGATIVE > 1
        )
    }
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
            a: 0,
            x: 0,
            y: 0,
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
        self.flags &= mask;
    }

    fn reset(&mut self) {
        self.sp = 0xfd;
        self.pc = self.memory.read_u16(0xfffc);
        self.flags |= flags::INTERRUPT_DISABLE;
        self.set_b_flag(true, false);
    }

    pub fn step(&mut self, trace_cpu: bool) {
        let i = instruction::get_instruction(self.memory.read_u8(self.pc));
        //TODO: manage cycles
        let _cycles_consumed = i.execute(self);
        if trace_cpu {
            log::trace!("{:#?}", self);
        }
    }
}
