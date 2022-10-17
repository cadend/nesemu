use crate::nes::cartridge::Cartridge;

// 0x0000 --- 0x07ff = internal RAM
// 0x0800 --- 0x0fff = RAM mirror
// 0x1000 --- 0x17ff = RAM mirror
// 0x1800 --- 0x1fff = RAM mirror
// 0x2000 --- 0x2007 = PPU registers
// 0x2008 --- 0x3fff = mirrored PPU registers every 8 bytes
// 0x4000 --- 0x4017 = APU registers
// 0x4018 --- 0x401f = APU and I/O functionality that is normally disabled (CPU test mode)
// 0x4020 --- 0xffff = cartridge space
pub struct Memory {
    internal_ram: [u8; 0x800],
    ppu_registers: [u8; 0x8],
    apu_registers: [u8; 0x18],
    cartridge: Cartridge,
}

impl Memory {
    pub fn new(cartridge: Cartridge) -> Self {
        let mut m = Memory {
            internal_ram: [0; 0x800],
            ppu_registers: [0; 0x8],
            apu_registers: [0; 0x18],
            cartridge,
        };
        //TODO: remove this hack, just gets past PPU wait loops to keep implementing instructions for now
        m.ppu_registers[2] = 0x80;
        m
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        let a = addr as usize;

        match addr {
            0x0000..=0x1fff => self.internal_ram[a % 0x0800],
            0x2000..=0x3fff => self.ppu_registers[a % 8],
            0x4000..=0x4017 => self.apu_registers[a - 0x4000],
            0x4018..=0x401f => panic!("disabled register at {:#06x} not supported", addr),
            0x4020..=0xffff => self.cartridge.read_u8(addr),
        }
    }

    pub fn write_u8(&mut self, addr: u16, val: u8) {
        let a = addr as usize;

        match addr {
            0x0000..=0x1fff => self.internal_ram[a % 0x0800] = val,
            0x2000..=0x3fff => self.ppu_registers[a % 8] = val,
            0x4000..=0x4017 => self.apu_registers[a - 0x4000] = val,
            0x4018..=0x401f => panic!("disabled register at {:#06x} not supported", addr),
            0x4020..=0xffff => unimplemented!("writes to cartridge address space not supported"),
        }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read_u8(addr) as u16;
        let hi = (self.read_u8(addr + 1) as u16) << 8;

        lo | hi
    }
}
