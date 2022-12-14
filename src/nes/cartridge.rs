use log;

use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum NametableMirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

impl NametableMirroring {
    fn from_byte(b: u8) -> Self {
        if (b >> 3) & 1 > 0 {
            NametableMirroring::FourScreen
        } else if b & 1 > 0 {
            NametableMirroring::Vertical
        } else {
            NametableMirroring::Horizontal
        }
    }
}

#[derive(Debug)]
struct Header {
    prg_rom_size: u8,
    chr_rom_size: u8,
    mapper_id: u8,
    trainer_present: bool,
    persistent_memory_present: bool,
    mirroring: NametableMirroring,
    vs_unisystem: bool,
    nes_2: bool,
    prg_ram_size: u8,
}

pub struct Cartridge {
    header: Header,
    trainer: Option<[u8; 0x200]>,
    prg_rom: Vec<u8>,
    prg_ram: Option<[u8; 0x2000]>,
    chr_rom: Vec<u8>,
}

impl Header {
    fn parse_header(h: &[u8]) -> Self {
        // if the last 4 bytes are not all zero, and the header is not marked for NES 2.0 format,
        // an emulator should either mask off the upper 4 bits of the mapper number or simply refuse to load the ROM.
        let mut mapper_id = ((h[6] >> 4) & 0b1111) | (h[7] & 0b11110000);
        if u32::from_le_bytes(h[12..16].try_into().expect("truncated ROM data")) != 0 {
            log::warn!("unexpected data found, masking mapper ID");
            mapper_id = mapper_id & 0b00001111;
        }

        Header {
            prg_rom_size: h[4],
            chr_rom_size: h[5],
            mapper_id: mapper_id,
            trainer_present: (h[6] >> 2) & 1 > 0,
            persistent_memory_present: (h[6] >> 1) > 0,
            mirroring: NametableMirroring::from_byte(h[6]),
            vs_unisystem: h[7] & 1 > 0,
            nes_2: (h[7] >> 2) & 0b11 == 2,
            prg_ram_size: h[8],
        }
    }
}

impl Cartridge {
    pub fn new(filename: &String) -> std::io::Result<Self> {
        let mut file = File::open(filename)?;

        let mut header_data = vec![0u8; 16];
        file.read_exact(&mut header_data)?;
        let header = Header::parse_header(&header_data);
        if header.mapper_id != 0 {
            panic!("only mapper 0 (NROM) is currently supported")
        }

        log::trace!("loading cartridge with header {:#?}", header);

        let trainer = if header.trainer_present {
            let mut t = [0u8; 0x200];
            file.read_exact(&mut t)?;
            Some(t)
        } else {
            None
        };

        let mut prg_rom = vec![0u8; 16384 * header.prg_rom_size as usize];
        file.read_exact(&mut prg_rom)?;

        // just always give a full 8KiB of ram if present, don't bother dealing with size in header
        let prg_ram = if header.persistent_memory_present {
            Some([0u8; 0x2000])
        } else {
            None
        };

        let mut chr_rom = vec![0u8; 8192 * header.chr_rom_size as usize];
        file.read_exact(&mut chr_rom)?;

        Ok(Cartridge {
            header,
            trainer,
            prg_rom,
            prg_ram,
            chr_rom,
        })
    }

    //TODO: move to dedicated mapper trait. hardcoding for NROM to allow forward movement
    pub fn read_u8(&self, addr: u16) -> u8 {
        let nrom256 = self.header.prg_rom_size == 2;

        let a = addr as usize;
        match addr {
            0x6000..=0x7fff => {
                if let Some(ram) = self.prg_ram.as_ref() {
                    ram[a - 0x6000]
                } else {
                    panic!(
                        "attempted reading PRG ram at {:#06x} when unallocated",
                        addr
                    )
                }
            }
            0x8000..=0xbfff => self.prg_rom[a - 0x8000],
            0xc000..=0xffff => {
                if nrom256 {
                    self.prg_rom[a - 0x8000]
                } else {
                    self.prg_rom[a - 0xc000]
                }
            }
            _ => panic!("unsupported cartridge memory read at {:#06x}", addr),
        }
    }
}
