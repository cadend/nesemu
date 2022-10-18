mod cartridge;
mod cpu;

use cartridge::Cartridge;

pub struct Nes {
    cpu: cpu::Cpu,
}

impl Nes {
    pub fn new(cartridge_path: String) -> std::io::Result<Self> {
        let cartridge = Cartridge::new(&cartridge_path)?;

        Ok(Nes {
            cpu: cpu::Cpu::new(cartridge),
        })
    }

    pub fn run(&mut self, trace_cpu: bool, breakpoint: Option<u16>) {
        if breakpoint.is_some() {
            self.cpu.set_breakpoint(breakpoint.unwrap());
        }
        loop {
            self.cpu.step(trace_cpu);
        }
    }
}
