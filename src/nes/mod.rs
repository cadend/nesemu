mod cartridge;
mod cpu;

use std::time::{Duration, Instant};

use cartridge::Cartridge;

// NTSC NES runs at 60.0988 Hz
const FRAME_TIME: Duration = Duration::from_secs(1 / 60);

pub struct Nes {
    cpu: cpu::Cpu,
    cycles_spent: usize,
    elapsed_time: Duration,
}

impl Nes {
    pub fn new(cartridge_path: String) -> std::io::Result<Self> {
        let cartridge = Cartridge::new(&cartridge_path)?;

        Ok(Nes {
            cpu: cpu::Cpu::new(cartridge),
            cycles_spent: 0,
            elapsed_time: Duration::ZERO,
        })
    }

    pub fn run(&mut self, trace_cpu: bool, breakpoint: Option<u16>) {
        if breakpoint.is_some() {
            self.cpu.set_breakpoint(breakpoint.unwrap());
        }

        // eventual loop to sync with frame timings

        // loop {
        //   let now = Instant::now();
        //
        //   if !ppu.is_frame_ready() {
        //     self.cycles_spent += 1;
        //     self.ppu.tick();
        //     if self.cycles_spent % 3 == 0 {
        //       self.cpu.tick();
        //     }
        //   }
        //   if ppu.is_frame_ready() && self.elapsed_time + now.elapsed() > FRAME_TIME {
        //     ppu.render_frame();
        //     self.elapsed_time = Duration::ZERO;
        //   } else {
        //     self.elapsed_time += now.elapsed();
        //   }
        // }

        loop {
            self.cycles_spent += 1;

            // 3 PPU ticks per CPU tick for NTSC
            // self.ppu.tick();

            if self.cycles_spent % 3 == 0 {
                self.cpu.tick(trace_cpu);
            }
            // frame.render()
        }
    }
}
