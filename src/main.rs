mod nes;

use clap::Parser;
use clap_num::maybe_hex;
use log;
use simple_logger;

use nes::Nes;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    rom: String,

    #[arg(short, long)]
    disassemble: bool,

    #[arg(long)]
    trace_cpu: bool,

    #[arg(short, long, value_parser=maybe_hex::<u16>)]
    breakpoint: Option<u16>,
}

fn main() {
    let args = Args::parse();

    //TODO: divorce dissassembly and log level flags
    let log_level = if args.disassemble {
        log::Level::Trace
    } else {
        log::Level::Warn
    };

    simple_logger::init_with_level(log_level).unwrap();

    let mut console = Nes::new(args.rom, args.disassemble).expect("unable to create console");
    console.run(args.trace_cpu, args.breakpoint);
}
