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
    trace: bool,

    #[arg(long)]
    trace_cpu: bool,

    #[arg(short, long, value_parser=maybe_hex::<u16>)]
    breakpoint: Option<u16>,
}

fn main() {
    let args = Args::parse();

    let log_level = if args.trace {
        log::Level::Trace
    } else {
        log::Level::Warn
    };
    simple_logger::init_with_level(log_level).unwrap();

    let mut console = Nes::new(args.rom).expect("unable to create console");
    console.run(args.trace_cpu, args.breakpoint);
}
