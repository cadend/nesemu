mod err;
mod nes;

use clap::Parser;

use nes::Nes;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    rom: String,
}

fn main() {
    let args = Args::parse();

    let mut console = Nes::new(args.rom).expect("unable to create console");
    console.run();
}
