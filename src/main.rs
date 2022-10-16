mod err;
mod rom;

use clap::Parser;

use rom::Rom;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    rom: String,
}

fn main() {
    let args = Args::parse();

    match Rom::new(&args.rom) {
        Ok(_) => println!("loaded rom"),
        Err(e) => println!("error loading rom: {:?}", e),
    }
}
