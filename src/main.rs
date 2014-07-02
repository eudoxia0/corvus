extern crate reader;

fn main() {
    loop {
        print!("> ");
        let input = reader::readStream(reader::stdinReader());
        print!("\n");
    }
}
