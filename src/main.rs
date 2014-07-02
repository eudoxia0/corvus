extern crate ast;
extern crate reader;

fn main() {
    loop {
        print!("> ");
        let input = reader::read_stream(reader::stdin_reader());
        print!("{}\n", ast::print(input));
    }
}
