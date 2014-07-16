extern crate ast;
extern crate reader;

fn main() {
    loop {
        print!("> ");
        let mut reader = reader::stdin_reader();
        let input = reader::read_stream(&mut reader);
        print!("{}\n", ast::print(input));
    }
}
