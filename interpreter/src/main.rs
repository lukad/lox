mod env;
mod interpreter;
mod resolver;
mod value;

use interpreter::Interpreter;
use resolver::Resolver;

fn main() {
    let path = std::env::args().nth(1).expect("no path provided");
    let source = std::fs::read_to_string(path).expect("failed to read file");

    let tokens = lox_parser::lex(&source).unwrap();
    let mut tokens = multipeek::multipeek(tokens.iter());
    let program = lox_parser::parse(&mut tokens).unwrap();

    let mut interpreter = Interpreter::new();
    let mut resolver = Resolver::new();
    resolver.resolve(&program).unwrap();
    resolver.apply_resolutions(&mut interpreter);

    interpreter.interpret(&program).unwrap();
}
