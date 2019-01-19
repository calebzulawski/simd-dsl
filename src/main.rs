mod builtins;
mod full_ast;
mod lexer;
mod parser;
mod primitives;

fn main() {
    let args = std::env::args();
    for filename in args.skip(1) {
        let contents = std::fs::read_to_string(filename).unwrap();
        let tokens = crate::lexer::tokenize(&contents).unwrap();
        let ast = crate::parser::parse(&tokens).unwrap();
        println!("{:#?}", ast);
    }
}
