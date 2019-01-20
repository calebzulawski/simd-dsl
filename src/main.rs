mod builtins;
mod full_ast;
mod lexer;
mod parser;
mod primitives;
mod reduced_ast;
mod reducer;

fn main() {
    let args = std::env::args();
    for filename in args.skip(1) {
        let contents = std::fs::read_to_string(filename).unwrap();
        let tokens = crate::lexer::tokenize(&contents).unwrap();
        let ast = crate::parser::parse(&tokens).unwrap();
        let reduced_ast = crate::reducer::reduce(ast).unwrap();
        println!("{:#?}", reduced_ast);
    }
}
