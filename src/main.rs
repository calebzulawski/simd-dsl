mod lexer;

fn main() {
    let args = std::env::args();
    for filename in args.skip(1) {
        let contents = std::fs::read_to_string(filename).unwrap();
        let tokens = crate::lexer::tokenize(&contents);
        for token in tokens {
            println!("{:#?}", token);
        }
    }
}
