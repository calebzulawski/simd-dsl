#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Unsigned8,
    Unsigned16,
    Unsigned32,
    Unsigned64,
    Signed8,
    Signed16,
    Signed32,
    Signed64,
    Float32,
    Float64,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Fn,
    Let,
    Return,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Equals,
    Arrow,
    Identifier(String),
    Type(Type),
}

pub fn tokenize(input: &str) -> Vec<Token> {
    // strip comments
    let comment_re = regex::Regex::new(r"(?m)#.*\n").unwrap();
    let input_stripped = comment_re.replace_all(input, "\n");

    let mut tokens = Vec::new();

    let token_re = regex::Regex::new(concat!(
        r"(?P<identifier>\p{Alphabetic}\w*)|",
        r"(?P<semicolon>;)|",
        r"(?P<lparen>\()|",
        r"(?P<rparen>\))|",
        r"(?P<lbrace>\{)|",
        r"(?P<rbrace>\})|",
        r"(?P<comma>,)|",
        r"(?P<colon>:)|",
        r"(?P<equals>=)|",
        r"(?P<arrow>->)"
    ))
    .unwrap();

    for capture in token_re.captures_iter(&input_stripped) {
        let token = if capture.name("identifier").is_some() {
            let identifier = match capture.name("identifier").unwrap().as_str() {
                "fn" => Token::Fn,
                "let" => Token::Let,
                "return" => Token::Return,
                "u8" => Token::Type(Type::Unsigned8),
                "u16" => Token::Type(Type::Unsigned16),
                "u32" => Token::Type(Type::Unsigned16),
                "u64" => Token::Type(Type::Unsigned16),
                "i8" => Token::Type(Type::Signed8),
                "i16" => Token::Type(Type::Signed16),
                "i32" => Token::Type(Type::Signed16),
                "i64" => Token::Type(Type::Signed16),
                "f32" => Token::Type(Type::Float32),
                "f64" => Token::Type(Type::Float64),
                identifier => Token::Identifier(identifier.to_string()),
            };
            Ok(identifier)
        } else if capture.name("semicolon").is_some() {
            Ok(Token::Semicolon)
        } else if capture.name("lparen").is_some() {
            Ok(Token::LeftParen)
        } else if capture.name("rparen").is_some() {
            Ok(Token::RightParen)
        } else if capture.name("lbrace").is_some() {
            Ok(Token::LeftBrace)
        } else if capture.name("rbrace").is_some() {
            Ok(Token::RightBrace)
        } else if capture.name("comma").is_some() {
            Ok(Token::Comma)
        } else if capture.name("colon").is_some() {
            Ok(Token::Colon)
        } else if capture.name("equals").is_some() {
            Ok(Token::Equals)
        } else if capture.name("arrow").is_some() {
            Ok(Token::Arrow)
        } else {
            Err("Unrecognized token")
        };

        tokens.push(token.unwrap());
    }

    tokens
}
