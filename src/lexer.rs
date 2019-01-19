use crate::builtins::lex_builtin;
use crate::builtins::Builtin;
use crate::primitives::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Fn,
    Pub,
    Let,
    Mut,
    Return,
    Scalar,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Equals,
    Arrow,
    Dot,
    Identifier(String),
    Builtin(Builtin),
    Primitive(Primitive),
    Literal(Literal),
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    // strip comments
    let comment_re = regex::Regex::new(r"(?m)#.*\n").unwrap();
    let input_stripped = comment_re.replace_all(input, "\n");

    let mut tokens = Vec::new();

    let token_re = regex::Regex::new(concat!(
        r"(?P<identifier>[a-zA-Z]\w*)|",
        r"%(?P<builtin>[a-z]\w*)|",
        r"(?P<litf64>\-?\d+[\.\d*]?)f64|",
        r"(?P<litf32>\-?\d+[\.\d*]?)f32|",
        r"(?P<litu8>\d+)u8|",
        r"(?P<litu16>\d+)u16|",
        r"(?P<litu32>\d+)u32|",
        r"(?P<litu64>\d+)u64|",
        r"(?P<liti8>\-?\d+)i8|",
        r"(?P<liti16>\-?\d+)i16|",
        r"(?P<liti32>\-?\d+)i32|",
        r"(?P<liti64>\-?\d+)i64|",
        r"(?P<litf>\-?\d+\.\d*)|",
        r"(?P<liti>\-?\d+)|",
        r"(?P<semicolon>;)|",
        r"(?P<lparen>\()|",
        r"(?P<rparen>\))|",
        r"(?P<lbrace>\{)|",
        r"(?P<rbrace>\})|",
        r"(?P<comma>,)|",
        r"(?P<colon>:)|",
        r"(?P<equals>=)|",
        r"(?P<arrow>->)|",
        r"(?P<dot>\.)"
    ))
    .unwrap();

    // Check for bad tokens
    for extraneous in token_re.split(&input_stripped) {
        if let Some(bad_token) = extraneous.split_whitespace().nth(0) {
            return Err(format!("Bad token: {}", bad_token));
        }
    }

    // Parse tokens
    for capture in token_re.captures_iter(&input_stripped) {
        let token = if capture.name("identifier").is_some() {
            let identifier = match capture.name("identifier").unwrap().as_str() {
                "fn" => Token::Fn,
                "pub" => Token::Pub,
                "let" => Token::Let,
                "mut" => Token::Mut,
                "return" => Token::Return,
                "scalar" => Token::Scalar,
                "u8" => Token::Primitive(Primitive::Unsigned8),
                "u16" => Token::Primitive(Primitive::Unsigned16),
                "u32" => Token::Primitive(Primitive::Unsigned32),
                "u64" => Token::Primitive(Primitive::Unsigned64),
                "i8" => Token::Primitive(Primitive::Signed8),
                "i16" => Token::Primitive(Primitive::Signed16),
                "i32" => Token::Primitive(Primitive::Signed32),
                "i64" => Token::Primitive(Primitive::Signed64),
                "f32" => Token::Primitive(Primitive::Float32),
                "f64" => Token::Primitive(Primitive::Float64),
                identifier => Token::Identifier(identifier.to_string()),
            };
            Ok(identifier)
        } else if capture.name("litu8").is_some() {
            Ok(Token::Literal(Literal::Unsigned8(
                capture
                    .name("litu8")
                    .unwrap()
                    .as_str()
                    .parse::<u8>()
                    .unwrap(),
            )))
        } else if capture.name("litu16").is_some() {
            Ok(Token::Literal(Literal::Unsigned16(
                capture
                    .name("litu16")
                    .unwrap()
                    .as_str()
                    .parse::<u16>()
                    .unwrap(),
            )))
        } else if capture.name("litu32").is_some() {
            Ok(Token::Literal(Literal::Unsigned32(
                capture
                    .name("litu32")
                    .unwrap()
                    .as_str()
                    .parse::<u32>()
                    .unwrap(),
            )))
        } else if capture.name("litu64").is_some() {
            Ok(Token::Literal(Literal::Unsigned64(
                capture
                    .name("litu64")
                    .unwrap()
                    .as_str()
                    .parse::<u64>()
                    .unwrap(),
            )))
        } else if capture.name("liti8").is_some() {
            Ok(Token::Literal(Literal::Signed8(
                capture
                    .name("liti8")
                    .unwrap()
                    .as_str()
                    .parse::<i8>()
                    .unwrap(),
            )))
        } else if capture.name("liti16").is_some() {
            Ok(Token::Literal(Literal::Signed16(
                capture
                    .name("liti16")
                    .unwrap()
                    .as_str()
                    .parse::<i16>()
                    .unwrap(),
            )))
        } else if capture.name("liti32").is_some() {
            Ok(Token::Literal(Literal::Signed32(
                capture
                    .name("liti32")
                    .unwrap()
                    .as_str()
                    .parse::<i32>()
                    .unwrap(),
            )))
        } else if capture.name("liti64").is_some() {
            Ok(Token::Literal(Literal::Signed64(
                capture
                    .name("liti64")
                    .unwrap()
                    .as_str()
                    .parse::<i64>()
                    .unwrap(),
            )))
        } else if capture.name("litf32").is_some() {
            Ok(Token::Literal(Literal::Float32(
                capture
                    .name("litf32")
                    .unwrap()
                    .as_str()
                    .parse::<f32>()
                    .unwrap(),
            )))
        } else if capture.name("litf64").is_some() {
            Ok(Token::Literal(Literal::Float64(
                capture
                    .name("litf64")
                    .unwrap()
                    .as_str()
                    .parse::<f64>()
                    .unwrap(),
            )))
        } else if capture.name("litf").is_some() {
            Ok(Token::Literal(Literal::Float64(
                capture
                    .name("litf")
                    .unwrap()
                    .as_str()
                    .parse::<f64>()
                    .unwrap(),
            )))
        } else if capture.name("liti").is_some() {
            Ok(Token::Literal(Literal::Signed64(
                capture
                    .name("liti")
                    .unwrap()
                    .as_str()
                    .parse::<i64>()
                    .unwrap(),
            )))
        } else if capture.name("builtin").is_some() {
            Ok(Token::Builtin(
                lex_builtin(capture.name("builtin").unwrap().as_str()).unwrap(),
            ))
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
        } else if capture.name("dot").is_some() {
            Ok(Token::Dot)
        } else {
            Err("Unhandled token")
        };

        tokens.push(token.unwrap());
    }

    Ok(tokens)
}
