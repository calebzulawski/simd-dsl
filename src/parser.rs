use crate::lexer::Primitive;
use crate::lexer::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode {
    Function(Function),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Return(ReturnStatement),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Literal(String),
    Variable(String),
    Call(CallExpression),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub type_name: Primitive,
    pub name: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<Variable>,
    pub returns: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub expression: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
}

macro_rules! expect_token (
    ([ $($token:pat, $result:expr);+ ] <= $tokens:ident, $error:expr) => (
        match $tokens.pop() {
            $(
            Some($token) => $result,
            )+
            None => Err(concat!("Ran out of tokens: ", $error).to_string()),
            _ => Err($error.to_string())
        }?
    );
);

pub fn parse(tokens: &[Token]) -> Result<Vec<ASTNode>, String> {
    let mut remaining = tokens.to_vec();
    remaining.reverse();

    let mut ast = Vec::new();

    loop {
        let result = match remaining.pop() {
            Some(Token::Fn) => parse_function(&mut remaining),
            None => break,
            _ => Err("expected function definition".to_string()),
        }?;
        ast.push(result);
    }

    Ok(ast)
}

pub fn parse_function(mut tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    let prototype = parse_prototype(&mut tokens)?;
    let body = parse_scope(&mut tokens)?;
    Ok(ASTNode::Function(Function {
        prototype: prototype,
        body: body,
    }))
}

pub fn parse_prototype(tokens: &mut Vec<Token>) -> Result<Prototype, String> {
    let name = expect_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected function name"
    );

    expect_token!(
        [Token::LeftParen, Ok(())] <= tokens,
        "expected ( after function name"
    );

    let mut args = Vec::new();
    loop {
        let name = expect_token!(
            [Token::Identifier(identifier), Ok(identifier)] <= tokens,
            "expected function argument name"
        );

        expect_token!(
            [Token::Colon, Ok(())] <= tokens,
            "expected : after argument name"
        );

        let type_name = expect_token!(
            [Token::Primitive(primitive), Ok(primitive)] <= tokens,
            "expected argument type"
        );

        args.push(Variable {
            name: name,
            type_name: type_name,
        });

        expect_token!([Token::Comma, Ok(()); Token::RightParen, break] <= tokens, "expected ',' or ')' after function parameters");
    }

    expect_token!([Token::Arrow, Ok(())] <= tokens, "expected '->'");

    let returns = expect_token!(
        [Token::Primitive(primitive), Ok(primitive)] <= tokens,
        "expected type"
    );

    Ok(Prototype {
        name: name,
        args: args,
        returns: returns,
    })
}

pub fn parse_scope(tokens: &mut Vec<Token>) -> Result<Vec<Statement>, String> {
    expect_token!(
        [Token::LeftBrace, Ok(())] <= tokens,
        "expected { before function body"
    );

    let statements = Vec::new();

    loop {
        if tokens.last() == Some(&Token::RightBrace) {
            tokens.pop();
            break;
        } else if tokens.last() == None {
            return Err("ran out of tokens".to_string());
        } else {
            // statements.push(parse_statement(tokens)?);
            tokens.pop(); // ignore all statements for now
        }
    }

    Ok(statements)
}
