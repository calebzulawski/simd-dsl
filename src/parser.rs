use crate::lexer::Primitive;
use crate::lexer::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode {
    Function(Function),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Return(ReturnStatement),
    Let(LetStatement),
    Assignment(AssignmentStatement),
    Scope(ScopeStatement),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Literal(String),
    Identifier(String),
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
    pub body: ScopeStatement,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub expression: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ScopeStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: String,
    pub mutable: bool,
    pub type_name: Option<Primitive>,
    pub initializer: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct AssignmentStatement {
    pub name: String,
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
    ([ $($token:pat, $result:expr);+ ] else $notmatched:block <= $tokens:ident, $error:expr) => (
        match $tokens.pop() {
            $(
            Some($token) => $result,
            )+
            None => Err(concat!("Ran out of tokens: ", $error).to_string()),
            _ => {$notmatched}
        }?
    )
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

fn parse_function(mut tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    let prototype = parse_prototype(&mut tokens)?;

    expect_token!(
        [Token::LeftBrace, Ok(())] <= tokens,
        "expected { before function body"
    );

    let body = parse_scope(&mut tokens)?;
    Ok(ASTNode::Function(Function {
        prototype: prototype,
        body: body,
    }))
}

fn parse_prototype(tokens: &mut Vec<Token>) -> Result<Prototype, String> {
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

        expect_token!([Token::Comma, Ok(());
                      Token::RightParen, break] <= tokens,
                      "expected ',' or ')' after function parameters");
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

fn parse_scope(tokens: &mut Vec<Token>) -> Result<ScopeStatement, String> {
    let mut statements = Vec::new();
    loop {
        let statement = expect_token!(
            [Token::RightBrace, break;
            Token::LeftBrace, Ok(Statement::Scope(parse_scope(tokens)?));
            Token::Let, parse_let(tokens);
            Token::Identifier(identifier), parse_identifier_statement(tokens, identifier);
            Token::Return, parse_return(tokens)] <= tokens, "expected statement");

        // Eat semicolon on non-scope statements
        match statement {
            Statement::Scope(_) => (),
            _ => {
                expect_token!(
                    [Token::Semicolon, Ok(())] <= tokens,
                    "expected semicolon at end of statement"
                );
                ()
            }
        }

        statements.push(statement);
    }

    Ok(ScopeStatement {
        statements: statements,
    })
}

fn parse_return(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    let expression = parse_expression(tokens)?;
    Ok(Statement::Return(ReturnStatement {
        expression: expression,
    }))
}

fn parse_let(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    let mutable = match tokens.last() {
        Some(Token::Mut) => {
            tokens.pop();
            true
        }
        _ => false,
    };

    let name = expect_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected variable name"
    );

    let type_name = match tokens.last() {
        Some(Token::Colon) => {
            tokens.pop();
            Some(expect_token!(
                [Token::Primitive(primitive), Ok(primitive)] <= tokens,
                "expected type after ':'"
            ))
        }
        _ => None,
    };

    expect_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' after variable declaration"
    );
    let initializer = parse_expression(tokens)?;

    Ok(Statement::Let(LetStatement {
        name: name,
        type_name: type_name,
        mutable: mutable,
        initializer: initializer,
    }))
}

fn parse_expression(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    Ok(
        expect_token!([Token::Identifier(identifier), parse_identifier_expression(tokens, identifier);
                  Token::Number(number), Ok(Expression::Literal(number))] <= tokens,
                  "expected expression"),
    )
}

fn parse_identifier_expression(
    tokens: &mut Vec<Token>,
    identifier: String,
) -> Result<Expression, String> {
    match tokens.last() {
        Some(Token::LeftParen) => {
            tokens.pop(); // eat the paren
            let mut args = Vec::new();
            loop {
                if tokens.last() == Some(&Token::RightParen) {
                    tokens.pop();
                    break;
                }
                let arg = parse_expression(tokens)?;
                args.push(arg);
                expect_token!([Token::RightParen, break;
                              Token::Comma, continue] <= tokens,
                              "expected ')' or ','");
            }
            Ok(Expression::Call(CallExpression {
                name: identifier,
                args: args,
            }))
        }
        _ => Ok(Expression::Identifier(identifier)),
    }
}

fn parse_identifier_statement(
    tokens: &mut Vec<Token>,
    identifier: String,
) -> Result<Statement, String> {
    // assignment
    expect_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' in assignment"
    );
    let expression = parse_expression(tokens)?;

    Ok(Statement::Assignment(AssignmentStatement {
        name: identifier,
        expression: expression,
    }))
}
