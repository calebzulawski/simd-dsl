use crate::lexer::Primitive;
use crate::lexer::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum TopNode {
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
    Variable(String),
    Call(CallExpression),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub scalar: bool,
    pub type_name: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub mutable: bool,
    pub name: String,
    pub type_name: Option<Type>,
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
    pub mutable: bool,
    pub name: String,
    pub type_name: Option<Type>,
    pub initializer: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct AssignmentStatement {
    pub name: String,
    pub expression: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub builtin: bool,
    pub name: String,
    pub args: Vec<Expression>,
}

macro_rules! check_token (
    ([ $($token:pat, $result:expr);+ ] <= $tokens:ident, $error:expr) => (
        match $tokens.last().clone() {
            $(
            Some($token) => $result,
            )+
            None => Err(concat!("Ran out of tokens: ", $error).to_string()),
            _ => Err($error.to_string())
        }?
    );
    ([ $($token:pat, $result:expr);+ ] else $notmatched:block <= $tokens:ident, $error:expr) => (
        match $tokens.last().clone() {
            $(
            Some($token) => $result,
            )+
            None => Err(concat!("Ran out of tokens: ", $error).to_string()),
            _ => {$notmatched}
        }?
    )
);

macro_rules! eat_token (
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

macro_rules! eat_optional_token (
    ($token:pat, $tokens:ident) => (
        match $tokens.last() {
            Some($token) => {
                $tokens.pop();
                true
            }
            _ => false
        }
    )
);

pub fn parse(tokens: &[Token]) -> Result<Vec<TopNode>, String> {
    let mut remaining = tokens.to_vec();
    remaining.reverse();

    let mut ast = Vec::new();

    loop {
        let result = match remaining.last().clone() {
            Some(Token::Fn) => parse_function(&mut remaining),
            None => break,
            _ => Err("expected function definition".to_string()),
        }?;
        ast.push(result);
    }

    Ok(ast)
}

fn parse_function(mut tokens: &mut Vec<Token>) -> Result<TopNode, String> {
    let prototype = parse_prototype(&mut tokens)?;
    let body = parse_scope(&mut tokens)?;
    Ok(TopNode::Function(Function {
        prototype: prototype,
        body: body,
    }))
}

fn parse_prototype(tokens: &mut Vec<Token>) -> Result<Prototype, String> {
    eat_token!([Token::Fn, Ok(())] <= tokens, "expected 'fn'");

    let name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected function name"
    );

    eat_token!([Token::LeftParen, Ok(())] <= tokens, "expected '('");

    let mut args = Vec::new();
    loop {
        let mutable = eat_optional_token!(Token::Mut, tokens);

        let name = eat_token!(
            [Token::Identifier(identifier), Ok(identifier)] <= tokens,
            "expected function argument name"
        );

        let type_name = Some(parse_type(tokens)?.ok_or("expected type in function prototype")?);

        args.push(Variable {
            mutable: mutable,
            name: name,
            type_name: type_name,
        });

        eat_token!([Token::Comma, Ok(());
                    Token::RightParen, break] <= tokens,
                    "expected ',' or ')'");
    }

    eat_token!([Token::Arrow, Ok(())] <= tokens, "expected '->'");

    let returns = eat_token!(
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

    eat_token!([Token::LeftBrace, Ok(())] <= tokens, "expected '{'");

    loop {
        let statement = check_token!(
            [Token::RightBrace, break;
             Token::LeftBrace, Ok(Statement::Scope(parse_scope(tokens)?));
             Token::Let, parse_let(tokens);
             Token::Identifier(identifier), parse_assignment(tokens);
             Token::Return, parse_return(tokens)] <= tokens, "expected statement");

        // Eat semicolon on non-scope statements
        match statement {
            Statement::Scope(_) => (),
            _ => {
                eat_token!([Token::Semicolon, Ok(())] <= tokens, "expected ';'");
                ()
            }
        }

        statements.push(statement);
    }

    eat_token!([Token::RightBrace, Ok(())] <= tokens, "expected '}'");

    Ok(ScopeStatement {
        statements: statements,
    })
}

fn parse_return(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    eat_token!([Token::Return, Ok(())] <= tokens, "expected 'return'");
    let expression = parse_expression(tokens)?;
    Ok(Statement::Return(ReturnStatement {
        expression: expression,
    }))
}

fn parse_type(tokens: &mut Vec<Token>) -> Result<Option<Type>, String> {
    match tokens.last() {
        Some(Token::Colon) => {
            tokens.pop();

            let scalar = eat_optional_token!(Token::Scalar, tokens);

            let type_name = eat_token!(
                [Token::Primitive(primitive), Ok(primitive)] <= tokens,
                "expected type after ':'"
            );

            Ok(Some(Type {
                scalar: scalar,
                type_name: type_name,
            }))
        }
        _ => Ok(None),
    }
}

fn parse_let(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    eat_token!([Token::Let, Ok(())] <= tokens, "expected 'let'");
    let mutable = eat_optional_token!(Token::Mut, tokens);

    let name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected variable name"
    );

    let type_name = parse_type(tokens)?;

    eat_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' after variable declaration"
    );
    let initializer = parse_expression(tokens)?;

    Ok(Statement::Let(LetStatement {
        mutable: mutable,
        name: name,
        type_name: type_name,
        initializer: initializer,
    }))
}

fn parse_expression(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    Ok(
        check_token!([Token::Identifier(_), parse_identifier_expression(tokens);
                      Token::Builtin(_), parse_builtin(tokens);
                      Token::Number(_), parse_literal(tokens)] <= tokens,
                      "expected expression"),
    )
}

fn parse_call_args(tokens: &mut Vec<Token>) -> Result<Vec<Expression>, String> {
    eat_token!([Token::LeftParen, Ok(())] <= tokens, "expected '('");
    let mut args = Vec::new();
    loop {
        if eat_optional_token!(Token::RightParen, tokens) {
            break;
        }
        let arg = parse_expression(tokens)?;
        args.push(arg);
        eat_token!([Token::RightParen, break;
                    Token::Comma, continue] <= tokens,
                    "expected ',' or ')'");
    }
    Ok(args)
}

fn parse_builtin(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let builtin = eat_token!(
        [Token::Builtin(builtin), Ok(builtin)] <= tokens,
        "expected builtin name"
    );
    let args = parse_call_args(tokens)?;
    Ok(Expression::Call(CallExpression {
        builtin: true,
        name: builtin,
        args: args,
    }))
}

fn parse_identifier_expression(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    let identifier = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected identifier"
    );
    match tokens.last() {
        Some(Token::LeftParen) => {
            let args = parse_call_args(tokens)?;
            Ok(Expression::Call(CallExpression {
                builtin: false,
                name: identifier,
                args: args,
            }))
        }
        _ => Ok(Expression::Variable(identifier)),
    }
}

fn parse_assignment(tokens: &mut Vec<Token>) -> Result<Statement, String> {
    let identifier = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected identifier"
    );
    eat_token!([Token::Equals, Ok(())] <= tokens, "expected '='");
    let expression = parse_expression(tokens)?;

    Ok(Statement::Assignment(AssignmentStatement {
        name: identifier,
        expression: expression,
    }))
}

fn parse_literal(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    Ok(eat_token!(
        [Token::Number(number), Ok(Expression::Literal(number))] <= tokens,
        "expected number"
    ))
}
