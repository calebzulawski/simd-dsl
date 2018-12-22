use crate::lexer::Primitive;
use crate::lexer::Token;

// AST node variants

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
    Literal(LiteralExpression),
    Variable(String),
    Call(CallExpression),
}

// Top level AST nodes

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub signature: Signature,
    pub body: ScopeStatement,
}

// Statements

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
    pub variable: Variable,
    pub initializer: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct AssignmentStatement {
    pub name: String,
    pub expression: Expression,
}

// Expressions

#[derive(PartialEq, Clone, Debug)]
pub enum LiteralExpression {
    NonNegative(u64),
    Negative(i64),
    Float(f64),
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub builtin: bool,
    pub name: String,
    pub args: Vec<Expression>,
}

// Contents

#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub scalar: bool,
    pub type_name: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub mutable: bool,
    pub name: String,
    pub type_name: Type,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Signature {
    pub name: String,
    pub args: Vec<Variable>,
    pub returns: Primitive,
}

// Contains the variables and functions that are in scope

struct ScopeContents {
    pub functions: std::collections::HashMap<String, Signature>,
    pub variables: Vec<std::collections::HashMap<String, Variable>>,
}

impl ScopeContents {
    fn new() -> Self {
        ScopeContents {
            functions: std::collections::HashMap::new(),
            variables: Vec::new(),
        }
    }

    fn add_function(&mut self, signature: Signature) {
        self.functions.insert(signature.name.clone(), signature);
    }

    fn get_function_return(&self, name: &String, args: Vec<Type>) -> Result<Primitive, String> {
        match self.functions.get(name) {
            Some(signature) => {
                if signature.args.len() != args.len() {
                    return Err("wrong number of arguments".to_string());
                }
                for (arg, arg_type) in signature.args.iter().zip(args.iter()) {
                    if &arg.type_name != arg_type {
                        return Err("mismatched type".to_string());
                    }
                }
                Ok(signature.returns.clone())
            }
            None => Err(format!("no function '{}'", name)),
        }
    }

    fn push_scope(&mut self) {
        self.variables.push(std::collections::HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.variables
            .pop()
            .expect("Compiler error: missing scope on pop!");
    }

    fn add_variable(&mut self, variable: Variable) {
        self.variables
            .last_mut()
            .expect("Compiler error: missing scope on add!")
            .insert(variable.name.clone(), variable);
    }

    fn get_variable(&self, name: &String) -> Result<Variable, String> {
        for scope in self.variables.iter().rev() {
            match scope.get(name) {
                Some(var) => return Ok(var.clone()),
                None => continue,
            }
        }
        Err(format!("no variable '{}'", name))
    }
}

// Helpers for checking the contents of tokens

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

// Parse functions

pub fn parse(tokens: &[Token]) -> Result<Vec<TopNode>, String> {
    let mut remaining = tokens.to_vec();
    remaining.reverse();

    let mut ast = Vec::new();

    let mut contents = ScopeContents::new();

    loop {
        let result = match remaining.last().clone() {
            Some(Token::Fn) => parse_function(&mut remaining, &mut contents),
            None => break,
            _ => Err("expected function definition".to_string()),
        }?;
        ast.push(result);
    }

    Ok(ast)
}

fn parse_function(
    mut tokens: &mut Vec<Token>,
    mut contents: &mut ScopeContents,
) -> Result<TopNode, String> {
    let signature = parse_signature(&mut tokens)?;
    let body = parse_scope(&mut tokens, &mut contents)?;
    contents.add_function(signature.clone()); // add function after scope to prevent recursive functions
    Ok(TopNode::Function(Function {
        signature: signature,
        body: body,
    }))
}

fn parse_signature(tokens: &mut Vec<Token>) -> Result<Signature, String> {
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

        let type_name = parse_type(tokens)?.ok_or("expected type in function signature")?;

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

    Ok(Signature {
        name: name,
        args: args,
        returns: returns,
    })
}

fn parse_scope(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<ScopeStatement, String> {
    let mut statements = Vec::new();

    eat_token!([Token::LeftBrace, Ok(())] <= tokens, "expected '{'");

    contents.push_scope();

    loop {
        let statement = check_token!(
            [Token::RightBrace, break;
             Token::LeftBrace, Ok(Statement::Scope(parse_scope(tokens, contents)?));
             Token::Let, parse_let(tokens, contents);
             Token::Identifier(_), parse_assignment(tokens, contents);
             Token::Return, parse_return(tokens, contents)] <= tokens, "expected statement");

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

    contents.pop_scope();

    Ok(ScopeStatement {
        statements: statements,
    })
}

fn parse_return(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<Statement, String> {
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

fn parse_let(tokens: &mut Vec<Token>, contents: &mut ScopeContents) -> Result<Statement, String> {
    eat_token!([Token::Let, Ok(())] <= tokens, "expected 'let'");
    let mutable = eat_optional_token!(Token::Mut, tokens);

    let name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected variable name"
    );

    let type_name = parse_type(tokens)?.ok_or("auto type not yet supported")?;

    eat_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' after variable declaration"
    );
    let initializer = parse_expression(tokens)?;

    let variable = Variable {
        mutable: mutable,
        name: name,
        type_name: type_name,
    };

    contents.add_variable(variable.clone());

    Ok(Statement::Let(LetStatement {
        variable: variable,
        initializer: initializer,
    }))
}

fn parse_expression(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    Ok(
        check_token!([Token::Identifier(_), parse_identifier_expression(tokens);
                      Token::Builtin(_), parse_builtin(tokens);
                      Token::Negative(val), pop_and_pass(tokens, Expression::Literal(LiteralExpression::Negative(*val)));
                      Token::NonNegative(val), pop_and_pass(tokens, Expression::Literal(LiteralExpression::NonNegative(*val)));
                      Token::Float(val), pop_and_pass(tokens, Expression::Literal(LiteralExpression::Float(*val)))] <= tokens,
                      "expected expression"),
    )
}

fn pop_and_pass<T>(tokens: &mut Vec<Token>, val: T) -> Result<T, String> {
    tokens.pop();
    Ok(val)
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

fn parse_assignment(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<Statement, String> {
    let identifier = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected identifier"
    );
    eat_token!([Token::Equals, Ok(())] <= tokens, "expected '='");
    let expression = parse_expression(tokens)?;

    let variable = contents.get_variable(&identifier)?;

    Ok(Statement::Assignment(AssignmentStatement {
        name: identifier,
        expression: expression,
    }))
}
