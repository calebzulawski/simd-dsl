use crate::lexer::Builtin;
use crate::lexer::Literal;
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
    Literal(Literal),
    Variable(String),
    Call(CallExpression),
    BuiltinCall(BuiltinCallExpression),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Single(SingleType),
    Tuple(Vec<Type>),
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
pub struct CallExpression {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct BuiltinCallExpression {
    pub builtin: Builtin,
    pub args: Vec<Expression>,
}

// Contents

#[derive(PartialEq, Clone, Debug)]
pub struct SingleType {
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
    pub returns: Type,
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

    fn get_function_return_type(&self, name: &String, args: Vec<Type>) -> Result<Type, String> {
        match self.functions.get(name) {
            Some(signature) => {
                if signature.args.len() != args.len() {
                    return Err("wrong number of arguments".to_string());
                }
                for (func_arg, arg) in signature.args.iter().zip(args.iter()) {
                    if &func_arg.type_name != arg {
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

// Validates builtin functions

fn get_builtin_return_type(builtin: &Builtin, args: Vec<Type>) -> Result<Type, String> {
    match builtin {
        Builtin::Add => get_builtin_return_type_binary(args),
        Builtin::Sub => get_builtin_return_type_binary(args),
        Builtin::Mul => get_builtin_return_type_binary(args),
        Builtin::Div => get_builtin_return_type_binary(args),
    }
}

fn get_builtin_return_type_binary(args: Vec<Type>) -> Result<Type, String> {
    if args.len() != 2 {
        Err(format!("Got {} arguments, expected 2", args.len()))
    } else if let (Type::Single(a), Type::Single(b)) = (&args[0], &args[1]) {
        if a != b {
            Err("incorrect arguments".to_string())
        } else {
            Ok(Type::Single(a.clone()))
        }
    } else {
        Err("tuple not expected".to_string())
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

fn get_expression_type(expression: &Expression, contents: &ScopeContents) -> Result<Type, String> {
    Ok(match expression {
        Expression::Literal(literal) => {
            let primitive = match literal {
                Literal::Unsigned8(_) => Primitive::Unsigned8,
                Literal::Unsigned16(_) => Primitive::Unsigned16,
                Literal::Unsigned32(_) => Primitive::Unsigned32,
                Literal::Unsigned64(_) => Primitive::Unsigned64,
                Literal::Signed8(_) => Primitive::Signed8,
                Literal::Signed16(_) => Primitive::Signed16,
                Literal::Signed32(_) => Primitive::Signed32,
                Literal::Signed64(_) => Primitive::Signed64,
                Literal::Float32(_) => Primitive::Signed32,
                Literal::Float64(_) => Primitive::Signed64,
            };
            Type::Single(SingleType {
                scalar: true,
                type_name: primitive,
            })
        }
        Expression::Variable(variable) => contents.get_variable(variable)?.type_name,
        Expression::Call(call) => {
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(get_expression_type(&arg, contents)?);
            }
            contents.get_function_return_type(&call.name, args)?
        }
        Expression::BuiltinCall(call) => {
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(get_expression_type(&arg, contents)?);
            }
            get_builtin_return_type(&call.builtin, args)?
        }
    })
}

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
    contents.push_scope();
    for arg in &signature.args {
        contents.add_variable(arg.clone());
    }
    let body = parse_block(&mut tokens, &mut contents)?;
    contents.pop_scope();
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

        let type_name =
            parse_variable_type(tokens)?.ok_or("expected type in function signature")?;

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

    let returns = parse_type(tokens)?;

    Ok(Signature {
        name: name,
        args: args,
        returns: returns,
    })
}

fn parse_scoped_block(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<ScopeStatement, String> {
    contents.push_scope();
    let block = parse_block(tokens, contents);
    contents.pop_scope();
    block
}

fn parse_block(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<ScopeStatement, String> {
    let mut statements = Vec::new();

    eat_token!([Token::LeftBrace, Ok(())] <= tokens, "expected '{'");

    loop {
        let statement = check_token!(
            [Token::RightBrace, break;
             Token::LeftBrace, Ok(Statement::Scope(parse_scoped_block(tokens, contents)?));
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

fn parse_variable_type(tokens: &mut Vec<Token>) -> Result<Option<Type>, String> {
    match tokens.last() {
        Some(Token::Colon) => {
            tokens.pop();
            Ok(Some(parse_type(tokens)?))
        }
        _ => Ok(None),
    }
}

fn parse_type(tokens: &mut Vec<Token>) -> Result<Type, String> {
    if eat_optional_token!(Token::LeftParen, tokens) {
        let mut elements = Vec::new();
        loop {
            elements.push(parse_type(tokens)?);
            eat_token!([Token::RightParen, break;
                    Token::Comma, continue] <= tokens,
                    "expected ',' or ')'");
        }
        Ok(Type::Tuple(elements))
    } else {
        let scalar = eat_optional_token!(Token::Scalar, tokens);

        let type_name = eat_token!(
            [Token::Primitive(primitive), Ok(primitive)] <= tokens,
            "expected type after ':'"
        );

        Ok(Type::Single(SingleType {
            scalar: scalar,
            type_name: type_name,
        }))
    }
}

fn parse_let(tokens: &mut Vec<Token>, contents: &mut ScopeContents) -> Result<Statement, String> {
    eat_token!([Token::Let, Ok(())] <= tokens, "expected 'let'");
    let mutable = eat_optional_token!(Token::Mut, tokens);

    let name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected variable name"
    );

    let optional_type_name = parse_variable_type(tokens)?;

    eat_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' after variable declaration"
    );
    let initializer = parse_expression(tokens)?;

    let expression_type = get_expression_type(&initializer, contents)?;
    let type_name = match optional_type_name {
        Some(provided_type) => {
            if provided_type != expression_type {
                Err("incorrect initializer type".to_string())
            } else {
                Ok(provided_type)
            }
        }
        None => Ok(expression_type),
    }?;

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
                      Token::Literal(_), parse_literal(tokens)] <= tokens,
                      "expected expression"),
    )
}

fn parse_literal(tokens: &mut Vec<Token>) -> Result<Expression, String> {
    Ok(eat_token!(
        [Token::Literal(literal), Ok(Expression::Literal(literal))] <= tokens,
        "expected literal"
    ))
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
        "expected builtin"
    );
    let args = parse_call_args(tokens)?;
    Ok(Expression::BuiltinCall(BuiltinCallExpression {
        builtin: builtin,
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

    if !variable.mutable {
        return Err("cannot assign to immutable variable".to_string());
    }

    if variable.type_name != get_expression_type(&expression, contents)? {
        return Err("assignment type mismatch".to_string());
    }

    Ok(Statement::Assignment(AssignmentStatement {
        name: identifier,
        expression: expression,
    }))
}
