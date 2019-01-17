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
    Assignment(AssignmentStatement),
    Scope(ScopeStatement),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Call(CallExpression),
    BuiltinCall(BuiltinCallExpression),
    Tuple(Vec<TypedExpression>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Single(SingleType),
    Tuple(Vec<Type>),
}

// Top level AST nodes

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub public: bool,
    pub signature: Signature,
    pub body: ScopeStatement,
}

// Statements

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub expression: TypedExpression,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ScopeStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct AssignmentStatement {
    pub variables: Vec<Variable>,
    pub expression: TypedExpression,
    pub initial: bool,
}

// Expressions
#[derive(PartialEq, Clone, Debug)]
pub struct TypedExpression {
    pub expression: Expression,
    pub type_name: Type,
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub name: String,
    pub args: Vec<TypedExpression>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct BuiltinCallExpression {
    pub builtin: Builtin,
    pub args: Vec<TypedExpression>,
}

// Contents

#[derive(PartialEq, Clone, Debug)]
pub struct SingleType {
    pub scalar: bool,
    pub type_name: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct UnresolvedVariable {
    pub mutable: bool,
    pub name: String,
    pub type_name: Option<Type>,
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
    pub current_function: Option<Signature>,
}

impl ScopeContents {
    fn new() -> Self {
        ScopeContents {
            functions: std::collections::HashMap::new(),
            variables: Vec::new(),
            current_function: None,
        }
    }

    fn enter_function(&mut self, signature: Signature) {
        self.current_function.replace(signature);
        self.push_scope();
    }

    fn leave_function(&mut self) {
        self.pop_scope();
        let signature = self
            .current_function
            .take()
            .expect("Compiler error: leave_function");
        self.functions.insert(signature.name.clone(), signature);
    }

    fn get_current_return_type(&self) -> Type {
        self.current_function
            .as_ref()
            .expect("Compiler error: get_current_return_type")
            .returns
            .clone()
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

    fn add_variable(&mut self, variable: Variable) -> Result<(), String> {
        let scope = &mut self
            .variables
            .last_mut()
            .expect("Compiler error: missing scope on add!");
        if scope.contains_key(&variable.name) {
            Err("variable already exists".to_string())
        } else {
            scope.insert(variable.name.clone(), variable);
            Ok(())
        }
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
        if a.type_name != b.type_name {
            Err("incorrect arguments".to_string())
        } else {
            Ok(Type::Single(SingleType {
                scalar: a.scalar && b.scalar,
                type_name: a.type_name.clone(),
            }))
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

// Parse functions

pub fn parse(tokens: &[Token]) -> Result<Vec<TopNode>, String> {
    let mut remaining = tokens.to_vec();
    remaining.reverse();

    let mut ast = Vec::new();

    let mut contents = ScopeContents::new();

    loop {
        let result = match remaining.last().clone() {
            Some(Token::Fn) => parse_function(&mut remaining, &mut contents),
            Some(Token::Pub) => parse_function(&mut remaining, &mut contents),
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
    let public = eat_optional_token!(Token::Pub, tokens);
    let signature = parse_signature(&mut tokens)?;
    contents.enter_function(signature.clone());
    for arg in &signature.args {
        contents.add_variable(arg.clone())?;
    }
    let body = parse_block(&mut tokens, &mut contents)?;
    contents.leave_function();
    Ok(TopNode::Function(Function {
        public: public,
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
             Token::Let, parse_assignment(tokens, contents);
             Token::Identifier(_), parse_assignment(tokens, contents);
             Token::LeftParen, parse_assignment(tokens, contents);
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
    let expression = parse_expression(tokens, contents)?;
    if expression.type_name != contents.get_current_return_type() {
        Err("mismatched return type".to_string())
    } else {
        Ok(Statement::Return(ReturnStatement {
            expression: expression,
        }))
    }
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

fn parse_variable_declaration(tokens: &mut Vec<Token>) -> Result<UnresolvedVariable, String> {
    let mutable = eat_optional_token!(Token::Mut, tokens);

    let name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected variable name"
    );

    let type_name = parse_variable_type(tokens)?;

    Ok(UnresolvedVariable {
        mutable: mutable,
        name: name,
        type_name: type_name,
    })
}

fn parse_variable_assignment(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<UnresolvedVariable, String> {
    let variable_name = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected identifier"
    );
    let variable = contents.get_variable(&variable_name)?;
    Ok(UnresolvedVariable {
        name: variable.name,
        mutable: variable.mutable,
        type_name: Some(variable.type_name),
    })
}

// This handles both let and assignments
fn parse_assignment(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<Statement, String> {
    let initial = eat_optional_token!(Token::Let, tokens); // check if this is a let statement

    // If a let statement, parse variables with optional type declarations
    // If an assignment, just parse the variable names and check if they exist
    let mut maybe_typed_variables = Vec::new();
    if eat_optional_token!(Token::LeftParen, tokens) {
        loop {
            maybe_typed_variables.push(if initial {
                parse_variable_declaration(tokens)?
            } else {
                parse_variable_assignment(tokens, contents)?
            });
            eat_token!([Token::RightParen, break;
                    Token::Comma, continue] <= tokens,
                    "expected ',' or ')'");
        }
    } else {
        maybe_typed_variables.push(if initial {
            parse_variable_declaration(tokens)?
        } else {
            parse_variable_assignment(tokens, contents)?
        });
    }

    // parse the assignment expression
    eat_token!(
        [Token::Equals, Ok(())] <= tokens,
        "expected '=' after variable declaration or assignment"
    );
    let expression = parse_expression(tokens, contents)?;

    // check that the variable types match the expression, handling auto type deduction
    let mut variables = Vec::new();
    if maybe_typed_variables.len() == 1 {
        variables.push(Variable {
            mutable: maybe_typed_variables[0].mutable,
            name: maybe_typed_variables[0].name.clone(),
            type_name: match maybe_typed_variables[0].type_name.clone() {
                Some(provided_type) => {
                    if provided_type != expression.type_name {
                        Err("incorrect initializer type".to_string())
                    } else {
                        Ok(provided_type)
                    }
                }
                None => Ok(expression.type_name.clone()),
            }?,
        });
    } else {
        if let Type::Tuple(tuple) = expression.type_name.clone() {
            if maybe_typed_variables.len() == tuple.len() {
                for (t, var) in tuple.iter().zip(maybe_typed_variables.iter()) {
                    variables.push(Variable {
                        mutable: var.mutable,
                        name: var.name.clone(),
                        type_name: match var.type_name.clone() {
                            Some(provided_type) => {
                                if &provided_type != t {
                                    Err("incorrect initializer type".to_string())
                                } else {
                                    Ok(provided_type)
                                }
                            }
                            None => Ok(t.clone()),
                        }?,
                    });
                }
            } else {
                return Err("Tuple sizes do not match".to_string());
            }
        } else {
            return Err("Expected tuple type".to_string());
        };
    }

    // add the variables to the scope on a let statement
    if initial {
        for variable in &variables {
            contents.add_variable(variable.clone())?;
        }
    }

    Ok(Statement::Assignment(AssignmentStatement {
        variables: variables,
        expression: expression,
        initial: initial,
    }))
}

fn parse_expression(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<TypedExpression, String> {
    Ok(
        check_token!([Token::Identifier(_), parse_identifier_expression(tokens, contents);
                      Token::Builtin(_), parse_builtin(tokens, contents);
                      Token::Literal(_), parse_literal(tokens);
                      Token::LeftParen, parse_tuple_expression(tokens, contents)] <= tokens,
                      "expected expression"),
    )
}

fn parse_expression_group(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<(Vec<TypedExpression>, Vec<Type>), String> {
    eat_token!([Token::LeftParen, Ok(())] <= tokens, "expected '('");
    let mut args = Vec::new();
    loop {
        if eat_optional_token!(Token::RightParen, tokens) {
            break;
        }
        let arg = parse_expression(tokens, contents)?;
        args.push(arg);
        eat_token!([Token::RightParen, break;
                    Token::Comma, continue] <= tokens,
                    "expected ',' or ')'");
    }
    let types = args.iter().map(|x| x.type_name.clone()).collect();
    Ok((args, types))
}

fn parse_tuple_expression(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<TypedExpression, String> {
    let (expressions, types) = parse_expression_group(tokens, contents)?;
    Ok(TypedExpression {
        expression: Expression::Tuple(expressions),
        type_name: Type::Tuple(types),
    })
}

fn parse_literal(tokens: &mut Vec<Token>) -> Result<TypedExpression, String> {
    let literal = eat_token!(
        [Token::Literal(literal), Ok(literal)] <= tokens,
        "expected literal"
    );
    let type_name = match literal {
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
    Ok(TypedExpression {
        expression: Expression::Literal(literal),
        type_name: Type::Single(SingleType {
            scalar: true,
            type_name: type_name,
        }),
    })
}

fn parse_builtin(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<TypedExpression, String> {
    let builtin = eat_token!(
        [Token::Builtin(builtin), Ok(builtin)] <= tokens,
        "expected builtin"
    );
    let (args, types) = parse_expression_group(tokens, contents)?;
    let type_name = get_builtin_return_type(&builtin, types)?;
    Ok(TypedExpression {
        expression: Expression::BuiltinCall(BuiltinCallExpression {
            builtin: builtin,
            args: args,
        }),
        type_name: type_name,
    })
}

fn parse_identifier_expression(
    tokens: &mut Vec<Token>,
    contents: &mut ScopeContents,
) -> Result<TypedExpression, String> {
    let identifier = eat_token!(
        [Token::Identifier(identifier), Ok(identifier)] <= tokens,
        "expected identifier"
    );
    match tokens.last() {
        Some(Token::LeftParen) => {
            let (args, types) = parse_expression_group(tokens, contents)?;
            let type_name = contents.get_function_return_type(&identifier, types)?;
            Ok(TypedExpression {
                expression: Expression::Call(CallExpression {
                    name: identifier,
                    args: args,
                }),
                type_name: type_name,
            })
        }
        _ => {
            let type_name = contents.get_variable(&identifier)?.type_name;
            Ok(TypedExpression {
                expression: Expression::Variable(identifier),
                type_name: type_name,
            })
        }
    }
}
