use crate::builtins::Builtin;
use crate::primitives::*;

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
    TupleAccess(TupleAccessExpression),
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
    pub typed_as: Type,
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

#[derive(PartialEq, Clone, Debug)]
pub struct TupleAccessExpression {
    pub expression: Box<TypedExpression>,
    pub element: u64,
}

// Contents

#[derive(PartialEq, Clone, Debug)]
pub struct SingleType {
    pub scalar: bool,
    pub primitive: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub mutable: bool,
    pub name: String,
    pub typed_as: Type,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Signature {
    pub name: String,
    pub args: Vec<Variable>,
    pub returns: Type,
}
