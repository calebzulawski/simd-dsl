use crate::builtins::Builtin;
use crate::primitives::*;

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub scalar: bool,
    pub primitive: Primitive,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub typed_as: Type,
    pub name: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Variable>,
    pub variables: Vec<Variable>,
    pub returns: Vec<Variable>,
    pub scalar_body: Vec<Statement>,
    pub loop_body: Vec<Statement>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Assignment(Assignment),
    Broadcast(Broadcast),
    Load(Load),
    Store(Store),
    BuiltinUniformType(BuiltinCallUniformType),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Assignment {
    pub source: String,
    pub destination: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Broadcast {
    pub primitive: Primitive,
    pub source: String,
    pub destination: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Load {
    pub primitive: Primitive,
    pub source: String,
    pub destination: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Store {
    pub primitive: Primitive,
    pub source: String,
    pub destination: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct BuiltinCallUniformType {
    pub typed_as: Type,
    pub variable: String,
    pub args: Vec<String>,
    pub builtin: Builtin,
}
