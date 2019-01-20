use crate::builtins::Builtin;
use crate::primitives::*;

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Variable {
    pub scalar: bool,
    pub primitive: Primitive,
    pub name: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Variable>,
    pub returns: Vec<Variable>,
    pub scalar_body: Vec<Statement>,
    pub loop_body: Vec<Statement>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Broadcast(Broadcast),
    BuiltinUniformType(BuiltinCallUniformType),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Broadcast {
    pub initial: bool,
    pub primitive: Primitive,
    pub new_variable: String,
    pub old_variable: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct BuiltinCallUniformType {
    pub initial: bool,
    pub scalar: bool,
    pub primitive: Primitive,
    pub variable: String,
    pub args: Vec<String>,
    pub builtin: Builtin,
}
