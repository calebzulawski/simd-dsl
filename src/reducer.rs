use crate::builtins::Builtin;
use crate::primitives::*;
use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
enum BoundVariable {
    Tuple(Vec<BoundVariable>),
    Single(String),
}

#[derive(PartialEq, Clone, Debug)]
struct ScopeContents {
    variables: Vec<HashMap<String, BoundVariable>>,
    bound: Vec<crate::reduced_ast::Variable>,
    counter: u64,
}

impl ScopeContents {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            bound: Vec::new(),
            counter: 0,
        }
    }

    pub fn push_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.variables
            .pop()
            .expect("Compiler error: missing scope on pop!");
    }

    fn obtain_next_name(&mut self) -> String {
        let name = format!("_v{}", self.counter);
        self.counter += 1;
        name
    }

    pub fn get_variables(&self) -> Vec<crate::reduced_ast::Variable> {
        self.bound.clone()
    }

    fn get_bound_name(&mut self, typed_as: &crate::full_ast::Type) -> BoundVariable {
        match typed_as {
            crate::full_ast::Type::Single(single) => {
                let name = self.obtain_next_name();
                self.bound.push(crate::reduced_ast::Variable {
                    name: name.clone(),
                    typed_as: crate::reduced_ast::Type {
                        scalar: single.scalar,
                        primitive: single.primitive.clone(),
                    },
                });
                BoundVariable::Single(name)
            }
            crate::full_ast::Type::Tuple(tuple) => BoundVariable::Tuple(
                tuple
                    .iter()
                    .map(|x| self.get_bound_name(x))
                    .collect::<Vec<BoundVariable>>(),
            ),
        }
    }

    fn get_scope_mut(&mut self) -> &mut HashMap<String, BoundVariable> {
        self.variables
            .last_mut()
            .expect("Compiler error: missing scope on get!")
    }

    fn get_scope(&self) -> &HashMap<String, BoundVariable> {
        self.variables
            .last()
            .expect("Compiler error: missing scope on get!")
    }

    pub fn add_variable(&mut self, variable: crate::full_ast::Variable) -> BoundVariable {
        if self.get_scope().contains_key(&variable.name) {
            panic!("Compiler error: scope already contains variable");
        } else {
            let bound = self.get_bound_name(&variable.typed_as);
            self.get_scope_mut().insert(variable.name, bound.clone());
            return bound;
        }
    }

    pub fn get_variable(&mut self, variable: crate::full_ast::Variable) -> BoundVariable {
        let scope = self
            .variables
            .last()
            .expect("Compiler error: missing scope on get!");
        scope
            .get(&variable.name)
            .expect("Compiler error: variable not in scope")
            .clone()
    }
}

pub fn reduce(ast: Vec<crate::full_ast::TopNode>) -> Result<crate::reduced_ast::Program, String> {
    let mut functions = HashMap::new();
    let mut reduced_functions = Vec::new();
    for node in ast {
        match node {
            crate::full_ast::TopNode::Function(function) => {
                if let Some(f) = reduce_function(function.clone(), &mut functions)? {
                    reduced_functions.push(f);
                }
            }
        }
    }
    Ok(crate::reduced_ast::Program {
        functions: reduced_functions,
    })
}

fn reduce_function(
    function: crate::full_ast::Function,
    functions: &mut HashMap<String, crate::full_ast::Function>,
) -> Result<Option<crate::reduced_ast::Function>, String> {
    match &function.signature {
        crate::full_ast::Signature::PublicSignature(s) => {
            let mut contents = ScopeContents::new();
            contents.push_scope();
            let get_parameters = |parameters: &Vec<crate::full_ast::Variable>| {
                parameters
                    .iter()
                    .map(|x| match &x.typed_as {
                        crate::full_ast::Type::Single(typed_as) => {
                            Ok(crate::reduced_ast::Variable {
                                typed_as: crate::reduced_ast::Type {
                                    scalar: typed_as.scalar,
                                    primitive: typed_as.primitive.clone(),
                                },
                                name: x.name.clone(),
                            })
                        }
                        crate::full_ast::Type::Tuple(_) => {
                            Err("Compiler error: should not have allowed tuple parameter!"
                                .to_string())
                        }
                    })
                    .collect::<Result<Vec<crate::reduced_ast::Variable>, String>>()
            };
            let mut scalar_body = Vec::new();
            let mut loop_body = Vec::new();
            for arg in &s.args {
                if let (BoundVariable::Single(name), crate::full_ast::Type::Single(typed_as)) =
                    (contents.add_variable(arg.clone()), arg.typed_as.clone())
                {
                    if typed_as.scalar {
                        scalar_body.push(crate::reduced_ast::Statement::Assignment(
                            crate::reduced_ast::Assignment {
                                source: arg.name.clone(),
                                destination: name.clone(),
                            },
                        ));
                    } else {
                        loop_body.push(crate::reduced_ast::Statement::Load(
                            crate::reduced_ast::Load {
                                source: arg.name.clone(),
                                destination: name.clone(),
                                primitive: typed_as.primitive.clone(),
                            },
                        ));
                    }
                } else {
                    return Err("unexpected tuple argument".to_string());
                }
            }
            let mut reduced_function = crate::reduced_ast::Function {
                name: s.name.clone(),
                args: get_parameters(&s.args)?,
                returns: get_parameters(&s.returns)?,
                variables: Vec::new(),
                scalar_body: scalar_body,
                loop_body: loop_body,
            };
            // TODO: process body here
            reduced_function.variables = contents.get_variables();
            functions.insert(s.name.clone(), function);
            contents.pop_scope();
            Ok(Some(reduced_function))
        }
        crate::full_ast::Signature::PrivateSignature(s) => {
            functions.insert(s.name.clone(), function);
            Ok(None)
        }
    }
}
