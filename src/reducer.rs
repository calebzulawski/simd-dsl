use crate::builtins::Builtin;
use crate::primitives::*;
use std::collections::HashMap;

pub struct ScopeContents {
    pub variables: Vec<HashMap<String, String>>,
}

impl ScopeContents {}

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
            let get_parameters = |parameters: &Vec<crate::full_ast::Variable>| {
                parameters
                    .iter()
                    .map(|x| match &x.typed_as {
                        crate::full_ast::Type::Single(typed_as) => {
                            Ok(crate::reduced_ast::Variable {
                                scalar: typed_as.scalar,
                                primitive: typed_as.primitive.clone(),
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
            let result = Ok(Some(crate::reduced_ast::Function {
                name: s.name.clone(),
                args: get_parameters(&s.args)?,
                returns: get_parameters(&s.returns)?,
                scalar_body: Vec::new(),
                loop_body: Vec::new(),
            }));
            functions.insert(s.name.clone(), function);
            result
        }
        crate::full_ast::Signature::PrivateSignature(s) => {
            functions.insert(s.name.clone(), function);
            Ok(None)
        }
    }
}
