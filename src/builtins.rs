use crate::primitives::Primitive;

fn all_binary(_builtin: Builtin, args: Vec<Primitive>) -> Result<Primitive, String> {
    if args.len() != 2 {
        Err(format!("expected 2 arguments, got {}", args.len()))
    } else if args[0] != args[1] {
        Err("incorrect argument types".to_string())
    } else {
        Ok(args[0].clone())
    }
}

macro_rules! define_builtins (
    ($($name:ident, $checker:ident);+) => (
        #[derive(PartialEq, Clone, Debug)]
        pub enum Builtin {
            $($name,)+
        }

        pub fn lex_builtin(name: &str) -> Result<Builtin, String> {
            // Uppercase the first letter to match the enum
            let mut uppercase = name.to_string();
            if let Some(c) = uppercase.get_mut(0..1) {
                c.make_ascii_uppercase();
            } else {
                return Err("empty builtin name!".to_string())
            }
            match uppercase.as_ref() {
                $(stringify!($name) => Ok(Builtin::$name),)+
                other => Err(format!("unrecognized builtin '{}'", other)),
            }
        }

        pub fn get_builtin_return_type(builtin: Builtin, args: Vec<Primitive>) -> Result<Primitive, String> {
            match builtin {
                $(Builtin::$name => $checker(builtin, args),)+
            }
        }

    )
);

define_builtins!(
    Add, all_binary;
    Sub, all_binary;
    Mul, all_binary;
    Div, all_binary
);
