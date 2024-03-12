#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    LetIn {
        name: String,
        value: Box<Expression>,
        body: Box<Expression>,
    },
    Fun {
        arg: String, 
        body: Box<Expression>,
    },
    App {
        fun: Box<Expression>,
        arg: Box<Expression>,
    },
    StringLiteral(String),
    IntLiteral(i64),
    Variable(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Atom(String),
    Var(usize),
    Fun(Box<Type>, Box<Type>),
    Polymorphic {
        variables: Vec<usize>,
        matrix: Box<Type>,
    },
    PolymorphicVar(usize),
    ParameterizedAtom {
        name: String,
        parameters: Vec<Type>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Let {
        name: String,
        value: Expression,
    },
    ExternLet {
        name: String,
        type_: Type,
    },
    UnboundExpr(Expression),
    Type {
        name: String,
        parameter_count: usize,
    },
}

use std::fmt::Display;

fn type_variable_id_to_name(id: usize) -> String {
    let id = id - 1;

    let big_index = id / 26;
    let small_index = (id % 26) as u8;
    let c = (b'a' + small_index) as char;

    if big_index == 0 {
        format!("{c}")
    } else {
        format!("{c}{big_index}")
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Atom(name) => write!(f, "{name}"),
            Type::Var(name) => write!(f, "^{name}"),
            Type::PolymorphicVar(name) => write!(f, "'{}", type_variable_id_to_name(*name)),
            Type::Fun(from, to) => match **from {
                Type::Fun(_, _) => write!(f, "({from}) -> {to}"),
                _ => write!(f, "{from} -> {to}"),
            },
            Type::ParameterizedAtom { name, parameters } => {
                for p in parameters {
                    write!(f, "{p} ")?;
                }
                write!(f, "{name}")
            }
            Type::Polymorphic { variables, matrix } => {
                write!(f, "∀")?;
                for v in variables {
                    write!(f, " '{}", type_variable_id_to_name(*v))?;
                }
                write!(f, ", ")?;
                write!(f, "{}", *matrix)
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::LetIn { name, value, body } => {
                write!(f, "let {name} = {value} in \n{body}")
            }
            Expression::Fun {
                arg: name,
                body,
            } => write!(f, "fun {name} -> {body}"),
            Expression::App { fun, arg } => write!(f, "{fun}({arg})"),
            Expression::StringLiteral(content) => write!(f, "\"{content}\""),
            Expression::IntLiteral(value) => write!(f, "{value}"),
            Expression::Variable(name) => write!(f, "{name}"),
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Let { name, value } => write!(f, "let {name} = {value}"),
            Declaration::ExternLet { name, type_ } => write!(f, "let {name} = extern {type_}"),
            Declaration::UnboundExpr(expr) => write!(f, "{expr}"),
            Declaration::Type {
                name,
                parameter_count,
            } => {
                write!(f, "type ")?;
                for i in 1..=*parameter_count {
                    write!(f, "{} ", type_variable_id_to_name(i))?;
                }

                write!(f, "{name}")
            }
        }
    }
}
