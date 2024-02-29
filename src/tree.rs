#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionData {
    LetIn {
        name: String,
        value: Box<Expression>,
        body: Box<Expression>,
    },
    Fun {
        args: Vec<(String, Option<Type>)>,
        body: Box<Expression>
    },
    StringLiteral(String),
    IntLiteral(i64),
    Variable(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Atom(String),
    Var(String),
    Fun(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Option<Type>
}

impl Expression {
    pub fn untyped(data: ExpressionData) -> Expression {
        Expression { data, type_: None } 
    }
}

use std::fmt::Display;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Atom(name) => write!(f, "{name}"),
            Type::Var(name) => write!(f, "'{name}"),
            Type::Fun(from, to) => write!(f, "({from} -> {to})")
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.type_ {
            Some(ty) => write!(f, "({}: {})", self.data, ty),
            None     => write!(f, "{}", self.data),
        }
    }
}

impl Display for ExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionData::LetIn { name, value, body } =>
                write!(f, "let {name} = {value} in \n{body}"),
            ExpressionData::Fun { args, body } => {
                write!(f, "fun ")?;
                for (name, ty) in args {
                    match ty {
                        None => write!(f, "{name} ")?,
                        Some(ty) => write!(f, "({name}: {ty}) ")?
                    }
                }
                write!(f, "-> \n")?;
                write!(f, "({body})")
            }
            ExpressionData::StringLiteral(content) => 
                write!(f, "\"{content}\""),
            ExpressionData::IntLiteral(value) => 
                write!(f, "{value}"),
            ExpressionData::Variable(name) => 
                write!(f, "{name}"),
        }
    }
}
