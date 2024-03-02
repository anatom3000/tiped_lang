#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionData {
    LetIn {
        name: String,
        value: Box<Expression>,
        body: Box<Expression>,
    },
    Fun {
        arg: (String, Option<Type>),
        body: Box<Expression>
    },
    App {
        fun: Box<Expression>,
        arg: Box<Expression>
    },
    StringLiteral(String),
    IntLiteral(i64),
    Variable(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Atom(String),
    Var(usize),
    PolymorphicVar(usize),
    Fun(Box<Type>, Box<Type>),
    Polymorphic {
        variables: Vec<usize>,
        matrix: Box<Type>
    },
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
    Type {
        name: String,
    }
}

use std::fmt::Display;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Atom(name) => write!(f, "{name}"),
            Type::Var(name) => write!(f, "''{name}"),
            Type::PolymorphicVar(name) => write!(f, "'{name}"),
            Type::Fun(from, to) => write!(f, "({from} -> {to})"),
            Type::Polymorphic { variables, matrix } => {
                write!(f, "∀")?;
                for v in variables {
                    write!(f, " '{v}")?;
                }
                write!(f, ", ")?;
                write!(f, "{}", *matrix)
            }
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
            ExpressionData::Fun { arg: (name, ty), body } => {
                write!(f, "fun ")?;
                match ty {
                    None => write!(f, "{name} ")?,
                    Some(ty) => write!(f, "({name}: {ty}) ")?
                }
                write!(f, "-> \n")?;
                write!(f, "({body})")
            }
            ExpressionData::App { fun, arg } =>
                write!(f, "{fun}({arg})"),
            ExpressionData::StringLiteral(content) => 
                write!(f, "\"{content}\""),
            ExpressionData::IntLiteral(value) => 
                write!(f, "{value}"),
            ExpressionData::Variable(name) => 
                write!(f, "{name}"),
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Let { name, value } => write!(f, "let {name} = {value}"),
            Declaration::ExternLet { name, type_ } => write!(f, "let {name} = extern {type_}"),
            Declaration::Type { name } => write!(f, "type {name}"),
        }
    }
}
