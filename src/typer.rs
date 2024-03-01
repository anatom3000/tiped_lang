use std::collections::HashMap;

use crate::tree::{Type, Expression, ExpressionData};

fn occurs_in(variable: usize, term: &Type) -> bool {
    match term {
        Type::Var(name)     => &variable == name,
        Type::Atom(_)       => false,
        Type::Fun(from, to) => occurs_in(variable, &from)
                            || occurs_in(variable, &to),
    }
}

fn substitute(old: usize, new: &Type, expr: &mut Type) {
    match expr {
        Type::Var(v) if v == &old => *expr = new.clone(),
        Type::Fun(from, to) => {
            substitute(old, new, from);
            substitute(old, new, to);
        },
        | Type::Atom(_)
        | Type::Var(_) => (),
    }
}

fn unify(mut constraints: Vec<(Type, Type)>, type_: &mut Type) {
    use Type::*;

    while let Some(c) = constraints.pop() {
        match c {
            (Atom(left), Atom(right)) if left == right => (),
            (Var(left), Var(right)) if left == right => (),
            (Fun(from1, to1), Fun(from2, to2)) => {
                constraints.push((*from1, *from2));
                constraints.push((*to1, *to2));
            },
            | (Var(variable), value)
            | (value, Var(variable)) if !occurs_in(variable, &value) => {
                for (left, right) in &mut constraints {
                    substitute(variable, &value, left);
                    substitute(variable, &value, right);
                }
                // apply substitution to type_ directly
                // so we don't have to compute compositions of substitutions
                substitute(variable, &value, type_);
            },
            (left, right) => panic!("could not unify {left} with {right}")
        }
    }
}

// TODO: add this ugly global variable
static mut NEXT_FRESH_VARIABLE: usize = 0;

fn new_var() -> Type {
    unsafe { NEXT_FRESH_VARIABLE += 1; }
    Type::Var(unsafe { NEXT_FRESH_VARIABLE })
}

struct InferenceResult {
    type_: Type,
    constraints: Vec<(Type, Type)>
}

#[derive(Debug, Clone)]
pub struct Environment {
    variables: HashMap<String, Type>,
}

impl Environment {
    fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    fn infer_type(&self, expr: &Expression) -> InferenceResult {
        match &expr.data {
            ExpressionData::LetIn { name, value, body } => {
                todo!("typing of let expressions {name:?}, {value:?}, {body:?}")
            },
            ExpressionData::Fun { arg: (arg_name, _ /* == None */), body } => {
                let arg_type = new_var();
                
                let mut with_arg = self.clone();
                with_arg.add_variable(arg_name.to_string(), arg_type.clone());

                let InferenceResult { type_: body_type, constraints } = with_arg.infer_type(body);

                let arg_type = Box::new(arg_type);
                let body_type = Box::new(body_type);
                
                InferenceResult {
                    type_: Type::Fun(arg_type, body_type),
                    constraints
                }
            },
            ExpressionData::App { fun, arg } => {
                let result_type = new_var();
                let InferenceResult { type_: fun_type, constraints:     fun_constraints } = self.infer_type(fun);
                let InferenceResult { type_: arg_type, constraints: mut arg_constraints } = self.infer_type(arg);

                let mut constraints = fun_constraints;
                constraints.append(&mut arg_constraints);
                
                let result_type_boxed = Box::new(result_type.clone());
                let arg_type = Box::new(arg_type);

                constraints.push((fun_type, Type::Fun(arg_type, result_type_boxed)));

                InferenceResult {
                    type_: result_type,
                    constraints
                }
            },
            ExpressionData::Variable(v) => 
                match self.variables.get(v) {
                    Some(ty) => InferenceResult { type_: ty.clone(), constraints: vec![] },
                    None => panic!("unknown variable {v}")
                },
            ExpressionData::StringLiteral(_) => InferenceResult { type_: Type::Atom("string".to_string()), constraints: vec![] },
            ExpressionData::IntLiteral(_)    => InferenceResult { type_: Type::Atom("int"   .to_string()), constraints: vec![] }
        }
    }
}

pub fn infer_type(expr: &mut Expression) {
    let ctx = Environment { variables: HashMap::new() };

    let InferenceResult { mut type_, constraints } = ctx.infer_type(expr);

    unify(constraints, &mut type_);
    
    expr.type_ = Some(type_);
}
