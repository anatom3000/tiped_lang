use std::collections::HashMap;

use crate::tree::{Type, Expression, ExpressionData};

fn occurs_in(variable: &str, term: &Type) -> bool {
    match term {
        Type::Var(name)     => variable == name,
        Type::Atom(_)       => false,
        Type::Fun(from, to) => occurs_in(variable, &from)
                            || occurs_in(variable, &to),
    }
}

fn disagreement(expr1: &Type, expr2: &Type) -> Option<(Type, Type)> {
    match (expr1, expr2) {
        | (Type::Atom(name1), Type::Atom(name2))
        | (Type::Var(name1), Type::Var(name2))
            => (name1 != name2).then_some((expr1.clone(), expr2.clone())),

        | (Type::Atom(_), Type::Var(_))
        | (Type::Var(_), Type::Atom(_))
            => Some((expr1.clone(), expr2.clone())),

        (Type::Atom(_) | Type::Var(_), Type::Fun(_, _))
            => Some((expr1.clone(), expr2.clone())),

        (Type::Fun(_, _), Type::Atom(_) | Type::Var(_))
            => Some((expr2.clone(), expr1.clone())),

        (Type::Fun(from1, to1), Type::Fun(from2, to2))
            => disagreement(from1, from2)
            .or_else(|| disagreement(to1, to2)),
    }
}

fn substitute(old: &str, new: &Type, expr: &mut Type) {
    match expr {
        Type::Var(v) if v == old => *expr = new.clone(),
        Type::Fun(from, to) => {
            substitute(old, new, from);
            substitute(old, new, to);
        },
        | Type::Atom(_)
        | Type::Var(_) => (),
    }
}

fn unify(expr1: &mut Type, expr2: &mut Type) -> bool {
    // println!("starting unification of {expr1} and {expr2}");
    // storing the substitutions is not needed, as they happen in place
    while expr1 != expr2 {
        let (d1, d2) = disagreement(&expr1, &expr2)
            .expect("expr1 and expr2 should disagree, otherwise they are identical");
        match (d1, d2) {
            (Type::Var(d1), d2) => {
                if occurs_in(&d1, &d2) {
                    //println!("d1='{d1} occurs in d2={d2}, unification failed...");
                    return false;
                }

                substitute(&d1, &d2, expr1);
                substitute(&d1, &d2, expr2);
            },
            (d1, Type::Var(d2)) => {
                if occurs_in(&d2, &d1) {
                    //println!("d2='{d2} occurs in d1={d1}, unification failed...");
                    return false;
                }

                substitute(&d2, &d1, expr1);
                substitute(&d2, &d1, expr2);
            }
            (_d1, _d2) => {
                //println!("d1={d1} was not a variable, (d2={d2}) unification failed...");
                return false;
            }
        }
    }

    true
}

// TODO: add this ugly global variable
static mut NEXT_FRESH_VARIABLE: usize = 0;

#[derive(Debug, Clone)]
pub struct Context {
    variables: HashMap<String, Type>,
}

impl Context {
    fn new_var(&mut self) -> Type {
        unsafe { NEXT_FRESH_VARIABLE += 1; }
        Type::Var(format!("{}", unsafe { NEXT_FRESH_VARIABLE }))
    }

    fn add_variable(mut self, name: String, ty: Type) -> Self {
        self.variables.insert(name, ty);
        self
    }

    fn infer_type(mut self, expr: &mut Expression) {
        expr.type_ = Some(match &mut expr.data {
            ExpressionData::LetIn { name, value, body } => {
                // [Let] rule
                self.clone().infer_type(value);
                let with_var = self.add_variable(name.clone(), value.type_.clone().expect("value was typed above"));
                
                with_var.infer_type(body);
                body.type_.clone().expect("body was typed above")       
            },
            ExpressionData::Fun { arg: (arg_name, _), body } => {
                let arg_type = self.new_var();
                let with_arg = self.add_variable(arg_name.to_string(), arg_type.clone());
                
                with_arg.infer_type(body);
                let return_type = body.type_.clone().expect("body was typed above");

                let arg_type = Box::new(arg_type);
                let return_type = Box::new(return_type);

                Type::Fun(arg_type, return_type)
            },
            ExpressionData::App { fun, arg } => {
                self.clone().infer_type(fun);
                self.clone().infer_type(arg);
                
                let return_type = self.new_var();
                let mut fun_type = fun.type_.clone().expect("fun was typed above");
                let arg_type = arg.type_.clone().expect("arg was typed above");
                let mut infered_fun_type = 
                    Type::Fun(
                        Box::new(arg_type),
                        Box::new(return_type)
                    );

                let fun_type_old = fun_type.clone();
                let infered_fun_type_old = infered_fun_type.clone();

                if !unify(&mut fun_type, &mut infered_fun_type) {
                    panic!("could not unify {fun_type_old} and {infered_fun_type_old}");
                }
                
                let Type::Fun(_, return_type) = fun_type
                    else { unreachable!("fun type have been unified successfully") };

                *return_type
            },
            ExpressionData::StringLiteral(_) => Type::Atom("string".to_string()),
            ExpressionData::IntLiteral(_) => Type::Atom("int".to_string()),
            ExpressionData::Variable(v) => {
                match self.variables.get(v) {
                    Some(ty) => ty.clone(),
                    None => panic!("unknown variable {v}")
                }
            },
        })
    }
}

pub fn infer_type(expr: &mut Expression) {
    let ctx = Context { variables: HashMap::new() };
    ctx.infer_type(expr);
}
