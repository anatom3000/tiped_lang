use crate::tree::{Type, Expression};

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

pub fn unify(mut expr1: Type, mut expr2: Type) -> bool {
    // substitutions not needed
    while expr1 != expr2 {
        let (d1, d2) = disagreement(&expr1, &expr2)
            .expect("expr1 and expr2 should disagree, otherwise they are identical");
        
        match d1 {
            Type::Var(d1) => {
                if occurs_in(&d1, &d2) { return false; }

                substitute(&d1, &d2, &mut expr1);
                substitute(&d1, &d2, &mut expr2);
            },
            _ => return false
        }
    }

    true
}

pub fn type_expression(expr: Expression) -> Expression {
    todo!()
}
