use std::collections::{HashMap, HashSet};

use crate::tree::{Type, Expression, ExpressionData};

fn occurs_in(variable: usize, term: &Type) -> bool {
    match term {
        Type::Var(name)     => &variable == name,
        Type::Fun(from, to) => occurs_in(variable, &from)
                            || occurs_in(variable, &to),
        | Type::Atom(_)
        | Type:: PolymorphicVar(_) => false,
        Type::Polymorphic { variables: _, matrix } => occurs_in(variable, matrix),
    }
}

fn substitute(old: usize, new: &Type, expr: &mut Type) {
    match expr {
        Type::Var(v) if v == &old => *expr = new.clone(),
        Type::Fun(from, to) => {
            substitute(old, new, from);
            substitute(old, new, to);
        },
        Type::Polymorphic { variables: _, matrix } => substitute(old, new, matrix),
        | Type::Atom(_)
        | Type::PolymorphicVar(_)
        | Type::Var(_) => (),
    }
}

fn substitute_polymorphic_variables(old: usize, new: &Type, expr: &mut Type) {
    match expr {
        Type::PolymorphicVar(v) if v == &old => *expr = new.clone(),
        Type::Polymorphic { variables: _, matrix } => substitute_polymorphic_variables(old, new, matrix),
        Type::Fun(from, to) => {
            substitute_polymorphic_variables(old, new, from);
            substitute_polymorphic_variables(old, new, to);
        },
        | Type::PolymorphicVar(_)
        | Type::Atom(_)
        | Type::Var(_) => (),
    }
}


// TODO: add this ugly global variable
static mut NEXT_FRESH_VARIABLE: usize = 0;


struct InferenceResult {
    type_: Type,
    constraints: Vec<(Type, Type)>
}

#[derive(Debug, Clone)]
pub struct Environment {
    variables: HashMap<String, Type>,
    type_variables: HashSet<usize>,
    parent_type_variables: HashSet<usize>,
}

impl Environment {
    fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    fn sub(&self) -> Self {
        let mut new = self.clone();
        new.parent_type_variables = new.parent_type_variables.clone();

        new
    }
    
    fn new_var(&mut self) -> Type {
        unsafe { NEXT_FRESH_VARIABLE += 1; }
        self.type_variables.insert(unsafe { NEXT_FRESH_VARIABLE });
        Type::Var(unsafe { NEXT_FRESH_VARIABLE })
    }

    fn instantiate(&mut self, ty: Type) -> Type {
        match ty {
            Type::Polymorphic { variables, mut matrix } => {
                for v in variables {
                    substitute_polymorphic_variables(v, &self.new_var(), &mut matrix);
                }
                *matrix
            },
            Type::PolymorphicVar(_) => unreachable!("a polymorphic variable should not be alone"),
            | Type::Fun(_, _) // all polymorphic types are in prenex form
            | Type::Atom(_)
            | Type::Var(_) => ty,
        }
    }

    fn unify(&mut self, mut constraints: Vec<(Type, Type)>, type_: &mut Type) {
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
                    // apply substitution directly
                    // so we don't have to compute compositions of substitutions
                    substitute(variable, &value, type_);
                    for (_, ty) in &mut self.variables {
                        substitute(variable, &value, ty);
                    }

                    self.type_variables.remove(&variable);
                },
                (left, right) => panic!("could not unify {left} with {right}")
            }
        }
    }

    fn generalize(&mut self, constraints: Vec<(Type, Type)>, mut ty: Type) -> Type {
        fn vars(ty: &Type) -> Vec<usize> {
            match ty {
                Type::Atom(_) => vec![],
                Type::Var(v) => vec![*v],
                Type::Fun(from, to) => {
                    let mut from_vars = vars(from);
                    let mut to_vars = vars(to);

                    // potential duplicates!
                    from_vars.append(&mut to_vars);
                    from_vars
                },
                | Type::Polymorphic {..}
                | Type::PolymorphicVar(_) => unreachable!("type shouldn't be polymorphic while generalizing"),
            }
        }

        self.unify(constraints, &mut ty);

        let mut variables = vars(&ty);

        variables.sort();
        variables.dedup();

        for v in &variables {
            if !self.parent_type_variables.contains(v) {
                substitute(*v, &Type::PolymorphicVar(*v), &mut ty);
            }
        }

        Type::Polymorphic {
            variables,
            matrix: Box::new(ty),
        }
    }

    fn infer_type(&mut self, expr: &Expression) -> InferenceResult {
        match &expr.data {
            ExpressionData::LetIn { name, value, body } => {
                let InferenceResult { type_: value_type, constraints: value_constraints } = self.infer_type(value);
                let value_type = self.generalize(value_constraints.clone(), value_type);
                self.add_variable(name.to_string(), value_type.clone());

                let InferenceResult { type_: body_type, constraints: mut body_constraints } = self.infer_type(body);

                let mut constraints = value_constraints;
                constraints.append(&mut body_constraints);

                InferenceResult { type_: body_type, constraints }
            },
            ExpressionData::Fun { arg: (arg_name, _ /* == None */), body } => {
                
                let mut with_arg = self.sub();
                let arg_type = with_arg.new_var();

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
                let result_type = self.new_var();

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
                    Some(ty) => InferenceResult { type_: self.instantiate(ty.clone()), constraints: vec![] },
                    None => panic!("unknown variable {v}")
                },
            ExpressionData::StringLiteral(_) => InferenceResult { type_: Type::Atom("string".to_string()), constraints: vec![] },
            ExpressionData::IntLiteral(_)    => InferenceResult { type_: Type::Atom("int"   .to_string()), constraints: vec![] }
        }
    }
}

pub fn infer_type(expr: &mut Expression) {
    let mut ctx = Environment {
        variables: HashMap::new(), 
        type_variables: HashSet::new(),
        parent_type_variables: HashSet::new(),
    };

    let InferenceResult { type_, constraints } = ctx.infer_type(expr);

    let type_ = ctx.generalize(constraints, type_);
    
    expr.type_ = Some(type_);
}
