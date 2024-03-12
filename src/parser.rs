use std::collections::{HashMap, HashSet};

use crate::lexer::{Token, TokenData};
use crate::tree::{Declaration, Expression, Type};

macro_rules! parsing_error {
    ($self:ident, $message:expr $(, $($args:expr),*)?) => {
        match $self.current_token_full() {
            Some(Token { data: _, line, column }) => {
                panic!(concat!("parsing error: ", $message, " at {}:{}"), line, column, $($($args, )*)?)
            },
            None => {
                panic!(concat!("parsing error: ", $message, " at EOF"), $($($args, )*)?)
            },
        }
    };
}

macro_rules! parsing_error_expected {
    ($self:ident, $expected:expr) => {
        match $self.current_token_full() {
            Some(Token { data: found, line, column }) => {
                panic!("parsing error: expected {}, found {} at {}:{}", $expected, found, line, column)
            },
            None => {
                panic!("parsing error: expected {}, found EOF", $expected)
            },
        }
    };
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn current_token_full(&mut self) -> Option<Token> {
        self.tokens.get(self.current).cloned()
    }

    fn current_token(&mut self) -> Option<TokenData> {
        self.current_token_full().map(|t| t.data)
    }

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Vec<Declaration> {
        let mut decls = vec![];
        while self.current_token().is_some() {
            decls.push(self.declaration());
        }

        decls
    }

    fn declaration(&mut self) -> Declaration {
        match self.current_token() {
            Some(TokenData::Let) => {
                self.current += 1;
                self.let_decl()
            }
            Some(TokenData::Type) => {
                self.current += 1;
                self.type_decl()
            },
            _ => Declaration::UnboundExpr(self.expression()),
        }
    }

    fn let_decl(&mut self) -> Declaration {
        let Some(TokenData::Identifier(name)) = self.current_token()
            else { parsing_error_expected!(self, "identifier after `let`") };
        let name = name.to_string();

        self.current += 1;
        self.expect(TokenData::Assign);

        match self.current_token() {
            Some(TokenData::Extern) => {
                self.current += 1;

                let type_ = self.type_();

                Declaration::ExternLet { name, type_ }
            }
            _ => {
                let value = self.expression();
                Declaration::Let { name, value }
            }
        }
    }

    fn parameterized_atom(&mut self, parameters: &mut HashSet<String>) {
        while let Some(TokenData::Tick) = self.current_token() {
            self.current += 1;
            match self.current_token() {
                Some(TokenData::Identifier(p)) => {
                    if parameters.contains(&p) {
                        parsing_error!(self, "type parameter '{} occurs several times", p);
                    }
                    parameters.insert(p.to_string());

                    self.current += 1;
                }
                _ => parsing_error_expected!(self, "identifier after `'`"),
            }
        }
    }

    fn type_decl(&mut self) -> Declaration {
        match self.current_token() {
            Some(TokenData::Identifier(name)) => {
                let name = name.to_string();
                self.current += 1;
                Declaration::Type {
                    name,
                    parameter_count: 0,
                }
            }
            Some(TokenData::Tick) => {
                // parameterized atom
                let mut parameters = HashSet::new();
                self.parameterized_atom(&mut parameters);

                let name = match self.current_token() {
                    Some(TokenData::Identifier(name)) => {
                        let name = name.to_string();
                        self.current += 1;
                        name
                    }
                    _ => parsing_error_expected!(self, "identifier after `'`"),
                };

                Declaration::Type {
                    name,
                    parameter_count: parameters.len(),
                }
            }
            _ => parsing_error_expected!(self, "identifier after `type`"),
        }
    }

    fn type_(&mut self) -> Type {
        let mut variables = HashMap::new();
        let ty = self.type_matrix(&mut variables);

        if variables.is_empty() {
            ty
        } else {
            let variables = (1..=variables.len()).collect();

            Type::Polymorphic {
                variables,
                matrix: Box::new(ty),
            }
        }
    }

    fn type_matrix(&mut self, variables: &mut HashMap<String, usize>) -> Type {
        let ty = match self.current_token() {
            Some(TokenData::Identifier(name)) => {
                let name = name.to_string();
                self.current += 1;
                Type::Atom(name)
            }
            Some(TokenData::Tick) => {
                self.current += 1;
                match self.current_token() {
                    Some(TokenData::Identifier(v)) => {
                        let count = variables.len();

                        let v = variables.entry(v.to_string()).or_insert(count + 1);
                        self.current += 1;

                        let v = Type::PolymorphicVar(*v);

                        match self.current_token() {
                            Some(TokenData::Identifier(name)) => {
                                let name = name.to_string();
                                self.current += 1;
                                Type::ParameterizedAtom {
                                    name,
                                    parameters: vec![v],
                                }
                            }
                            Some(TokenData::Tick) => {
                                let mut parameters = vec![v];

                                while let Some(TokenData::Tick) = self.current_token() {
                                    self.current += 1;
                                    match self.current_token() {
                                        Some(TokenData::Identifier(p)) => {
                                            let p =
                                                variables.entry(p.to_string()).or_insert(count + 1);
                                            parameters.push(Type::PolymorphicVar(*p));

                                            self.current += 1;
                                        }
                                        _ => parsing_error_expected!(self, "identifier after `'`"),
                                    }
                                }

                                let name = match self.current_token() {
                                    Some(TokenData::Identifier(name)) => {
                                        let name = name.to_string();
                                        self.current += 1;
                                        name
                                    }
                                    _ => parsing_error_expected!(self, "identifier after `'`"),
                                };

                                Type::ParameterizedAtom { name, parameters }
                            }
                            _ => v,
                        }
                    }
                    _ => parsing_error_expected!(self, "identifier after `'`"),
                }
            }
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let ty = self.type_matrix(variables);
                self.expect(TokenData::RightParen);
                ty
            }
            _ => parsing_error_expected!(self, "identifier, `'` or `(`"),
        };

        match self.current_token() {
            Some(TokenData::Arrow) => {
                self.current += 1;
                let to = self.type_matrix(variables);

                let from = Box::new(ty);
                let to = Box::new(to);

                Type::Fun(from, to)
            }
            _ => ty,
        }
    }

    fn expression(&mut self) -> Expression {
        use Expression::*;

        let mut expr = match self.current_token() {
            Some(TokenData::NewLine) => {
                self.current += 1;
                self.expression()
            }
            Some(TokenData::Let) => {
                self.current += 1;
                self.let_in()
            }
            Some(TokenData::Fun) => {
                self.current += 1;
                self.fun()
            }
            Some(TokenData::String(content)) => {
                let content = content.clone();
                self.current += 1;
                StringLiteral(content)
            }
            Some(TokenData::Integer(value)) => {
                let value = value.parse().expect("int literal is a digit sequence");
                self.current += 1;
                IntLiteral(value)
            }
            Some(TokenData::Identifier(name)) => {
                let name = name.clone();
                self.current += 1;
                Variable(name)
            }
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let inner = self.expression();
                self.expect(TokenData::RightParen);
                inner
            }
            _ => parsing_error_expected!(self, "expression"),
        };

        while let Some(TokenData::LeftParen) = self.current_token() {
            self.current += 1;

            let mut args = vec![];
            loop {
                args.push(self.expression());

                match self.current_token() {
                    Some(TokenData::Comma) => {
                        self.current += 1;
                        if let Some(TokenData::RightParen) = self.current_token() {
                            break;
                        } else {
                            continue;
                        }
                    },
                    Some(TokenData::RightParen) => break,
                    _ => parsing_error_expected!(self, "`,` or `)` after expression in function application"),
                }
            }

            self.expect(TokenData::RightParen);

            for arg in args {
                expr = Expression::App {
                    fun: Box::new(expr),
                    arg: Box::new(arg),
                };
            }
        }

        expr
    }

    fn let_in(&mut self) -> Expression {
        let Some(TokenData::Identifier(name)) = self.current_token()
            else { parsing_error_expected!(self, "identifier after `let`") };
        let name = name.to_string();

        self.current += 1;
        self.expect(TokenData::Assign);
        let value = Box::new(self.expression());
        self.expect(TokenData::In);

        let body = Box::new(self.expression());

        Expression::LetIn { name, value, body }
    }

    fn fun(&mut self) -> Expression {
        let mut args = vec![];

        loop {
            match self.current_token() {
                Some(TokenData::Identifier(arg)) => {
                    let arg = arg.to_string();
                    self.current += 1;

                    args.push(arg);
                }
                Some(TokenData::Arrow) => break,
                _ => parsing_error_expected!(self, "argument or `->` in fun head"),
            }
        }

        // you can write "fun -> e" instead of "fun _ -> e" for procedures
        if args.is_empty() {
            args.push("_".to_string());
        };

        self.expect(TokenData::Arrow);

        let body = self.expression();

        let mut expr = body;
        for arg in args.into_iter().rev() {
            expr = Expression::Fun {
                arg,
                body: Box::new(expr),
            };
        }

        expr
    }

    fn expect(&mut self, expected: TokenData) {
        match self.current_token() {
            Some(tok) if tok == expected => {
                self.current += 1;
            }
            _ => parsing_error_expected!(self, format!("{expected}")),
        }
    }
}
