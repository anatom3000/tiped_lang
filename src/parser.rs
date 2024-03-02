use crate::lexer::{Token, TokenData};
use crate::tree::{Declaration, Expression, ExpressionData, Type};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn current_token_full(&mut self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn current_token(&mut self) -> Option<&TokenData> {
        self.current_token_full().map(|t| &t.data)
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
            }
            Some(other) => panic!("expected statement, found {other:?}"),
            None => panic!("expected statement, found EOF"),
        }
    }

    fn let_decl(&mut self) -> Declaration {
        let Some(TokenData::Identifier(name)) = self.current_token()
            else { panic!("expected identifier after `let`") };
        let name = name.to_string();

        self.current += 1;
        self.expect(TokenData::Assign);

        match self.current_token() {
            Some(TokenData::Extern) => {
                self.current += 1;

                let type_ = self.type_();

                Declaration::ExternLet { name, type_ }
            },
            _ => {
                let value = self.expression();
                Declaration::Let { name, value }
            }
        }
    }

    fn type_decl(&mut self) -> Declaration {
        match self.current_token() {
            Some(TokenData::Identifier(name)) => {
                let name = name.to_string();
                self.current += 1;
                Declaration::Type { name }
            }
            Some(other) => panic!("expected identifier after `type`, found {other:?}"),
            None => panic!("expected identifier after `type`, found EOF"),
        }
    }

    fn type_(&mut self) -> Type {
        let mut variable_count = 0;
        let ty = self.type_matrix(&mut variable_count);
        
        if variable_count == 0 {
            ty
        } else {
            let variables = (1..=variable_count).collect();
            
            Type::Polymorphic {
                variables,
                matrix: Box::new(ty),
            }
        }
    }

    fn type_matrix(&mut self, variable_count: &mut usize) -> Type {
        let ty = match self.current_token() {
            Some(TokenData::Identifier(name)) => {
                let name = name.to_string();
                self.current += 1;
                Type::Atom(name)
            },
            Some(TokenData::Tick) => {
                self.current += 1;
                match self.current_token() {
                    Some(TokenData::Identifier(_)) => {
                        self.current += 1;
                        *variable_count += 1;
                        Type::PolymorphicVar(*variable_count)
                    },
                    Some(other) => panic!("expected identifier after `'`, found {other:?}"),
                    None => panic!("expected identifier after `'`, found EOF"),
                
                }
            },
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let ty = self.type_matrix(variable_count);
                self.expect(TokenData::RightParen);
                ty
            },
            Some(other) => panic!("expected identifier, `'` or `(`, found {other:?}"),
            None => panic!("expected identifier, `'` or `(`, found EOF"),
        };

        match self.current_token() {
            Some(TokenData::Arrow) => {
                self.current += 1;
                let to = self.type_matrix(variable_count);
                
                let from = Box::new(ty);
                let to = Box::new(to);

                Type::Fun(from, to)
            },
            _ => ty
        }
    }

    fn expression(&mut self) -> Expression {
        use ExpressionData::*;

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
                Expression::untyped(StringLiteral(content))
            }
            Some(TokenData::Integer(value)) => {
                let value = value.parse().expect("int literal is a digit sequence");
                self.current += 1;
                Expression::untyped(IntLiteral(value))
            }
            Some(TokenData::Identifier(name)) => {
                let name = name.clone();
                self.current += 1;
                Expression::untyped(Variable(name))
            }
            Some(TokenData::LeftParen) => {
                self.current += 1;
                let inner = self.expression();
                self.expect(TokenData::RightParen);
                inner
            }
            Some(other) => panic!("expected expression, found {other:?}"),
            None => panic!("expected expression, found EOF"),
        };

        while let Some(TokenData::LeftParen) = self.current_token() {
            self.current += 1;
            let arg = self.expression();

            self.expect(TokenData::RightParen);

            expr = Expression::untyped(ExpressionData::App {
                fun: Box::new(expr),
                arg: Box::new(arg),
            })
        }

        expr
    }

    fn let_in(&mut self) -> Expression {
        let Some(TokenData::Identifier(name)) = self.current_token()
            else { panic!("expected identifier after `let`") };
        let name = name.to_string();

        self.current += 1;
        self.expect(TokenData::Assign);
        let value = Box::new(self.expression());
        self.expect(TokenData::In);

        let body = Box::new(self.expression());

        Expression::untyped(ExpressionData::LetIn { name, value, body })
    }

    fn fun(&mut self) -> Expression {
        let Some(TokenData::Identifier(arg)) = self.current_token() else
            { panic!("expected argument after `fun`") };

        let arg = arg.to_string();
        self.current += 1;

        self.expect(TokenData::Arrow);

        let body = self.expression();

        Expression::untyped(ExpressionData::Fun {
            arg: (arg, None),
            body: Box::new(body),
        })
    }

    fn expect(&mut self, expected: TokenData) {
        match self.current_token() {
            Some(tok) if tok == &expected => {
                self.current += 1;
            }
            Some(unexpected) => panic!("expected {expected:?}, found {unexpected}"),
            None => panic!("expected {expected:?}, found EOF"),
        }
    }
}
