use crate::lexer::{Token, TokenData};
use crate::tree::{Expression, ExpressionData};

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

    pub fn parse(mut self) -> Expression {
        self.expression()
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
            Some(other) => panic!("expected inneression, found {other:?}"),
            None => panic!("expected expression, found EOF"),
        };

        while let Some(TokenData::LeftParen) = self.current_token() {
            self.current += 1;
            let arg = self.expression();

            self.expect(TokenData::RightParen);

            expr = Expression::untyped(ExpressionData::App {
                fun: Box::new(expr),
                arg: Box::new(arg)
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
            body: Box::new(body)
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
