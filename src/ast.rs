use std::any::Any;

use crate::lexer::Token;
use crate::lexer::TokenType;

pub trait Node: Any {
    fn token_type(&self) -> TokenType;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {}

pub trait Expression: Node {}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_type(&self) -> TokenType {
        if !self.statements.is_empty() {
            self.statements[0].token_type()
        } else {
            TokenType::EOF
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Identifier {
    pub token: Token, // the token.IDENT token
    pub value: String,
}
impl Expression for Identifier {}
impl Node for Identifier {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct LetStatement {
    pub token: Token, // the token.LET token
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Statement for LetStatement {}
impl Node for LetStatement {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
