use std::any::Any;

use crate::lexer::Token;
use crate::lexer::TokenType;

pub trait Node: Any {
    fn token_type(&self) -> TokenType;
    fn as_any(&self) -> &dyn Any;
    fn string(&self) -> String;
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

    fn string(&self) -> String {
        let mut out = String::new();
        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }
        out
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

    fn string(&self) -> String {
        self.value.clone()
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

    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token.literal.clone().unwrap_or_default());
        out.push(' ');
        out.push_str(&self.name.string());
        out.push_str(" = ");
        out.push_str(&self.value.string());
        out.push(';');
        out
    }
}

pub struct ReturnStatement {
    pub token: Token, // the 'return' token
    pub return_value: Box<dyn Expression>,
}

impl Statement for ReturnStatement {}
impl Node for ReturnStatement {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token.literal.clone().unwrap_or_default());
        out.push(' ');
        out.push_str(&self.return_value.string());
        out.push(';');
        out
    }
}

pub struct ExpressionStatement {
    pub token: Token, // the first token of the expression
    pub expression: Box<dyn Expression>,
}

impl Statement for ExpressionStatement {}
impl Node for ExpressionStatement {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        self.expression.string()
    }
}

pub struct InfixExpression {
    pub token: Token, // The operator token, e.g. +
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.left.string());
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        out.push_str(&self.right.string());
        out.push(')');
        out
    }
}

impl Expression for InfixExpression {}

pub struct PrefixExpression {
    pub token: Token, // The prefix token, e.g. !
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.operator);
        out.push_str(&self.right.string());
        out.push(')');
        out
    }
}
impl Expression for PrefixExpression {}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        self.token.literal.clone().unwrap_or_default()
    }
}
impl Expression for IntegerLiteral {}

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_type(&self) -> TokenType {
        self.token.token_type.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn string(&self) -> String {
        self.token_type().to_string()
    }
}
impl Expression for Boolean {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement_string() {
        let let_stmt = LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: Some("let".to_string()),
            },
            name: Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("myVar".to_string()),
                },
                value: "myVar".to_string(),
            },
            value: Box::new(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("anotherVar".to_string()),
                },
                value: "anotherVar".to_string(),
            }),
        };
        assert_eq!(let_stmt.string(), "let myVar = anotherVar;");
    }
}
