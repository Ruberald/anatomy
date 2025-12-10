use crate::ast::*;
use crate::lexer::*;

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            l,
            cur_token: Token {
                token_type: TokenType::EOF,
                literal: None,
            },
            peek_token: Token {
                token_type: TokenType::EOF,
                literal: None,
            },
        };

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token.token_type == t {
            self.next_token();
            true
        } else {
            false
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, String> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::IDENT) {
            return Err("expected identifier after let".to_string());
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone().unwrap_or_default(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return Err("expected '=' after identifier".to_string());
        }

        // Skipping expression parsing for simplicity
        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Ok(Box::new(LetStatement {
            token,
            name,
            value: Box::new(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("0".to_string()),
                },
                value: "0".to_string(),
            }), // Placeholder
        }))
    }

    pub fn parse_statement(&mut self) -> Result<Option<Box<dyn Statement>>, String> {
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement().map(Some),
            // TokenType::RETURN => self.parse_return_statement().map(Some),
            _ => Ok(None), // For simplicity, we return None for unrecognized statements
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != TokenType::EOF {
            if let Some(stmt) = self.parse_statement()? {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // -------------------------------
    // parse_program() TESTS
    // -------------------------------

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().expect("parser error");
        assert_eq!(program.statements.len(), 3);

        let expected = ["x", "y", "foobar"];

        for (stmt, expected_ident) in program.statements.iter().zip(expected.iter()) {
            test_let_statement(stmt.as_ref(), expected_ident);
        }
    }

    fn test_let_statement(stmt: &dyn Statement, expected_name: &str) {
        assert_eq!(stmt.token_type(), TokenType::LET);

        let let_stmt = stmt
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("statement is not a LetStatement");

        assert_eq!(let_stmt.name.value, expected_name);
        assert_eq!(let_stmt.name.token_type(), TokenType::IDENT);
    }
}
