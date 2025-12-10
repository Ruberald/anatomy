use crate::ast::*;
use crate::lexer::*;

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken {
        expected: TokenType,
        found: TokenType,
    },
    MissingIdentifier,
    MissingAssign,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedToken { expected, found } => {
                write!(f, "Expected token {:?}, found {:?}", expected, found)
            }
            ParserError::MissingIdentifier => write!(f, "Expected identifier after 'let'"),
            ParserError::MissingAssign => write!(f, "Expected '=' after identifier"),
        }
    }
}

pub struct ParserErrors(pub Vec<ParserError>);

impl std::fmt::Display for ParserErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.0 {
            writeln!(f, "{error}")?;
        }
        Ok(())
    }
}

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: ParserErrors,
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
            errors: ParserErrors(Vec::new()),
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

    pub fn errors(&self) -> &ParserErrors {
        &self.errors
    }

    pub fn peek_error(&mut self, t: TokenType) {
        let error = ParserError::ExpectedToken {
            expected: t,
            found: self.peek_token.token_type.clone(),
        };
        self.errors.0.push(error);
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token.token_type == t {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::IDENT) {
            self.errors.0.push(ParserError::MissingIdentifier);
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone().unwrap_or_default(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            self.errors.0.push(ParserError::MissingAssign);
            return None;
        }

        // Skipping expression parsing for simplicity
        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(LetStatement {
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

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            // TokenType::RETURN => self.parse_return_statement().map(Some),
            _ => None, // For simplicity, we return None for unrecognized statements
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
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
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        let expected = ["x", "y", "foobar"];

        for (stmt, expected_ident) in program.statements.iter().zip(expected.iter()) {
            test_let_statement(&**stmt, expected_ident);
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        assert!(
            errors.0.is_empty(),
            "Parser has {} errors:\n{}",
            errors.0.len(),
            errors
        );
    }

    fn test_let_statement(stmt: &dyn Statement, expected_name: &str) {
        assert_eq!(stmt.token_type(), TokenType::LET, "Wrong token type");

        let let_stmt = stmt
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expected LetStatement");

        assert_eq!(let_stmt.name.value, expected_name);
        assert_eq!(let_stmt.name.token_type(), TokenType::IDENT);
    }
}
