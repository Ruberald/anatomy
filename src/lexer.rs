#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    TRUE,
    FALSE,

    PLUS,
    COMMA,
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    LT,
    GT,
    EQ,
    NOT_EQ,

    INT,

    ASSIGN,

    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    IDENT,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: Option<String>,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn peek_char(&self) -> Option<u8> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.as_bytes()[self.read_position])
        }
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while (self.ch.unwrap_or(b'0') as char).is_whitespace() {
            self.read_char();
        }
    }

    #[rustfmt::skip]
    fn lookup_ident(ident: &str) -> Token {
        match ident {
            "let" => Token { token_type: TokenType::LET, literal: None, },
            "fn" => Token { token_type: TokenType::FUNCTION, literal: None, },
            "if" => Token { token_type: TokenType::IF, literal: None, },
            "else" => Token { token_type: TokenType::ELSE, literal: None, },
            "return" => Token { token_type: TokenType::RETURN, literal: None, },
            "true" => Token { token_type: TokenType::TRUE, literal: None, },
            "false" => Token { token_type: TokenType::FALSE, literal: None, },
            _ => Token { token_type: TokenType::IDENT, literal: Some(ident.to_string()), },
        }
    }

    // TODO: other number types (float, hex, etc.)
    fn read_number(&mut self) -> Token {
        let start = self.position;
        while (self.peek_char().unwrap_or(b' ') as char).is_numeric() {
            self.read_char();
        }

        let lit = self.input[start..self.position + 1].to_string();

        Token {
            token_type: TokenType::INT,
            literal: Some(lit),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.position;
        while (self.peek_char().unwrap_or(b' ') as char).is_alphanumeric()
            || self.peek_char().unwrap_or(b' ') == b'_'
        {
            self.read_char();
        }

        let lit = self.input[start..self.position + 1].to_string();

        Self::lookup_ident(&lit)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        #[rustfmt::skip]
        let tok = match self.ch {
            Some(b'=') => {
                if self.peek_char() == Some(b'=') {
                    self.read_char();
                    Token { token_type: TokenType::EQ, literal: None, }
                } else {
                    Token { token_type: TokenType::ASSIGN, literal: None, }
                }
            },
            Some(b'+') => Token { token_type: TokenType::PLUS, literal: None, },
            Some(b',') => Token { token_type: TokenType::COMMA, literal: None, },
            Some(b';') => Token { token_type: TokenType::SEMICOLON, literal: None, },
            Some(b'(') => Token { token_type: TokenType::LPAREN, literal: None, },
            Some(b')') => Token { token_type: TokenType::RPAREN, literal: None, },
            Some(b'{') => Token { token_type: TokenType::LBRACE, literal: None, },
            Some(b'}') => Token { token_type: TokenType::RBRACE, literal: None, },
            Some(b'!') => {
                if self.peek_char() == Some(b'=') {
                    self.read_char();
                    Token { token_type: TokenType::NOT_EQ, literal: None, }
                } else {
                    Token { token_type: TokenType::BANG, literal: None, }
                }
            },
            Some(b'-') => Token { token_type: TokenType::MINUS, literal: None, },
            Some(b'/') => Token { token_type: TokenType::SLASH, literal: None, },
            Some(b'*') => Token { token_type: TokenType::ASTERISK, literal: None, },
            Some(b'<') => Token { token_type: TokenType::LT, literal: None, },
            Some(b'>') => Token { token_type: TokenType::GT, literal: None, },
            Some(x) if (x as char).is_numeric() => self.read_number(),
            Some(x) => {
                // if x == b' ' { self.read_char(); }
                self.read_identifier()
            }

            _ => Token {
                token_type: TokenType::EOF,
                literal: None,
            },
        };

        self.read_char();

        tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("let x = y + z;");
        let mut lexer = Lexer::new(input);

        #[rustfmt::skip]
        let expected_tokens = vec![
            Token { token_type: TokenType::LET, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()), },
            Token { token_type: TokenType::ASSIGN, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()), },
            Token { token_type: TokenType::PLUS, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("z".to_string()), },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::EOF, literal: None, },
        ];

        for expected in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected);
        }
    }

    #[test]
    fn test_complex_program() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            ",
        );
        let mut lexer = Lexer::new(input);

        #[rustfmt::skip]
        let expected_tokens = vec![
            Token { token_type: TokenType::LET, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("five".to_string()), },
            Token { token_type: TokenType::ASSIGN, literal: None, },
            Token { token_type: TokenType::INT, literal: Some("5".to_string()), },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::LET, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()), },
            Token { token_type: TokenType::ASSIGN, literal: None, },
            Token { token_type: TokenType::INT, literal: Some("10".to_string()), },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::LET, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("add".to_string()), },
            Token { token_type: TokenType::ASSIGN, literal: None, },
            Token { token_type: TokenType::FUNCTION, literal: None, },
            Token { token_type: TokenType::LPAREN, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()), },
            Token { token_type: TokenType::COMMA, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()), },
            Token { token_type: TokenType::RPAREN, literal: None, },
            Token { token_type: TokenType::LBRACE, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()), },
            Token { token_type: TokenType::PLUS, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()), },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::RBRACE, literal: None, },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::LET, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("result".to_string()), },
            Token { token_type: TokenType::ASSIGN, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("add".to_string()), },
            Token { token_type: TokenType::LPAREN, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("five".to_string()), },
            Token { token_type: TokenType::COMMA, literal: None, },
            Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()), },
            Token { token_type: TokenType::RPAREN, literal: None, },
            Token { token_type: TokenType::SEMICOLON, literal: None, },
            Token { token_type: TokenType::EOF, literal: None, },
        ];

        for expected in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected);
        }
    }

    #[test]
    fn test_complex_program_with_operators() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            ",
        );

        let mut lexer = Lexer::new(input);

        #[rustfmt::skip]
        let expected_tokens = vec![
            Token { token_type: TokenType::LET, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("five".to_string()) },
            Token { token_type: TokenType::ASSIGN, literal: None },
            Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            Token { token_type: TokenType::LET, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()) },
            Token { token_type: TokenType::ASSIGN, literal: None },
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            Token { token_type: TokenType::LET, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("add".to_string()) },
            Token { token_type: TokenType::ASSIGN, literal: None },
            Token { token_type: TokenType::FUNCTION, literal: None },
            Token { token_type: TokenType::LPAREN, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()) },
            Token { token_type: TokenType::COMMA, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()) },
            Token { token_type: TokenType::RPAREN, literal: None },
            Token { token_type: TokenType::LBRACE, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()) },
            Token { token_type: TokenType::PLUS, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },
            Token { token_type: TokenType::RBRACE, literal: None },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            Token { token_type: TokenType::LET, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("result".to_string()) },
            Token { token_type: TokenType::ASSIGN, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("add".to_string()) },
            Token { token_type: TokenType::LPAREN, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("five".to_string()) },
            Token { token_type: TokenType::COMMA, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()) },
            Token { token_type: TokenType::RPAREN, literal: None },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            // NEW SECTION: "!-/*5;"
            Token { token_type: TokenType::BANG, literal: None },
            Token { token_type: TokenType::MINUS, literal: None },
            Token { token_type: TokenType::SLASH, literal: None },
            Token { token_type: TokenType::ASTERISK, literal: None },
            Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            // NEW SECTION: "5 < 10 > 5;"
            Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
            Token { token_type: TokenType::LT, literal: None },
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::GT, literal: None },
            Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            // NEW SECTION: "if (5 < 10) { return true; } else { return false; }"
            Token { token_type: TokenType::IF, literal: None },
            Token { token_type: TokenType::LPAREN, literal: None },
            Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
            Token { token_type: TokenType::LT, literal: None },
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::RPAREN, literal: None },
            Token { token_type: TokenType::LBRACE, literal: None },
            Token { token_type: TokenType::RETURN, literal: None },
            Token { token_type: TokenType::TRUE, literal: None },
            Token { token_type: TokenType::SEMICOLON, literal: None },
            Token { token_type: TokenType::RBRACE, literal: None },
            Token { token_type: TokenType::ELSE, literal: None },
            Token { token_type: TokenType::LBRACE, literal: None },
            Token { token_type: TokenType::RETURN, literal: None },
            Token { token_type: TokenType::FALSE, literal: None },
            Token { token_type: TokenType::SEMICOLON, literal: None },
            Token { token_type: TokenType::RBRACE, literal: None },

            // NEW SECTION: "10 == 10;"
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::EQ, literal: None },
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            // NEW SECTION: "10 != 9;"
            Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
            Token { token_type: TokenType::NOT_EQ, literal: None },
            Token { token_type: TokenType::INT, literal: Some("9".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },

            Token { token_type: TokenType::EOF, literal: None },
        ];

        for expected in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected);
        }
    }
}
