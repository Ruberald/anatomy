#[derive(Debug, PartialEq)]
enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

#[derive(Debug, PartialEq)]
struct Token {
    token_type: TokenType,
    literal: Option<String>,
}

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<u8>,
}

impl Lexer {

    fn new(input: String) -> Self {
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

    fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            _ => TokenType::IDENT,
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.position;
        while (self.peek_char().unwrap_or(b' ') as char).is_numeric() {
            self.read_char();
        }

        let lit = self.input[start..self.position+1].to_string();

        Token {
            token_type: TokenType::INT,
            literal: Some(lit),
        }

    }

    fn read_identifier(&mut self) -> Token {
        let start = self.position;
        while (self.peek_char().unwrap_or(b' ') as char).is_alphanumeric() || self.peek_char().unwrap_or(b' ') == b'_' {
            self.read_char();
        }

        let lit = self.input[start..self.position+1].to_string();

        Token {
            token_type: Self::lookup_ident(lit.as_str()),
            literal: Some(lit),
        }

    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {

            Some(b'=') => Token { token_type: TokenType::ASSIGN, literal: None },
            Some(b'+') => Token { token_type: TokenType::PLUS, literal: None },
            Some(b',') => Token { token_type: TokenType::COMMA, literal: None },
            Some(b';') => Token { token_type: TokenType::SEMICOLON, literal: None },
            Some(b'(') => Token { token_type: TokenType::LPAREN, literal: None },
            Some(b')') => Token { token_type: TokenType::RPAREN, literal: None },
            Some(b'{') => Token { token_type: TokenType::LBRACE, literal: None },
            Some(b'}') => Token { token_type: TokenType::RBRACE, literal: None },
            Some(x) if (x as char).is_numeric() => {
                self.read_number()
            }
            Some(x) => {
                // if x == b' ' { self.read_char(); }
                self.read_identifier()
            }

            _ => Token { token_type: TokenType::EOF, literal: None },
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

        let expected_tokens = vec![
            Token { token_type: TokenType::LET, literal: Some("let".to_string()) },
            Token { token_type: TokenType::IDENT, literal: Some("x".to_string()) },
            Token { token_type: TokenType::ASSIGN, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("y".to_string()) },
            Token { token_type: TokenType::PLUS, literal: None },
            Token { token_type: TokenType::IDENT, literal: Some("z".to_string()) },
            Token { token_type: TokenType::SEMICOLON, literal: None },
            Token { token_type: TokenType::EOF, literal: None },
        ];

        for expected in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected);
        }
    }


    #[test]
    fn test_complex_program() {
        let input = String::from("
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            ");
            let mut lexer = Lexer::new(input);

            let expected_tokens = vec![
                Token { token_type: TokenType::LET, literal: Some("let".to_string()) },
                Token { token_type: TokenType::IDENT, literal: Some("five".to_string()) },
                Token { token_type: TokenType::ASSIGN, literal: None },
                Token { token_type: TokenType::INT, literal: Some("5".to_string()) },
                Token { token_type: TokenType::SEMICOLON, literal: None },

                Token { token_type: TokenType::LET, literal: Some("let".to_string()) },
                Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()) },
                Token { token_type: TokenType::ASSIGN, literal: None },
                Token { token_type: TokenType::INT, literal: Some("10".to_string()) },
                Token { token_type: TokenType::SEMICOLON, literal: None },

                Token { token_type: TokenType::LET, literal: Some("let".to_string()) },
                Token { token_type: TokenType::IDENT, literal: Some("add".to_string()) },
                Token { token_type: TokenType::ASSIGN, literal: None },
                Token { token_type: TokenType::FUNCTION, literal: Some("fn".to_string()) },
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

                Token { token_type: TokenType::LET, literal: Some("let".to_string()) },
                Token { token_type: TokenType::IDENT, literal: Some("result".to_string()) },
                Token { token_type: TokenType::ASSIGN, literal: None },
                Token { token_type: TokenType::IDENT, literal: Some("add".to_string()) },
                Token { token_type: TokenType::LPAREN, literal: None },
                Token { token_type: TokenType::IDENT, literal: Some("five".to_string()) },
                Token { token_type: TokenType::COMMA, literal: None },
                Token { token_type: TokenType::IDENT, literal: Some("ten".to_string()) },
                Token { token_type: TokenType::RPAREN, literal: None },
                Token { token_type: TokenType::SEMICOLON, literal: None },

                Token { token_type: TokenType::EOF, literal: None },
                ];

            for expected in expected_tokens {
                let tok = lexer.next_token();
                println!("{:?}", tok);
                assert_eq!(tok, expected);
            }
    }
}

