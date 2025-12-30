use crate::ast::*;
use crate::lexer::*;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken {
        expected: TokenType,
        found: TokenType,
    },
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedToken { expected, found } => {
                write!(f, "expected {:?}, got {:?}", expected, found)
            }
        }
    }
}

pub struct ParserErrors(pub Vec<ParserError>);

impl std::fmt::Display for ParserErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.0 {
            writeln!(f, "{e}")?;
        }
        Ok(())
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X !X
    CALL,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Option<Box<dyn Expression>>;

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: ParserErrors,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
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
            errors: ParserErrors(vec![]),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.next_token();
        p.next_token();

        // -------- prefix --------
        p.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        p.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        p.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        p.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        p.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::IF, Parser::parse_if_expression);
        p.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);

        // -------- infix --------
        p.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        p.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        p.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        p.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        p.register_infix(TokenType::NOT_EQ, Parser::parse_infix_expression);
        p.register_infix(TokenType::ASSIGN, Parser::parse_infix_expression);
        p.register_infix(TokenType::LT, Parser::parse_infix_expression);
        p.register_infix(TokenType::GT, Parser::parse_infix_expression);
        p.register_infix(TokenType::LPAREN, Parser::parse_call_expression);

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn errors(&self) -> &ParserErrors {
        &self.errors
    }

    fn register_prefix(&mut self, token: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, func);
    }

    fn register_infix(&mut self, token: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token, func);
    }

    // =====================
    // STATEMENTS
    // =====================

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone().unwrap(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(LetStatement { token, name, value }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        self.next_token();

        let return_value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(ReturnStatement {
            token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(ExpressionStatement { token, expression }))
    }

    // =====================
    // EXPRESSIONS (PRATT)
    // =====================

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = *self.prefix_parse_fns.get(&self.cur_token.token_type)?;

        let mut left = prefix(self)?;

        while self.peek_token.token_type != TokenType::SEMICOLON
            && (precedence < self.peek_precedence()
                || self.peek_token.token_type == TokenType::ASSIGN)
        {
            let infix = match self.infix_parse_fns.get(&self.peek_token.token_type) {
                Some(f) => *f,
                None => return Some(left),
            };

            self.next_token();
            left = infix(self, left)?;
        }

        Some(left)
    }

    // =====================
    // PREFIX PARSERS
    // =====================

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone().unwrap(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let value = token.literal.as_ref()?.parse::<i64>().ok()?;

        Some(Box::new(IntegerLiteral { token, value }))
    }

    fn parse_boolean(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let value = match token.token_type {
            TokenType::TRUE => true,
            TokenType::FALSE => false,
            _ => return None,
        };

        Some(Box::new(Boolean { token, value }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(exp)
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let token = self.cur_token.clone();
        let mut statements = vec![];

        self.next_token();

        while self.cur_token.token_type != TokenType::RBRACE
            && self.cur_token.token_type != TokenType::EOF
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Some(BlockStatement { token, statements })
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement()?;

        let mut alternative = None;

        if self.peek_token.token_type == TokenType::ELSE {
            self.next_token();

            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }

            alternative = Some(self.parse_block_statement()?);
        }

        Some(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token.token_type == TokenType::RPAREN {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone().unwrap(),
        };
        identifiers.push(ident);

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();

            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone().unwrap(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement()?;

        Some(Box::new(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.token_type.to_string();

        self.next_token();

        let right = self.parse_expression(Precedence::PREFIX)?;

        Some(Box::new(PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    // =====================
    // INFIX PARSERS
    // =====================

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut args = vec![];

        if self.peek_token.token_type == TokenType::RPAREN {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token.token_type == TokenType::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(args)
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let arguments = self.parse_call_arguments()?;

        Some(Box::new(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.token_type.to_string();
        let precedence = self.cur_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        }))
    }

    // =====================
    // PRECEDENCE
    // =====================

    fn peek_precedence(&self) -> Precedence {
        token_precedence(&self.peek_token.token_type)
    }

    fn cur_precedence(&self) -> Precedence {
        token_precedence(&self.cur_token.token_type)
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token.token_type == t {
            self.next_token();
            true
        } else {
            self.errors.0.push(ParserError::ExpectedToken {
                expected: t,
                found: self.peek_token.token_type.clone(),
            });
            false
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.token_type != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }
}

// =====================
// PRECEDENCE TABLE
// =====================

fn token_precedence(token: &TokenType) -> Precedence {
    match token {
        TokenType::EQ | TokenType::NOT_EQ => Precedence::EQUALS,
        TokenType::LT | TokenType::GT => Precedence::LESSGREATER,
        TokenType::PLUS | TokenType::MINUS => Precedence::SUM,
        TokenType::SLASH | TokenType::ASTERISK => Precedence::PRODUCT,
        TokenType::LPAREN => Precedence::CALL,
        _ => Precedence::LOWEST,
    }
}

pub fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    assert!(
        errors.0.is_empty(),
        "Parser has {} errors:\n{}",
        errors.0.len(),
        errors
    );
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

        let expected = ["x", "y", "foobar"];
        let expected_values = [5, 10, 838383];

        for ((stmt, expected_ident), expected_value) in program
            .statements
            .iter()
            .zip(expected.iter())
            .zip(expected_values.iter())
        {
            test_let_statement(&**stmt, expected_ident, expected_value);
        }
    }

    fn test_let_statement(stmt: &dyn Statement, expected_name: &str, expected_value: &i32) {
        assert_eq!(stmt.token_type(), TokenType::LET, "Wrong token type");

        let let_stmt = stmt
            .as_any()
            .downcast_ref::<LetStatement>()
            .expect("Expected LetStatement");

        assert_eq!(let_stmt.name.value, expected_name);
        assert_eq!(let_stmt.name.token_type(), TokenType::IDENT);

        assert_eq!(
            let_stmt
                .value
                .as_any()
                .downcast_ref::<IntegerLiteral>()
                .expect("Expected IntegerLiteral")
                .value,
            *expected_value as i64
        );
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            3,
            "Program.statements does not contain 3 statements"
        );

        let expected_return_values = [5, 10, 993322];

        for (stmt, expected_value) in program.statements.iter().zip(expected_return_values.iter()) {
            test_return_statement(&**stmt, *expected_value);
        }
    }

    #[test]
    fn test_parse_assignment_expression() {
        let input = "x = 5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<crate::ast::ExpressionStatement>()
            .expect("Expected ExpressionStatement");
        let infix = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<crate::ast::InfixExpression>()
            .expect("Expected InfixExpression");
        assert_eq!(infix.operator, "=");
        let left_ident = infix
            .left
            .as_any()
            .downcast_ref::<crate::ast::Identifier>()
            .expect("Expected Identifier on left");
        assert_eq!(left_ident.value, "x");
        let right_int = infix
            .right
            .as_any()
            .downcast_ref::<crate::ast::IntegerLiteral>()
            .expect("Expected IntegerLiteral on right");
        assert_eq!(right_int.value, 5);
    }

    fn test_return_statement(stmt: &dyn Statement, expected_value: i32) {
        assert_eq!(stmt.token_type(), TokenType::RETURN, "Wrong token type");

        let return_stmt = stmt
            .as_any()
            .downcast_ref::<ReturnStatement>()
            .expect("Expected ReturnStatement");

        assert_eq!(
            return_stmt
                .return_value
                .as_any()
                .downcast_ref::<IntegerLiteral>()
                .expect("Expected IntegerLiteral")
                .value,
            expected_value as i64
        );
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let ident = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token_type(), TokenType::IDENT);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let if_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .expect("Expected IfExpression");

        let condition = if_expr
            .condition
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Expected InfixExpression");
        assert_eq!(condition.operator.to_string(), "<");

        let left = condition
            .left
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(left.value, "x");

        let right = condition
            .right
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(right.value, "y");

        assert_eq!(
            if_expr.consequence.statements.len(),
            1,
            "ifExpr.consequence.statements does not contain 1 statement"
        );

        let consequence_stmt = &if_expr.consequence.statements[0];
        let consequence_expr_stmt = consequence_stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let consequence_expr = consequence_expr_stmt
            .expression
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(consequence_expr.value, "x");
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let if_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .expect("Expected IfExpression");

        assert!(if_expr.alternative.is_some(), "Expected alternative block");

        let alternative = if_expr.alternative.as_ref().unwrap();
        assert_eq!(
            alternative.statements.len(),
            1,
            "ifExpr.alternative.statements does not contain 1 statement"
        );

        let alternative_stmt = &alternative.statements[0];
        let alternative_expr_stmt = alternative_stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let alternative_expr = alternative_expr_stmt
            .expression
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(alternative_expr.value, "y");
    }

    #[test]
    fn test_function_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let function = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<FunctionLiteral>()
            .expect("Expected FunctionLiteral");

        assert_eq!(function.parameters.len(), 2, "Wrong number of parameters");

        let param1 = function.parameters[0]
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(param1.value, "x");

        let param2 = function.parameters[1]
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(param2.value, "y");

        assert_eq!(
            function.body.statements.len(),
            1,
            "Function body does not contain 1 statement"
        );

        let body_stmt = &function.body.statements[0];
        let body_expr_stmt = body_stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let body_expr = body_expr_stmt
            .expression
            .as_any()
            .downcast_ref::<InfixExpression>()
            .expect("Expected InfixExpression");

        assert_eq!(body_expr.operator.to_string(), "+");
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let stmt = &program.statements[0];
            let expr_stmt = stmt
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .expect("Expected ExpressionStatement");

            let function = expr_stmt
                .expression
                .as_any()
                .downcast_ref::<FunctionLiteral>()
                .expect("Expected FunctionLiteral");

            assert_eq!(
                function.parameters.len(),
                expected_params.len(),
                "Wrong number of parameters"
            );

            for (i, expected_param) in expected_params.iter().enumerate() {
                let param = function.parameters[i]
                    .as_any()
                    .downcast_ref::<Identifier>()
                    .expect("Expected Identifier");
                assert_eq!(param.value, *expected_param);
            }
        }
    }

    #[test]
    fn test_calling_function_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Program.statements does not contain 1 statement"
        );

        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect("Expected ExpressionStatement");

        let call_expr = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<CallExpression>()
            .expect("Expected CallExpression");

        let function = call_expr
            .function
            .as_any()
            .downcast_ref::<Identifier>()
            .expect("Expected Identifier");
        assert_eq!(function.value, "add");

        assert_eq!(call_expr.arguments.len(), 3, "Wrong number of arguments");
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.string();
            assert_eq!(actual, expected.to_string());
        }
    }

    #[test]
    fn test_parse_chained_assignment_expression() {
        let input = "a = b = 5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<crate::ast::ExpressionStatement>()
            .expect("Expected ExpressionStatement");
        let infix = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<crate::ast::InfixExpression>()
            .expect("Expected InfixExpression");
        // top-level operator should be '='
        assert_eq!(infix.operator, "=");
        let left_ident = infix
            .left
            .as_any()
            .downcast_ref::<crate::ast::Identifier>()
            .expect("Expected Identifier on left");
        assert_eq!(left_ident.value, "a");

        // right side should itself be an assignment b = 5
        let right_infix = infix
            .right
            .as_any()
            .downcast_ref::<crate::ast::InfixExpression>()
            .expect("Expected nested InfixExpression");
        assert_eq!(right_infix.operator, "=");
        let right_left = right_infix
            .left
            .as_any()
            .downcast_ref::<crate::ast::Identifier>()
            .expect("Expected Identifier on nested left");
        assert_eq!(right_left.value, "b");
        let right_right = right_infix
            .right
            .as_any()
            .downcast_ref::<crate::ast::IntegerLiteral>()
            .expect("Expected IntegerLiteral on nested right");
        assert_eq!(right_right.value, 5);
    }

    #[test]
    fn test_parse_assignment_applies_after_arithmetic() {
        let input = "a + b = c;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        let expr_stmt = stmt
            .as_any()
            .downcast_ref::<crate::ast::ExpressionStatement>()
            .expect("Expected ExpressionStatement");
        let infix = expr_stmt
            .expression
            .as_any()
            .downcast_ref::<crate::ast::InfixExpression>()
            .expect("Expected InfixExpression");
        // The parser treats assignment as binding to the right-hand side here,
        // so the top-level operator will be '+' and the RHS will be the assignment.
        assert_eq!(infix.operator, "+");
        let right_assign = infix
            .right
            .as_any()
            .downcast_ref::<crate::ast::InfixExpression>()
            .expect("Expected nested InfixExpression on right");
        assert_eq!(right_assign.operator, "=");
        let right_left = right_assign
            .left
            .as_any()
            .downcast_ref::<crate::ast::Identifier>()
            .expect("Expected Identifier on nested left");
        assert_eq!(right_left.value, "b");
        let right_right = right_assign
            .right
            .as_any()
            .downcast_ref::<crate::ast::Identifier>()
            .expect("Expected Identifier on nested right");
        assert_eq!(right_right.value, "c");
    }
}
