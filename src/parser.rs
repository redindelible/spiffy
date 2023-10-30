use crate::ast::{Block, Expr, File, Function, Stmt, TopLevel};
use crate::lexer::{Lexer, Token, TokenType};
use crate::stream::Stream;

pub struct Parser<'a> {
    lexer: Stream<Lexer<'a>>
}

impl<'a> Parser<'a> {
    pub fn parse(text: &'a str) -> File {
        Parser::new(text).parse_file()
    }

    fn new(text: &'a str) -> Parser<'a> {
        Parser { lexer: Stream::new(Lexer::new(text)) }
    }

    fn curr(&mut self) -> Token {
        self.lexer.peek().cloned().unwrap_or_else(|| Token { text: "\0".to_owned(), ttype: TokenType::EOF})
    }

    fn peek(&mut self, ttype: TokenType) -> bool {
        self.curr().ttype == ttype
    }

    fn expect(&mut self, ttype: TokenType) -> Token {
        let curr = self.curr();
        if curr.ttype == ttype {
            self.lexer.next();
            return curr;
        } else {
            panic!("Expected {:?}, got {:?}", ttype, curr.ttype);
        }
    }

    fn parse_file(&mut self) -> File {
        let mut top_levels = vec![];
        while !self.peek(TokenType::EOF) {
            if self.peek(TokenType::Fn) {
                top_levels.push(TopLevel::Function(self.parse_function()))
            } else {
                panic!("Could not parse a top level")
            }
        }
        File { top_levels }
    }

    fn parse_function(&mut self) -> Function {
        self.expect(TokenType::Fn);
        let name = self.expect(TokenType::Ident).text;

        self.expect(TokenType::LeftParen);
        let mut parameters = vec![];
        self.expect(TokenType::RightParen);
        let block = self.parse_block();

        Function { name, parameters, block }
    }

    fn parse_block(&mut self) -> Block {
        self.expect(TokenType::LeftBrace);
        let mut stmts = vec![];
        while !self.peek(TokenType::RightBrace) {
            stmts.push(self.parse_stmt());
        }
        self.expect(TokenType::RightBrace);
        Block { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        if self.peek(TokenType::Return) {
            self.expect(TokenType::Return);
            let expr = self.parse_expr();
            self.expect(TokenType::Semicolon);
            Stmt::Return(Box::new(expr))
        } else {
            let expr = self.parse_expr();
            self.expect(TokenType::Semicolon);
            Stmt::Expr(Box::new(expr))
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_terminal()
    }

    fn parse_expr_terminal(&mut self) -> Expr {
        if self.peek(TokenType::Number) {
            let number = self.expect(TokenType::Number).text.parse::<i32>().unwrap();
            Expr::Number(number)
        } else {
            panic!("Could not parse as an expression")
        }
    }
}
