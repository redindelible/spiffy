use std::str::Chars;
use crate::stream::Stream;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenType {
    Ident,
    Number,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,
    Return,
    Fn,
    EOF
}

#[derive(Debug, Clone)]
pub struct Token {
    pub text: String,
    pub ttype: TokenType
}

const BASIC_TOKENS: phf::Map<char, TokenType> = phf::phf_map! {
    '{' => TokenType::LeftBrace,
    '}' => TokenType::RightBrace,
    '(' => TokenType::LeftParen,
    ')' => TokenType::RightParen,
    ';' => TokenType::Semicolon
};

const KEYWORDS: phf::Map<&'static str, TokenType> = phf::phf_map! {
    "fn" => TokenType::Fn,
    "return" => TokenType::Return,
};

pub struct Lexer<'a> {
    chars: Stream<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Lexer<'a> {
        Lexer { chars: Stream::new(text.chars()) }
    }

    fn is_done(&mut self) -> bool {
        self.chars.check_done()
    }

    fn curr(&mut self) -> char {
        self.chars.peek().copied().unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        self.chars.next().unwrap_or('\0')
    }

    fn lex_token(&mut self) -> Option<Token> {
        while !self.is_done() {
            if self.curr().is_ascii_whitespace() {
                self.advance();
            } else if self.curr().is_ascii_alphabetic() || self.curr() == '_' {
                let mut ident = String::new();
                while self.curr().is_ascii_alphanumeric() || self.curr() == '_' {
                    ident.push(self.advance());
                }
                let ttype = *KEYWORDS.get(&ident).unwrap_or(&TokenType::Ident);
                return Some(Token { text: ident, ttype })
            } else if self.curr().is_ascii_digit() {
                let mut ident = String::new();
                while self.curr().is_ascii_digit() {
                    ident.push(self.advance());
                }
                return Some(Token { text: ident, ttype: TokenType::Number })
            } else if let Some(&ttype) = BASIC_TOKENS.get(&self.curr()) {
                return Some(Token { text: self.advance().to_string(), ttype })
            } else {
                panic!("Unexpected character: {}", self.curr())
            }
        }

        None
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}
