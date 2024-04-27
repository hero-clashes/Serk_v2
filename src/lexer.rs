use std::fmt; // to implement the Display trait later
use std::num::ParseIntError;
use logos::{Logos, Skip, SpannedIter};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexicalError)]
pub enum Token {
    #[regex("(//.+)", |_| Skip)]
    Comment,
    #[regex(r"([ \r\t\n\f]+)", |_| Skip)]
    Skipping,
    #[token("fn")]
    KeywordFn,
    #[token("var")]
    KeywordVar,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,
    #[token("return")]
    KeywordReturn,
    #[token("true")]
    KeywordTrue,
    #[token("false")]
    KeywordFalse,
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Num(i64),
    #[regex(r#""(.+)""#, |lex| lex.slice().to_string())]
    StringLet(String),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("{")]
    LPrace,
    #[token("}")]
    RPrace,
    
    #[token("=")]
    Assign,

    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessOrEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterOrEqual,
    #[token("!")]
    Not,

    #[token("&&")]
    And,
    #[token("||")]
    Or,

    #[token("->")]
    Arrow,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
    
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}

