use std::ops::Range;

#[derive(Debug)]
pub struct Statement{
    pub loc: (usize,usize),
    pub data: StatementData,
}
impl Statement{
    pub fn new(loc:(usize,usize), data:StatementData)->Self{
        Statement { loc, data }
    }
    pub fn to_rng(&self) -> Range<usize>{
        self.loc.0..self.loc.1
    }
}
#[derive(Debug)]
pub enum StatementData {
    VariableDecl(String, Option<String>, Box<Statement>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
    If(Box<Statement>, Box<Statement>, Box<Statement>),
    Return(Option<Box<Statement>>),
    Identifier(String),
    Assignment(String, Box<Statement>),
    Block(Vec<Statement>),
    BoolLet(bool),
    PreFix(char, Box<Statement>),
    While(Box<Statement>, Box<Statement>),
    Break,
    Continue,
    Call(String, Vec<Statement>),
    Yield(Option<Box<Statement>>),
    For(String, Box<Statement>, Box<Statement>)
}
