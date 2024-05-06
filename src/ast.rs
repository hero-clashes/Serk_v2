use std::ops::Range;

use crate::statement::Statement;

#[derive(Debug)]
pub struct AST{
    pub data: ASTData,
    pub loc: (usize,usize)
}
impl AST{
    pub fn new(loc: (usize,usize),data: ASTData) -> Self {
        Self { data, loc }
    }
    pub fn to_rng(&self) -> Range<usize>{
        self.loc.0..self.loc.1
    }
}
#[derive(Debug)]
pub enum ASTData {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Statement),
    GenFunction(Box<ASTData>),
}