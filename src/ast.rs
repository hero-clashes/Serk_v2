use std::io::SeekFrom;


#[derive(Debug)]
pub enum Statement{
    Comment,
    VariableDecl(String,String, Box<Vec<Statement>>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String)
}
#[derive(Debug)]
pub enum AST{
    Function(String,String,Vec<(String,String)>,Vec<Statement>)
}


impl AST{
    pub fn conv(&self) {
        match self{
            AST::Function(name,ty, args, stats) => {

            },
        }
    }
}





