
#[derive(Debug)]
pub enum Statement{
    Comment,
    VariableDecl(String,String, Box<Vec<Statement>>),
}
#[derive(Debug)]
pub enum AST{
    Function(String,Vec<(String,String)>,Vec<Statement>)
}








