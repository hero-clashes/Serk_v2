pub mod ast;
use lalrpop_util::{lalrpop_mod, state_machine::ParseError};

lalrpop_mod!(pub grammer);
use inkwell::context::Context;

fn main() {
    let input = "fn Something(any key,word some){
        //None sense Comment
        var s;
    }";
    let output = grammer::ModuleParser::new().parse(input);
    match output {
        Ok(s) => println!("{:?}", s),
        Err(s) => {
            println!("Error: {:?}", s);
            match s{
                lalrpop_util::ParseError::InvalidToken { location } => println!("{:?}",input.split_at(location)),
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => todo!(),
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => println!("{:?}",input.split_at(token.0)),
                lalrpop_util::ParseError::ExtraToken { token } => todo!(),
                lalrpop_util::ParseError::User { error } => todo!(),
            }
        }
    }
}

#[test]
fn tests() {}
