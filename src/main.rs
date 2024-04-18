pub mod ast;
use std::{fs::File, io::Read};

use lalrpop_util::{lalrpop_mod, state_machine::ParseError};

lalrpop_mod!(pub grammer);
use inkwell::{builder, context::Context, module::{self, Module}};
use once_cell::unsync::Lazy;

use crate::ast::Backend;

fn main() {
    let mut input = String::new();
    let _ = File::open("tests/test.serk")
        .unwrap()
        .read_to_string(&mut input);
    let output = grammer::ModuleParser::new().parse(&input);
    match output {
        Ok(s) => {
            println!("{:?}", s);
            let context: &'static Context =
                unsafe { std::mem::transmute(Box::leak(Box::new(Context::create()))) };
            let module = context.create_module("Hero");
            let builder = context.create_builder();
            
            s.gen_code(&mut Backend{ module, context, builder });
        }
        Err(s) => {
            println!("Error: {:?}", s);
            match s {
                lalrpop_util::ParseError::InvalidToken { location } => {
                    println!("{:?}", input.split_at(location))
                }
                lalrpop_util::ParseError::UnrecognizedEof { location, expected } => todo!(),
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                    println!("{:?}", input.split_at(token.0))
                }
                lalrpop_util::ParseError::ExtraToken { token } => todo!(),
                lalrpop_util::ParseError::User { error } => todo!(),
            }
        }
    }
}

#[test]
fn tests() {}
