pub mod ast;
pub mod lexer;
use std::fs;

use lalrpop_util::{lalrpop_mod, ParseError::{ExtraToken, InvalidToken, UnrecognizedEof, UnrecognizedToken, User}};

lalrpop_mod!(pub grammer);
use inkwell::
    context::Context
;
use lexer::Lexer;

use crate::ast::{Backend, Scope};

use argh::FromArgs;
#[derive(FromArgs)]
/// The Serk Complier
struct Input {
    ///file to be complied/JITed
    #[argh(option, default = "String::from(\"tests/test6.serk\")")]
    pub file: String,
}

fn main() {
    let cli: Input = argh::from_env();
    let input = fs::read_to_string(cli.file).unwrap();
    let output = grammer::ModuleParser::new().parse(Lexer::new(&input));
    match output {
        Ok(s) => {
            // println!("{:?}", s);
            let context = Context::create();
            let mut module = context.create_module("Hero");
            let mut builder = context.create_builder();
            Backend {
                module: &mut module,
                context: &context,
                builder: &mut builder,
                current_scope: Some(Box::new(Scope::default())),
            }.gen_code(*s);
            let _ = module.verify().unwrap();
            let exc = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
            let func = unsafe { exc.get_function::<unsafe extern "C" fn() -> i64>("Something").unwrap() };
        }
        Err(s) => {
            println!("Error: {:?}", s);
            match s {
                InvalidToken { location } => println!("{:?}", input.split_at(location)),
                UnrecognizedEof { location: _, expected:_ } => {},
                UnrecognizedToken { token, expected: _ } => println!("{:?}", input.split_at(token.0)),
                ExtraToken { token } => println!("{:?}", input.split_at(token.0)),
                User { error:_ } => for a in Lexer::new(&input){println!("{:?}",a)},
            }
        }
    }
}

use goldentests::{TestConfig, TestResult};

#[test]
fn run_golden_tests() -> TestResult<()> {
    let mut config = TestConfig::new("target/debug/Serk.exe", "tests", "// ")?;
    config.overwrite_tests = true;
    config.run_tests()
}
