pub mod ast;
pub mod lexer;
use std::fs;

use codespan_reporting::files::SimpleFiles;
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
    #[argh(option, default = "String::from(\"tests/test7.serk\")")]
    pub file: String,
}

fn main() {
    let cli: Input = argh::from_env();
    let mut files = SimpleFiles::new();
    let input = fs::read_to_string(cli.file.clone()).unwrap();
    let file_id = files.add(cli.file.clone(), input.clone());
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
                current_file: file_id,
                files
            }.gen_code(*s);
            let _ = module.verify().unwrap();
            let exc = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
            let func = unsafe { exc.get_function::<unsafe extern "C" fn() -> i64>("Something").unwrap() };
            println!("output: {:?}, Expected: {}",unsafe{func.call()}, (0..10).sum::<i64>());
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
#[allow(clippy::all)]
use goldentests::{TestConfig, TestResult};

#[test]
fn run_golden_tests() -> TestResult<()> {
    let mut config = TestConfig::new("target/debug/Serk.exe", "tests", "// ")?;
    config.overwrite_tests = true;
    config.run_tests()
}
