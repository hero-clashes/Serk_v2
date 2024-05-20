pub mod ast;
pub mod lexer;
pub mod scope;
pub mod statement;
pub mod backend;
use std::fs;

use backend::Backend;
use codespan_reporting::files::SimpleFiles;
use lalrpop_util::{lalrpop_mod, ParseError::{ExtraToken, InvalidToken, UnrecognizedEof, UnrecognizedToken, User}};

lalrpop_mod!(pub grammer);
use inkwell::{
    context::Context, targets::{InitializationConfig, Target, TargetMachine}}
;
use lexer::Lexer;
use scope::Scope;


use argh::FromArgs;
#[derive(FromArgs,Clone)]
/// The Serk Complier
struct Input {
    ///file to be complied/JITed
    #[argh(option, default = "String::from(\"tests/test7.serk\")")]
    pub file: String,
    ///run Code in Jit Mode
    #[argh(option, short = 'j', default = "true")]
    pub jit: bool
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
            let mut module = context.create_module(&cli.file);
            let mut builder = context.create_builder();
            Target::initialize_x86(&InitializationConfig::default());
            let get_default_triple = TargetMachine::get_default_triple();
            let target_machine: TargetMachine = Target::from_triple(&get_default_triple)
                .unwrap()
                .create_target_machine(
                    &get_default_triple,
                    "",
                    "",
                    inkwell::OptimizationLevel::None,
                    inkwell::targets::RelocMode::Default,
                    inkwell::targets::CodeModel::JITDefault,
                )
                .unwrap();
            Backend {
                module: &mut module,
                context: &context,
                builder: &mut builder,
                current_scope: Some(Box::new(Scope::default())),
                current_file: file_id,
                files,
                target_machine
            }.gen_code(*s);
            if cli.jit {
                let exc = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
                let func = unsafe { exc.get_function::<unsafe extern "C" fn() -> i64>("main").unwrap() };
                println!("Main output: {:?}",unsafe{func.call()});
            }
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
#[allow(unused_imports)]
use goldentests::{TestConfig, TestResult};

#[test]
fn run_golden_tests() -> TestResult<()> {
    let mut config = TestConfig::new("target/debug/Serk.exe", "tests", "// ")?;
    config.overwrite_tests = true;
    config.run_tests()
}
