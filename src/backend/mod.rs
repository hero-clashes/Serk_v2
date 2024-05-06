use std::process;

use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles, term::{self, termcolor::{ColorChoice, StandardStream}}};
use inkwell::{
    builder::Builder, context::Context, module::Module
};


use crate::scope::*;


pub mod ast_gen;
pub mod type_gen;
pub mod value_gen;



pub struct Backend<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a mut Builder<'ctx>,
    pub module: &'a mut Module<'ctx>,
    pub current_scope: Option<Box<Scope<'ctx>>>,
    pub current_file: usize,
    pub files: SimpleFiles<String,String>,
}


impl<'a, 'ctx> Backend<'a, 'ctx> {
    fn print_error(&self, d:Diagnostic<usize>) -> !{  
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer.lock(), &config, &self.files, &d).unwrap();
        process::exit(1)
    }
}