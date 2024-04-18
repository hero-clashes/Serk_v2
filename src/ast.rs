use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, FunctionType}, values::{AnyValueEnum, BasicValueEnum},
};
use once_cell::sync::Lazy;
use regex::Regex;



pub struct Backend{
    pub module: Module<'static>,
    pub context: &'static Context,
    pub builder: Builder<'static>,
}



#[derive(Debug)]
pub enum Statement {
    Comment,
    VariableDecl(String, Option<String>, Box<Vec<Statement>>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
}
static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

impl Statement {
    pub fn get_value(&self, backend: &mut Backend)-> BasicValueEnum{
        match self {
            Statement::Comment => panic!(),
            Statement::VariableDecl(n, t, s) => {
                let alloc = backend.builder.build_alloca(self.get_type(backend.context), &n).unwrap();

                match s.len() {
                    0 => {}
                    1 => {let value = s[0].get_value(backend); backend.builder.build_store(alloc, value).unwrap();},                    
                    _ => todo!()
                };                




                backend.builder.build_load(self.get_type(backend.context), alloc, "load").unwrap()
            },
            Statement::Math(l, op, r) => {
                let l = l.get_value(backend);
                let r = r.get_value(backend);
                match op {
                    '+'=>BasicValueEnum::IntValue(backend.builder.build_int_add(l.into_int_value(), r.into_int_value(), "Adding").unwrap()),
                    '-'=>BasicValueEnum::IntValue(backend.builder.build_int_sub(l.into_int_value(), r.into_int_value(), "Adding").unwrap()),
                    '/'=>BasicValueEnum::IntValue(backend.builder.build_int_signed_div(l.into_int_value(), r.into_int_value(), "Adding").unwrap()),
                    '*'=>BasicValueEnum::IntValue(backend.builder.build_int_mul(l.into_int_value(), r.into_int_value(), "Adding").unwrap()),
                    '%'=>BasicValueEnum::IntValue(backend.builder.build_int_signed_rem(l.into_int_value(), r.into_int_value(), "Adding").unwrap()),
                    _ => todo!(),
                }
            },
            Statement::Num(n) => BasicValueEnum::IntValue(backend.context.i64_type().const_int(*n as u64, true)),
            Statement::StringLet(lateral) => BasicValueEnum::ArrayValue(backend.context.const_string(lateral.as_bytes(), false)),
        }
    }



    pub fn get_type(&self, context: &'static Context) -> BasicTypeEnum<'static> {
        match self {
            Statement::Comment => panic!("Comment don't have type"),
            Statement::VariableDecl(name, ty, assignments) => {
                //TODO infer the type form the assignments
                match ty {
                    Some(s) => Self::convert_type(s.clone(), context),
                    None => todo!(),
                }
            }
            Statement::Math(_, _, _) => todo!(),
            Statement::Num(n) => todo!(),
            Statement::StringLet(s) => inkwell::types::BasicTypeEnum::ArrayType(context.i8_type().array_type(s.len() as u32)),
        }
    }

    pub fn convert_type(ty: String, context: &'static Context) -> BasicTypeEnum<'static> {
        if let Some(c) = INT_TYPE.captures(&ty) {
            return inkwell::types::BasicTypeEnum::IntType(context.custom_width_int_type(
                u32::from_str_radix(c.get(1).unwrap().as_str(), 10).unwrap(),
            ));
        }

        // if ty == "()" {return  context.void_type()};
        panic!("Can't Find Type {ty}");
    }

    pub fn convert_type_to_func(
        ty: &String,
        args: &Vec<(String, String)>,
        backend: &mut Backend,
    ) -> FunctionType<'static> {
        if let Some(c) = INT_TYPE.captures(&ty) {
            return backend.context
                .custom_width_int_type(u32::from_str_radix(c.get(1).unwrap().as_str(), 10).unwrap())
                .fn_type(
                    &args
                        .into_iter()
                        .map(|a| Statement::convert_type(a.0.to_string(), backend.context).into())
                        .collect::<Vec<_>>(),
                    false,
                );
        }

        if ty == "()" {
            return backend.context.void_type()
            .fn_type(
                &args
                    .into_iter()
                    .map(|a| Statement::convert_type(a.0.to_string(), backend.context).into())
                    .collect::<Vec<_>>(),
                false,
            );
        }

        panic!("Can't convert to FunctionType")
    }
}
#[derive(Debug)]
pub enum AST {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Vec<Statement>),
}

impl AST {
    pub fn gen_code(&self, backend: &mut Backend) {
        match self {
            AST::Function(name, ty, args, stats) => {
                let ty = Statement::convert_type_to_func(ty, args, backend);
                let func = backend.module.add_function(
                    name,
                    ty,
                    None,
                );
                let basic_block = backend.context.append_basic_block(func, "entry");
                backend.builder.position_at_end(basic_block);

                for stat in stats{
                    if let Statement::Comment{} = stat{

                    } else{
                        stat.get_value(backend);
                    }
                }

                backend.builder.build_return(None).unwrap();
            }
            AST::Module(l) => todo!(),
        }
        backend.module.print_to_stderr();
    }
}
