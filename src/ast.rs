use core::panic;
use std::{collections::HashMap, default, mem::transmute, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType},
    values::{
        AnyValue, AnyValueEnum,
        BasicValueEnum::{self, PointerValue},
        FunctionValue,
    },
};
use once_cell::sync::Lazy;
use regex::Regex;
#[derive(Default)]
pub enum ScopeType {
    #[default]
    Module,
    Function(FunctionValue<'static>),
    IF,
    Else,
    AssignmentBlock,
}
impl ScopeType {
    fn get_function_value(&self) -> &FunctionValue {
        match self {
            Self::Function(a) => a,
            _ => panic!(),
        }
    }
}
#[derive(Default)]
pub struct Scope {
    ty: ScopeType,
    parent_scope: Option<Box<Scope>>,
    defs: HashMap<String, (BasicTypeEnum<'static>, BasicValueEnum<'static>)>,
    types: HashMap<String, BasicTypeEnum<'static>>,
}

impl Scope {
    fn open_scope(self, ty: ScopeType) -> Self {
        let mut new_scope = Scope {
            ty,
            ..Default::default()
        };
        new_scope.parent_scope = Some(Box::new(self));
        new_scope
    }

    fn close_scope(self) -> Self {
        *self.parent_scope.unwrap()
    }

    fn get_value(
        &self,
        name: &String,
    ) -> Option<&(BasicTypeEnum<'static>, BasicValueEnum<'static>)> {
        let s = self.defs.get(name);
        if s.is_some() {
            return s;
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                let v = current_scope.defs.get(name);
                if v.is_some() {
                    return v;
                }
            }
        }

        None
    }

    fn set_value(
        &mut self,
        name: String,
        ty: BasicTypeEnum<'static>,
        value: BasicValueEnum<'static>,
    ) -> bool {
        self.defs.insert(name, (ty, value)).is_some()
    }
}
pub struct Backend {
    pub module: Module<'static>,
    pub context: &'static Context,
    pub builder: Builder<'static>,
    pub current_scope: Scope,
}

#[derive(Debug)]
pub enum Statement {
    Comment,
    VariableDecl(String, Option<String>, Vec<Statement>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
    If(Box<Statement>, Vec<Statement>, Vec<Statement>),
    Return(Box<Statement>),
    Identifier(String),
    Assignment(String, Vec<Statement>),
}
static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

impl Statement {
    pub fn get_value(&self, backend: &mut Backend) -> BasicValueEnum {
        match self {
            Statement::Comment => panic!(),
            Statement::VariableDecl(n, t, s) => {
                let ty = self.get_type(backend);
                let alloc = backend.builder.build_alloca(ty, &n).unwrap();

                match s.len() {
                    0 => {}
                    1 => {
                        let value = s[0].get_value(backend);
                        backend.builder.build_store(alloc, value).unwrap();
                    }
                    _ => todo!(),
                };
                backend
                    .current_scope
                    .set_value(n.to_string(), ty, PointerValue(alloc));
                BasicValueEnum::PointerValue(alloc)
            }
            Statement::Math(l, op, r) => {
                let l = l.get_value(backend);
                let r = r.get_value(backend);
                match op {
                    '+' => BasicValueEnum::IntValue(
                        backend
                            .builder
                            .build_int_add(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '-' => BasicValueEnum::IntValue(
                        backend
                            .builder
                            .build_int_sub(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '/' => BasicValueEnum::IntValue(
                        backend
                            .builder
                            .build_int_signed_div(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '*' => BasicValueEnum::IntValue(
                        backend
                            .builder
                            .build_int_mul(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '%' => BasicValueEnum::IntValue(
                        backend
                            .builder
                            .build_int_signed_rem(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    _ => todo!(),
                }
            }
            Statement::Num(n) => {
                BasicValueEnum::IntValue(backend.context.i64_type().const_int(*n as u64, true))
            }
            Statement::StringLet(lateral) => {
                BasicValueEnum::ArrayValue(backend.context.const_string(lateral.as_bytes(), false))
            }
            Statement::If(cond, stats, else_stats) => {
                let cond = cond.get_value(backend);

                let function = backend.current_scope.ty.get_function_value();
                let then_block = backend.context.append_basic_block(*function, "if_then");
                let else_block = backend.context.append_basic_block(*function, "if_else");
                let after_block = backend.context.append_basic_block(*function, "if_after");

                backend.builder.build_conditional_branch(
                    cond.into_int_value(),
                    then_block,
                    else_block,
                ).unwrap();

                backend.builder.position_at_end(then_block);
                for s in stats {
                    s.get_value(backend);
                }
                backend.builder.build_unconditional_branch(after_block);

                backend.builder.position_at_end(else_block);
                for s in else_stats {
                    s.get_value(backend);
                }
                backend.builder.build_unconditional_branch(after_block);

                backend.builder.position_at_end(after_block);

                panic!();
            }
            Statement::Return(s) => {
                let get_value = s.get_value(backend);
                backend
                    .builder
                    .build_return(Some(&get_value))
                    .unwrap();
                panic!();
            }
            Statement::Identifier(name) => {
                let (ty, mut val) = *backend.current_scope.get_value(name).unwrap();
                if !ty.is_pointer_type() && val.get_type().is_pointer_type() {
                    val = backend
                        .builder
                        .build_load(ty, val.into_pointer_value(), "load")
                        .unwrap();
                }
                val
            }
            Statement::Assignment(name, s) => {
                panic!();
            }
        }
    }

    pub fn get_type(&self, backend: &mut Backend) -> BasicTypeEnum<'static> {
        match self {
            Statement::Comment => panic!("Comment don't have type"),
            Statement::VariableDecl(_, ty, assignments) => {
                let def_ty = if let Some(t) = ty {
                    Some(Self::convert_type(t.clone(), backend.context))
                } else {
                    None
                };

                let assign_ty = match assignments.len() {
                    0 => panic!(),
                    1 => assignments[0].get_type(backend),
                    _ => {
                        let mut ty = None;
                        for s in assignments {
                            if s.is_returning() {
                                if ty.is_some() {
                                    let other_ty = s.get_type(backend);
                                    if ty.unwrap() != other_ty {
                                        panic!("all parts of block should return the same type");
                                    }
                                } else {
                                    ty = Some(s.get_type(backend));
                                }
                            }
                        }
                        ty.unwrap()
                    }
                };

                if let Some(t) = def_ty {
                    if t == assign_ty {
                        return t;
                    } else {
                        panic!();
                    }
                }
                assign_ty
            }
            Statement::Math(l, op, r) => {
                let get_type = l.get_type(backend);
                if get_type == r.get_type(backend) {
                    get_type
                } else {
                    panic!()
                }
            }
            Statement::Num(_) => BasicTypeEnum::IntType(backend.context.i64_type()),
            Statement::StringLet(s) => {
                BasicTypeEnum::ArrayType(backend.context.i8_type().array_type(s.len() as u32))
            }
            Statement::If(cond, stats, else_stats) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Identifier(idenf) => backend.current_scope.get_value(idenf).unwrap().0,
            Statement::Assignment(_, _) => todo!(),
        }
    }

    pub fn convert_type(ty: String, context: &'static Context) -> BasicTypeEnum<'static> {
        if let Some(c) = INT_TYPE.captures(&ty) {
            return BasicTypeEnum::IntType(context.custom_width_int_type(
                u32::from_str_radix(c.get(1).unwrap().as_str(), 10).unwrap(),
            ));
        }

        if let Some(c) = FLOAT_TYPE.captures(&ty) {
            match c.get(1).unwrap().as_str() {
                "f16" => return BasicTypeEnum::FloatType(context.f16_type()),
                "f32" => return BasicTypeEnum::FloatType(context.f32_type()),
                "f64" => return BasicTypeEnum::FloatType(context.f64_type()),
                "f128" => return BasicTypeEnum::FloatType(context.f128_type()),
                _ => panic!(),
            }
        }

        panic!("Can't Find Type {ty}");
    }

    pub fn convert_type_to_func(
        ty: &String,
        args: &Vec<(String, String)>,
        backend: &mut Backend,
    ) -> FunctionType<'static> {
        if ty == "()" {
            return backend.context.void_type().fn_type(
                &args
                    .into_iter()
                    .map(|a| Statement::convert_type(a.0.to_string(), backend.context).into())
                    .collect::<Vec<_>>(),
                false,
            );
        } else {
            return Self::convert_type(ty.to_string(), backend.context)
                .fn_type(
                    &args
                        .into_iter()
                        .map(|a| Statement::convert_type(a.0.to_string(), backend.context).into())
                        .collect::<Vec<_>>(),
                    false,
                );
        }
    }

    fn is_returning(&self) -> bool {
        match self {
            Statement::Comment => false,
            Statement::VariableDecl(_, _, _) => false,
            Statement::Math(_, _, _) => true,
            Statement::Num(_) => true,
            Statement::StringLet(_) => true,
            Statement::If(_, b, e) => {
                let mut returning_b = false;
                for s in b {
                    if s.is_returning() {
                        returning_b = true
                    }
                }
                let mut returning_e = false;
                for s in e {
                    if s.is_returning() {
                        returning_e = true
                    }
                }
                assert!(returning_b && returning_e);
                returning_b && returning_e
            }
            Statement::Return(_) => true,
            Statement::Identifier(_) => true,
            Statement::Assignment(_, _) => false,
        }
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
                let func = backend.module.add_function(name, ty, None);
                let basic_block = backend.context.append_basic_block(func, "entry");
                backend.builder.position_at_end(basic_block);

                backend.current_scope.open_scope(ScopeType::Function(func));
                for stat in stats {
                    if let Statement::Comment {} = stat {
                    } else {
                        stat.get_value(backend);
                    }
                }

                backend.builder.build_return(None).unwrap();
            }
            AST::Module(l) => {
                for ast in l {
                    ast.gen_code(backend)
                }
            }
        }
        backend.module.print_to_stderr();
    }
}
