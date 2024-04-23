use core::panic;
use std::{cell::RefCell, collections::HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicValueEnum::{self, PointerValue},
        FunctionValue,
    },
};
use once_cell::sync::Lazy;
use regex::Regex;
#[derive(Default)]
pub enum ScopeType<'ctx> {
    #[default]
    Module,
    Function(FunctionValue<'ctx>),
    IF,
    Else,
    AssignmentBlock,
}
impl ScopeType<'_> {
    fn get_function_value(&self) -> &FunctionValue {
        match self {
            Self::Function(a) => a,
            _ => panic!(),
        }
    }
}
#[derive(Default)]
pub struct Scope<'ctx> {
    ty: ScopeType<'ctx>,
    parent_scope: Option<Box<Scope<'ctx>>>,
    defs: HashMap<String, (BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
    types: HashMap<String, BasicTypeEnum<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    fn open_scope(self, ty: ScopeType<'ctx>) -> Self {
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
    ) -> Option<&(BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)> {
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
        ty: BasicTypeEnum<'ctx>,
        value: BasicValueEnum<'ctx>,
    ) -> bool {
        self.defs.insert(name, (ty, value)).is_some()
    }
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


#[derive(Debug)]
pub enum AST {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Vec<Statement>),
}


static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

pub struct Backend<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a mut  Builder<'ctx>,
    pub module: &'a mut Module<'ctx>,
    pub current_scope: RefCell<Scope<'ctx>>
}


impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn get_value(&self, stat: &Statement) -> BasicValueEnum<'ctx> {
        match stat {
            Statement::Comment => panic!(),
            Statement::VariableDecl(n, t, s) => {
                let ty = self.get_type(&stat);
                let alloc = self.builder.build_alloca(ty, &n).unwrap();

                match s.len() {
                    0 => {}
                    1 => {
                        let value = self.get_value(&s[0]);
                        self.builder.build_store(alloc, value).unwrap();
                    }
                    _ => todo!(),
                };
                self.current_scope.borrow_mut()
                    .set_value(n.to_string(), ty, PointerValue(alloc));
                BasicValueEnum::PointerValue(alloc)
            }
            Statement::Math(l, op, r) => {
                let l = self.get_value(l);
                let r = self.get_value(r);
                match op {
                    '+' => BasicValueEnum::IntValue(
                        self
                            .builder
                            .build_int_add(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '-' => BasicValueEnum::IntValue(
                        self
                            .builder
                            .build_int_sub(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '/' => BasicValueEnum::IntValue(
                        self
                            .builder
                            .build_int_signed_div(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '*' => BasicValueEnum::IntValue(
                        self
                            .builder
                            .build_int_mul(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    '%' => BasicValueEnum::IntValue(
                        self
                            .builder
                            .build_int_signed_rem(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    ),
                    _ => todo!(),
                }
            }
            Statement::Num(n) => {
                BasicValueEnum::IntValue(self.context.i64_type().const_int(*n as u64, true))
            }
            Statement::StringLet(lateral) => {
                BasicValueEnum::ArrayValue(self.context.const_string(lateral.as_bytes(), false))
            }
            Statement::If(cond, stats, else_stats) => {
                let cond = self.get_value(cond);

                let borrow_mut = self.current_scope.borrow_mut();
                let function = borrow_mut.ty.get_function_value();
                let then_block = self.context.append_basic_block(*function, "if_then");
                let else_block = self.context.append_basic_block(*function, "if_else");
                let after_block = self.context.append_basic_block(*function, "if_after");

                self.builder.build_conditional_branch(
                    cond.into_int_value(),
                    then_block,
                    else_block,
                ).unwrap();

                self.builder.position_at_end(then_block);
                for s in stats {
                    self.get_value(s);
                }
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(else_block);
                for s in else_stats {
                     self.get_value(s);

                }
                self.builder.build_unconditional_branch(after_block);

                self.builder.position_at_end(after_block);

                panic!();
            }
            Statement::Return(s) => {
                let get_value = self.get_value(s);
                self
                    .builder
                    .build_return(Some(&get_value))
                    .unwrap();
                panic!();
            }
            Statement::Identifier(name) => {
                let (ty, mut val) = *self.current_scope.borrow_mut().get_value(&name).unwrap();
                if !ty.is_pointer_type() && val.get_type().is_pointer_type() {
                    val = self
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

    pub fn get_type(&self, stat: &Statement) -> BasicTypeEnum<'ctx> {
        match stat {
            Statement::Comment => panic!("Comment don't have type"),
            Statement::VariableDecl(_, ty, assignments) => {
                let def_ty = if let Some(t) = ty {
                    Some(Self::convert_type(t.clone(), self.context))
                } else {
                    None
                };

                let assign_ty = match assignments.len() {
                    0 => panic!(),
                    1 => self.get_type(&assignments[0]),
                    _ => {
                        let mut ty = None;
                        for s in assignments {
                            if Self::is_returning(stat) {
                                if ty.is_some() {
                                    let other_ty = self.get_type(s);
                                    if ty.unwrap() != other_ty {
                                        panic!("all parts of block should return the same type");
                                    }
                                } else {
                                    ty = Some(self.get_type(s));
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
                let get_type = self.get_type(l);
                if get_type == self.get_type(r) {
                    get_type
                } else {
                    panic!()
                }
            }
            Statement::Num(_) => BasicTypeEnum::IntType(self.context.i64_type()),
            Statement::StringLet(s) => {
                BasicTypeEnum::ArrayType(self.context.i8_type().array_type(s.len() as u32))
            }
            Statement::If(cond, stats, else_stats) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Identifier(idenf) => self.current_scope.borrow_mut().get_value(idenf).unwrap().0,
            Statement::Assignment(_, _) => todo!(),
        }
    }

    pub fn convert_type(ty: String, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
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

    pub fn convert_type_to_func(&self,
        ty: &String,
        args: &Vec<(String, String)>,
    ) -> FunctionType<'ctx> {
        if ty == "()" {
            return self.context.void_type().fn_type(
                &args
                    .into_iter()
                    .map(|a| Self::convert_type(a.0.to_string(), self.context).into())
                    .collect::<Vec<_>>(),
                false,
            );
        } else {
            return Self::convert_type(ty.to_string(), self.context)
                .fn_type(
                    &args
                        .into_iter()
                        .map(|a| Self::convert_type(a.0.to_string(), self.context).into())
                        .collect::<Vec<_>>(),
                    false,
                );
        }
    }

    pub fn gen_code(&self, ast: AST) {
        match ast {
            AST::Function(name, ty, args, stats) => {
                let ty = self.convert_type_to_func(&ty,&args);
                let func = self.module.add_function(&name, ty, None);
                let basic_block = self.context.append_basic_block(func, "entry");
                self.builder.position_at_end(basic_block);

                self.current_scope.replace(self.current_scope.take().open_scope(ScopeType::Function(func)));
                for stat in stats {
                    if let Statement::Comment {} = stat {
                    } else {
                        self.get_value(&stat);
                    }
                }

                self.builder.build_return(None).unwrap();
            }
            AST::Module(l) => {
                for ast in l {
                    self.gen_code(*ast);
                }
            }
        }
        self.module.print_to_stderr();
    }
}
