use core::panic;
use std::{cell::RefCell, collections::HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{
        AsValueRef, BasicValueEnum::{self, PointerValue}, FunctionValue
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
    AssignmentBlock(String),
}
impl<'ctx> ScopeType<'ctx> {
    fn get_function_value(&self) -> &'ctx FunctionValue {
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

    fn get_value(&self, name: &String) -> Option<&(BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)> {
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
    VariableDecl(String, Option<String>, Box<Statement>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
    If(Box<Statement>, Box<Statement>, Box<Statement>),
    Return(Box<Statement>),
    Identifier(String),
    Assignment(String, Box<Statement>),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum AST {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Statement),
}

static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

pub struct Backend<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a mut Builder<'ctx>,
    pub module: &'a mut Module<'ctx>,
    pub current_scope: RefCell<Scope<'ctx>>,
}

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn get_value(&self, stat: &Statement) -> Option<BasicValueEnum<'ctx>> {
        match stat {
            Statement::Comment => panic!(),
            Statement::VariableDecl(n, t, s) => {
                let ty = self.get_type(&stat).unwrap();
                let alloc = self.builder.build_alloca(ty, &n).unwrap();

                let value = self.get_value(&s).unwrap();
                self.builder.build_store(alloc, value).unwrap();

                self.current_scope
                    .borrow_mut()
                    .set_value(n.to_string(), ty, PointerValue(alloc));
                Some(BasicValueEnum::PointerValue(alloc))
            }
            Statement::Math(l, op, r) => {
                let l = self.get_value(l).unwrap();
                let r = self.get_value(r).unwrap();
                match op {
                    '+' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_add(l.into_int_value(), r.into_int_value(), "Adding")
                            .unwrap(),
                    )),
                    '-' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_sub(l.into_int_value(), r.into_int_value(), "Subbing")
                            .unwrap(),
                    )),
                    '/' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_signed_div(l.into_int_value(), r.into_int_value(), "Dividing")
                            .unwrap(),
                    )),
                    '*' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_mul(l.into_int_value(), r.into_int_value(), "Multiplying")
                            .unwrap(),
                    )),
                    '%' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_signed_rem(l.into_int_value(), r.into_int_value(), "Reminder")
                            .unwrap(),
                    )),
                    'e' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::EQ,l.into_int_value(), r.into_int_value(), "Comparing")
                            .unwrap(),
                    )),
                    _ => todo!(),
                }
            }
            Statement::Num(n) => {
                Some(BasicValueEnum::IntValue(self.context.i64_type().const_int(*n as u64, true)))
            }
            Statement::StringLet(lateral) => {
                Some(BasicValueEnum::ArrayValue(self.context.const_string(lateral.as_bytes(), false)))
            }
            Statement::If(cond, stats, else_stats) => {
                let cond = self.get_value(cond).unwrap();

                let borrow_mut = self.current_scope.borrow_mut();
                let function = unsafe { FunctionValue::new(borrow_mut.ty.get_function_value().as_value_ref()).unwrap() };
                drop(borrow_mut);
                let then_block = self.context.append_basic_block(function, "if_then");
                let else_block = self.context.append_basic_block(function, "if_else");
                let after_block = self.context.append_basic_block(function, "if_after");
                self.builder
                    .build_conditional_branch(cond.into_int_value(), then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                self.current_scope.replace(
                    self.current_scope
                        .take()
                        .open_scope(ScopeType::IF),
                );
                self.get_value(stats);
                self.current_scope.replace(
                    self.current_scope.take().close_scope()
                );
                self.builder.build_unconditional_branch(after_block).unwrap();

                self.builder.position_at_end(else_block);
                self.current_scope.replace(
                    self.current_scope
                        .take()
                        .open_scope(ScopeType::Else),
                );
                self.get_value(else_stats);
                self.current_scope.replace(
                    self.current_scope.take().close_scope()
                );
                self.builder.build_unconditional_branch(after_block).unwrap();

                self.builder.position_at_end(after_block);

                None
            }
            Statement::Return(s) => {
                let get_value = self.get_value(s).unwrap();

                if matches!(self.current_scope.borrow_mut().ty, ScopeType::Function(_)){
                    self.builder.build_return(Some(&get_value)).unwrap();
                }else if self.current_scope.borrow_mut().parent_scope.as_ref().is_some() {
                    match &self.current_scope.borrow_mut().parent_scope.as_ref().unwrap().ty{
                        ScopeType::AssignmentBlock(s) => {self.builder.build_store(self.current_scope.borrow_mut().get_value(&s).unwrap().1.into_pointer_value(), get_value).unwrap();},
                        _ => {}
                    };

                }
                None
            }
            Statement::Identifier(name) => {
                let (ty, mut val) = *self.current_scope.borrow_mut().get_value(&name).unwrap();
                if !ty.is_pointer_type() && val.get_type().is_pointer_type() {
                    val = self
                        .builder
                        .build_load(ty, val.into_pointer_value(), "load")
                        .unwrap();
                }
                Some(val)
            }
            Statement::Assignment(name, s) => {
                self.current_scope.replace(
                    self.current_scope
                        .take()
                        .open_scope(ScopeType::AssignmentBlock(name.clone())),
                );
                if let Some(val) = self.get_value(s){
                    self.builder.build_store(self.current_scope.borrow_mut().get_value(&name).unwrap().1.into_pointer_value(), val).unwrap();
                };
                self.current_scope.replace(
                    self.current_scope.take().close_scope()
                );
                None
            }
            Statement::Block(stats) => {
                if stats.len() == 1{
                    return self.get_value(&stats[0])
                }

                for stat in stats{
                    self.get_value(stat);
                }

                None
            },
        }
    }

    pub fn get_type(&self, stat: &Statement) -> Option<BasicTypeEnum<'ctx>> {
        match stat {
            Statement::Comment => panic!("Comment don't have type"),
            Statement::VariableDecl(_, ty, assignments) => {
                let def_ty = if let Some(t) = ty {
                    Some(Self::convert_type(t, self.context))
                } else {
                    None
                };

                let assign_ty = self.get_type(&assignments).unwrap();

                if let Some(t) = def_ty {
                    if t == assign_ty {
                        return Some(t);
                    } else {
                        panic!();
                    }
                }
                Some(assign_ty)
            }
            Statement::Math(l, op, r) => {
                let get_type = self.get_type(l).unwrap();
                if get_type == self.get_type(r).unwrap() {
                    Some(get_type)
                } else {
                    panic!()
                }
            }
            Statement::Num(_) => Some(BasicTypeEnum::IntType(self.context.i64_type())),
            Statement::StringLet(s) => {
                Some(BasicTypeEnum::ArrayType(self.context.i8_type().array_type(s.len() as u32)))
            }
            Statement::If(cond, stats, else_stats) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Identifier(idenf) => {
                Some(self.current_scope.borrow_mut().get_value(idenf).unwrap().0)
            }
            Statement::Assignment(_, _) => todo!(),
            Statement::Block(stats) => match stats.len() {
                0 => panic!(),
                1 => Some(self.get_type(&stats[0]).unwrap()),
                _ => {
                    let mut ty = None;
                    for s in stats {
                        let get_type = &self.get_type(s);
                        if get_type.is_some() {
                            if ty.is_some() {
                                let other_ty = get_type.unwrap();
                                if ty.unwrap() != other_ty {
                                    panic!("all parts of block should return the same type");
                                }
                            } else {
                                ty = Some(self.get_type(s)).unwrap();
                            }
                        }
                    }
                    ty
                }
            },
        }
    }

    pub fn convert_type(ty: &String, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
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
        &self,
        ty: &String,
        args: &Vec<(String, String)>,
    ) -> FunctionType<'ctx> {
        if ty == "()" {
            return self.context.void_type().fn_type(
                &args
                    .into_iter()
                    .map(|a| Self::convert_type(&a.0, self.context).into())
                    .collect::<Vec<_>>(),
                false,
            );
        } else {
            return Self::convert_type(ty, self.context).fn_type(
                &args
                    .into_iter()
                    .map(|a| Self::convert_type(&a.0, self.context).into())
                    .collect::<Vec<_>>(),
                false,
            );
        }
    }

    pub fn gen_code(&self, ast: AST) {
        match ast {
            AST::Function(name, ty_name, args, stats) => {
                let ty = self.convert_type_to_func(&ty_name, &args);
                let func = self.module.add_function(&name, ty, None);
                let basic_block = self.context.append_basic_block(func, "entry");
                self.builder.position_at_end(basic_block);

                self.current_scope.replace(
                    self.current_scope
                        .take()
                        .open_scope(ScopeType::Function(func.clone())),
                );
                for (i, arg) in args.iter().enumerate() {
                    let param = func.get_nth_param(i as u32).unwrap();
                    let ty = Self::convert_type(&arg.0, self.context);
                    let ass_alloca = self.builder.build_alloca(ty, &arg.1).unwrap();
                    self.current_scope.borrow_mut().set_value(
                        arg.0.clone(),
                        ty,
                        BasicValueEnum::PointerValue(ass_alloca),
                    );
                }

                self.get_value(&stats);

                if ty_name == "()"
                    && func
                        .get_last_basic_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                {
                    self.builder.build_return(None).unwrap();
                };
                self.current_scope.replace(
                    self.current_scope.take().close_scope()
                );
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
