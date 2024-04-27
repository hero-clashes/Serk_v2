use core::panic;
use std::{collections::HashMap, mem};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum::{self, PointerValue}, FunctionValue
    },
};
use once_cell::sync::Lazy;
use regex::Regex;
#[derive(Default)]
pub struct Scope<'ctx> {
    parent_scope: Option<Box<Scope<'ctx>>>,
    defs: HashMap<String, (BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
    pub current_function: Option<FunctionValue<'ctx>>,
    pub current_assign: Option<String>,
}

impl<'ctx> Scope<'ctx> {
    fn open(self: Box<Self>) -> Box<Self> {
        let mut new_scope = Scope::default();
        new_scope.parent_scope = Some(self);
        Box::new(new_scope)
    }

    fn close(self) -> Box<Self> {
        self.parent_scope.unwrap()
    }

    fn open_scope(s: &mut Option<Box<Self>>) {
        *s = Some(mem::take(s).unwrap().open());
    }

    fn close_scope(s: &mut Option<Box<Self>>) {
        *s = Some(mem::take(s).unwrap().close())
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

    fn get_function(&self) -> Option<FunctionValue<'ctx>> {
        let s = self.current_function;
        if s.is_some() {
            return s;
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if current_scope.current_function.is_some() {
                    return current_scope.current_function;
                }
            }
        }

        None
    }
    fn get_assign(&self) -> Option<String> {
        let s = self.current_assign.clone();
        if s.is_some() {
            return s;
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if current_scope.current_assign.is_some() {
                    return current_scope.current_assign.clone();
                }
            }
        }

        None
    }
}

#[derive(Debug)]
pub enum Statement {
    VariableDecl(String, Option<String>, Box<Statement>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
    If(Box<Statement>, Box<Statement>, Box<Statement>),
    Return(Box<Statement>),
    Identifier(String),
    Assignment(String, Box<Statement>),
    Block(Vec<Statement>),
    BoolLet(bool),
    PreFix(char, Box<Statement>),
    While(Box<Statement>, Box<Statement>),
    Break,
    Continue,
    Call(String, Vec<Statement>),
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
    pub current_scope: Option<Box<Scope<'ctx>>>,
}

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn get_value(&mut self, stat: &Statement) -> Option<BasicValueEnum<'ctx>> {
        match stat {
            Statement::VariableDecl(n, t, s) => {
                let ty = self.get_type(&stat).unwrap();
                let alloc = self.builder.build_alloca(ty, &n).unwrap();

                let value = self.get_value(&s).unwrap();
                self.builder.build_store(alloc, value).unwrap();

                self.current_scope.as_mut().unwrap().set_value(
                    n.to_string(),
                    ty,
                    PointerValue(alloc),
                );
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
                            .build_int_signed_div(
                                l.into_int_value(),
                                r.into_int_value(),
                                "Dividing",
                            )
                            .unwrap(),
                    )),
                    '*' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_mul(l.into_int_value(), r.into_int_value(), "Multiplying")
                            .unwrap(),
                    )),
                    '%' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_signed_rem(
                                l.into_int_value(),
                                r.into_int_value(),
                                "Reminder",
                            )
                            .unwrap(),
                    )),
                    'e' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::EQ,
                                l.into_int_value(),
                                r.into_int_value(),
                                "Comparing Equal",
                            )
                            .unwrap(),
                    )),
                    'n' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                l.into_int_value(),
                                r.into_int_value(),
                                "Comparing Not Equal",
                            )
                            .unwrap(),
                    )),
                    'a' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_and(
                                l.into_int_value(),
                                r.into_int_value(),
                                "And",
                            )
                            .unwrap(),
                    )),
                    'o' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_or(
                                l.into_int_value(),
                                r.into_int_value(),
                                "Or",
                            )
                            .unwrap(),
                    )),
                    'l' => Some(BasicValueEnum::IntValue(
                        self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            l.into_int_value(),
                            r.into_int_value(),
                            "Less Than",
                        )
                        .unwrap(),
                    )),
                    'p' => Some(BasicValueEnum::IntValue(
                        self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLE,
                            l.into_int_value(),
                            r.into_int_value(),
                            "Less Equal",
                        )
                        .unwrap(),
                    )),
                    'g' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::SGT,
                                l.into_int_value(),
                                r.into_int_value(),
                                "Greater",
                            )
                            .unwrap(),
                    )),
                    'q' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_int_compare(
                                inkwell::IntPredicate::SGE,
                                l.into_int_value(),
                                r.into_int_value(),
                                "Greater_Equal",
                            )
                            .unwrap(),
                    )),
                    _ => todo!(),
                }
            }
            Statement::Num(n) => Some(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*n as u64, true),
            )),
            Statement::StringLet(lateral) => Some(BasicValueEnum::ArrayValue(
                self.context.const_string(lateral.as_bytes(), false),
            )),
            Statement::If(cond, stats, else_stats) => {
                let cond = self.get_value(cond).unwrap();

                let function = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let then_block = self.context.append_basic_block(function, "if_then");
                let else_block = self.context.append_basic_block(function, "if_else");
                let after_block = self.context.append_basic_block(function, "if_after");
                self.builder
                    .build_conditional_branch(cond.into_int_value(), then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                Scope::open_scope(&mut self.current_scope);
                self.get_value(stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder
                    .build_unconditional_branch(after_block)
                    .unwrap();

                self.builder.position_at_end(else_block);
                Scope::open_scope(&mut self.current_scope);
                self.get_value(else_stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder
                    .build_unconditional_branch(after_block)
                    .unwrap();

                self.builder.position_at_end(after_block);

                None
            }
            Statement::Return(s) => {
                let get_value = self.get_value(s).unwrap();

                let get_assign = &self.current_scope.as_ref().unwrap().get_assign();
                if get_assign.is_none() {
                    self.builder.build_return(Some(&get_value)).unwrap();
                } else {
                    self.builder
                        .build_store(
                            self.current_scope
                                .as_ref()
                                .unwrap()
                                .get_value(get_assign.as_ref().unwrap())
                                .unwrap()
                                .1
                                .into_pointer_value(),
                            get_value,
                        )
                        .unwrap();
                }
                None
            }
            Statement::Identifier(name) => {
                let (ty, mut val) = *self
                    .current_scope
                    .as_ref()
                    .unwrap()
                    .get_value(&name)
                    .unwrap();
                if !ty.is_pointer_type() && val.get_type().is_pointer_type() {
                    val = self
                        .builder
                        .build_load(ty, val.into_pointer_value(), "load")
                        .unwrap();
                }
                Some(val)
            }
            Statement::Assignment(name, s) => {
                Scope::open_scope(&mut self.current_scope);
                self.current_scope.as_mut().unwrap().current_assign = Some(name.to_string());
                if let Some(val) = self.get_value(s) {
                    self.builder
                        .build_store(
                            self.current_scope
                                .as_ref()
                                .unwrap()
                                .get_value(&name)
                                .unwrap()
                                .1
                                .into_pointer_value(),
                            val,
                        )
                        .unwrap();
                }else {
                    panic!();
                };
                self.current_scope.as_mut().unwrap().current_assign = None;
                Scope::close_scope(&mut self.current_scope);
                None
            }
            Statement::Block(stats) => {
                if stats.len() == 1 {
                    return self.get_value(&stats[0]);
                }

                for stat in stats {
                    self.get_value(stat);
                }

                None
            }
            Statement::BoolLet(bool) => Some(
                self.context
                    .bool_type()
                    .const_int(*bool as u64, false)
                    .as_basic_value_enum(),
            ),
            Statement::PreFix(p, s) => match p {
                '-' => {
                    let value = self.get_value(s).unwrap().into_int_value();
                    Some(
                        self.builder
                            .build_int_neg(value, "Neg")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
                '!' => {
                    let value = self.get_value(s).unwrap().into_int_value();
                    Some(
                        self.builder
                            .build_not(value, "Not")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
                _ => panic!(),
            },
            Statement::While(cond, stats) => {
                let function = self.current_scope.as_ref().unwrap().get_function().unwrap();

                let cond_block = self.context.append_basic_block(function, "while_cond");
                let then_block = self.context.append_basic_block(function, "while_then");
                let after_block = self.context.append_basic_block(function, "while_after");
                
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(cond_block);
                let cond = self.get_value(cond).unwrap();
                self.builder
                    .build_conditional_branch(cond.into_int_value(), then_block, after_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                Scope::open_scope(&mut self.current_scope);
                self.get_value(stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(after_block);
                None
            },
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Call(n, s) => {
                let func = self.module.get_function(&n).unwrap();
                
                let args = s.iter().map(|stat| BasicMetadataValueEnum::from(self.get_value(stat).unwrap())).collect::<Vec<_>>();
                self.builder.build_call(func, &args, "call").unwrap().try_as_basic_value().left()
            },
        }
    }

    pub fn get_type(&self, stat: &Statement) -> Option<BasicTypeEnum<'ctx>> {
        match stat {
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
            Statement::StringLet(s) => Some(BasicTypeEnum::ArrayType(
                self.context.i8_type().array_type(s.len() as u32),
            )),
            Statement::If(cond, stats, else_stats) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Identifier(idenf) => Some(
                self.current_scope
                    .as_ref()
                    .unwrap()
                    .get_value(idenf)
                    .unwrap()
                    .0,
            ),
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
            Statement::BoolLet(_) => Some(self.context.bool_type().as_basic_type_enum()),
            Statement::PreFix(_, s) => self.get_type(&s),
            Statement::While(_, s) => self.get_type(s),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Call(s, _) => Some(self.module.get_function(&s).unwrap().get_type().get_return_type().unwrap()),
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

        if ty == "bool" {
            return context.bool_type().as_basic_type_enum();
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

    pub fn gen_code(&mut self, ast: AST) {
        match ast {
            AST::Function(name, ty_name, args, stats) => {
                let ty = self.convert_type_to_func(&ty_name, &args);
                let func = self.module.add_function(&name, ty, None);
                let basic_block = self.context.append_basic_block(func, "entry");
                self.builder.position_at_end(basic_block);

                Scope::open_scope(&mut self.current_scope);
                self.current_scope.as_mut().unwrap().current_function = Some(func);
                for (i, arg) in args.iter().enumerate() {
                    let param = func.get_nth_param(i as u32).unwrap();
                    let ty = Self::convert_type(&arg.0, self.context);
                    let ass_alloca = self.builder.build_alloca(ty, &arg.1).unwrap();
                    self.current_scope.as_mut().unwrap().set_value(
                        arg.0.clone(),
                        ty,
                        BasicValueEnum::PointerValue(ass_alloca),
                    );
                    self.builder.build_store(ass_alloca, param).unwrap();
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
                Scope::close_scope(&mut self.current_scope);
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
