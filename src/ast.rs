use std::{borrow::Cow, collections::HashMap, ffi::{CStr, CString}, mem};

use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::{AsContextRef, Context}, intrinsics::Intrinsic, llvm_sys::{core::{LLVMBuildCall2, LLVMConstNull, LLVMGetEnumAttributeKindForName, LLVMRunFunctionPassManager, LLVMTokenTypeInContext}, prelude::LLVMValueRef}, module::Module, passes::{PassBuilderOptions, PassManager, PassManagerSubType}, targets::{InitializationConfig, Target, TargetMachine, TargetTriple}, types::{AsTypeRef, BasicType, BasicTypeEnum, FunctionType}, values::{
        AnyValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum::{self, PointerValue}, CallSiteValue, FunctionValue
    }, AddressSpace
};
use inkwell::values::PointerValue as Op;
use once_cell::sync::Lazy;
use regex::Regex;
#[derive(Clone)]
pub struct GenFunction<'ctx>{pub co_suspend:FunctionValue<'ctx>,pub suspend_block: BasicBlock<'ctx>,pub cleanup_block: BasicBlock<'ctx>, pub promise: Op<'ctx>}

#[derive(Default, Clone)]
pub enum ScopeTy<'ctx> {
    #[default]
    Normal,
    Function(),
    GenFunction(GenFunction<'ctx>),
    Assign(String),
    Loop {
        cond_block: BasicBlock<'ctx>,
        after_block: BasicBlock<'ctx>,
    },
}
#[derive(Default)]
pub struct Scope<'ctx> {
    parent_scope: Option<Box<Scope<'ctx>>>,
    defs: HashMap<String, (BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
    ty: ScopeTy<'ctx>,
    pub current_func: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    fn open(self: Box<Self>, ty: ScopeTy<'ctx>) -> Box<Self> {
        let mut new_scope = Scope::default();
        new_scope.parent_scope = Some(self);
        new_scope.ty = ty;
        Box::new(new_scope)
    }

    fn close(self) -> Box<Self> {
        self.parent_scope.unwrap()
    }

    fn open_scope(s: &mut Option<Box<Self>>, ty: ScopeTy<'ctx>) {
        *s = Some(mem::take(s).unwrap().open(ty));
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
        let s = &self.current_func;
        if s.is_some() {
            return *s
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if current_scope.current_func.is_some(){
                    return current_scope.current_func;
                }
            }
        }

        None
    }
    fn get_gen_function(&self) -> Option<GenFunction> {
        let s: &ScopeTy<'ctx> = &self.ty;
        if let ScopeTy::GenFunction(f) = s {
            return Some(f.clone());
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if let ScopeTy::GenFunction(f) = &current_scope.ty {
                    return Some(f.clone());
                }
            }
        }

        None
    }
    fn get_assign(&self) -> Option<String> {
        let s = &self.ty;
        if let ScopeTy::Assign(f) = s {
            return Some(f.to_string());
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if let ScopeTy::Assign(f) = &current_scope.ty {
                    return Some(f.to_string());
                }
            }
        }

        None
    }

    fn get_while(&self) -> Option<ScopeTy> {
        let s = &self.ty;
        if let ScopeTy::Loop {
            cond_block,
            after_block,
        } = s
        {
            return Some(s.clone());
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if let ScopeTy::Assign(f) = &current_scope.ty {
                    return Some(current_scope.ty.clone());
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
    Yield(Box<Statement>),
    For(String, Box<Statement>, Box<Statement>)
}

#[derive(Debug)]
pub enum AST {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Statement),
    GenFunction(Box<AST>),
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
                            .build_and(l.into_int_value(), r.into_int_value(), "And")
                            .unwrap(),
                    )),
                    'o' => Some(BasicValueEnum::IntValue(
                        self.builder
                            .build_or(l.into_int_value(), r.into_int_value(), "Or")
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
                Scope::open_scope(&mut self.current_scope, ScopeTy::Normal);
                self.get_value(stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder
                    .build_unconditional_branch(after_block)
                    .unwrap();

                self.builder.position_at_end(else_block);
                Scope::open_scope(&mut self.current_scope, ScopeTy::Normal);
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
                Scope::open_scope(&mut self.current_scope, ScopeTy::Assign(name.to_string()));
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
                } else {
                    match self.get_type(s){
                        Some(s) => {
                            if self.current_scope.as_ref().unwrap().get_value(name).unwrap().0 != s{
                                panic!()
                            }
                        },
                        None => panic!(),
                    }
                };
                self.current_scope.as_mut().unwrap().ty = ScopeTy::default();
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
                Scope::open_scope(
                    &mut self.current_scope,
                    ScopeTy::Loop {
                        cond_block,
                        after_block,
                    },
                );
                self.get_value(stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(after_block);
                None
            }
            Statement::Break => {
                if let ScopeTy::Loop {
                    cond_block,
                    after_block,
                } = self.current_scope.as_ref().unwrap().get_while().unwrap()
                {
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();
                } else {
                    panic!()
                };

                None
            }
            Statement::Continue => {
                if let ScopeTy::Loop {
                    cond_block,
                    after_block,
                } = self.current_scope.as_ref().unwrap().get_while().unwrap()
                {
                    self.builder.build_unconditional_branch(cond_block).unwrap();
                } else {
                    panic!()
                };

                None
            }
            Statement::Call(n, s) => {
                let func = self.module.get_function(&n).unwrap();

                let args = s
                    .iter()
                    .map(|stat| BasicMetadataValueEnum::from(self.get_value(stat).unwrap()))
                    .collect::<Vec<_>>();
                self.builder
                    .build_call(func, &args, "call")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
            }
            Statement::Yield(s) => {
                let current_func = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let value = self.get_value(s).unwrap();
                let GenFunction { co_suspend, suspend_block, cleanup_block, promise } = self.current_scope.as_ref().unwrap().get_gen_function().unwrap();
                self.builder.build_store(promise, value).unwrap();
                
                let next_block = self.context.append_basic_block(current_func, "next_block");
                let token_none = unsafe { LLVMConstNull(LLVMTokenTypeInContext(self.context.as_ctx_ref())) };
                
                let ch = self.manual_call(co_suspend, vec![token_none,self.context.bool_type().const_int(0, false).as_value_ref()], "ch".to_string());
                let i8t = self.context.i8_type();
                self.builder.build_switch(ch.as_any_value_enum().into_int_value(), suspend_block, &[(i8t.const_int(0, false),next_block),(i8t.const_int(1, false),cleanup_block)]).unwrap();
                self.builder.position_at_end(next_block);
                None
            }
            Statement::For(var, func_name, stats) => {
                let pr = |module,s|->FunctionValue  {Intrinsic::find(s)
                    .unwrap()
                    .get_declaration(module, &[])
                    .unwrap()};
                
                let func = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let hdl = self.get_value(&func_name).unwrap();
                let value = self.builder.build_call((|module,s|->FunctionValue  {Intrinsic::find(s)
                    .unwrap()
                    .get_declaration(module, &[])
                    .unwrap()})(self.module, "llvm.coro.promise"), &[hdl.as_basic_value_enum().into(), self.context.i32_type().const_int(8, false).into(), self.context.bool_type().const_int(0, false).into()], "promise_addr").unwrap().try_as_basic_value().left().unwrap();
                self.current_scope.as_mut().unwrap().set_value(var.to_string(), Self::convert_type(&"i64".to_owned(), &self.context), value); //TODO implement type
                let loop_block = self.context.append_basic_block(func, "for_block");
                let after_block= self.context.append_basic_block(func, "after_for");
                self.builder.build_unconditional_branch(loop_block).unwrap();
                self.builder.position_at_end(loop_block);
                self.get_value(stats);
                
                self.manual_call(pr(self.module,"llvm.coro.resume"), vec![hdl.as_value_ref()], "".to_owned());
                let done = self.manual_call(pr(self.module,"llvm.coro.done"), vec![hdl.as_value_ref()], "done".to_owned());

                self.builder.build_conditional_branch(done.as_any_value_enum().into_int_value(), after_block, loop_block).unwrap();
                self.builder.position_at_end(after_block);
                self.manual_call(pr(self.module,"llvm.coro.destroy"), vec![hdl.as_value_ref()], "".to_owned());


                None
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
            Statement::If(cond, stats, else_stats) => {
                let stats_ty = self.get_type(&stats);
                let else_stats_ty = self.get_type(&else_stats);

                if stats_ty == else_stats_ty {
                    return stats_ty;
                }else{
                    panic!()
                }
            },
            Statement::Return(s) => self.get_type(s),
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
                0 => None,
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
            Statement::Break => None,
            Statement::Continue => None,
            Statement::Call(s, _) => Some(
                self.module
                    .get_function(&s)
                    .unwrap()
                    .get_type()
                    .get_return_type()
                    .unwrap(),
            ),
            Statement::Yield(_) => None,
            Statement::For(_, _, s) => self.get_type(s),
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

                Scope::open_scope(&mut self.current_scope, ScopeTy::Function());
                self.current_scope.as_mut().unwrap().current_func = Some(func);
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
                    self.current_scope.as_mut().unwrap().set_value(arg.1.to_owned(), ty, ass_alloca.as_basic_value_enum());
                }

                self.get_value(&stats);

                if func
                        .get_last_basic_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                {
                    if ty_name == "()"{
                        self.builder.build_return(None).unwrap();
                    }else{
                        panic!("function doesn't have terminator");
                    }
                };
                Scope::close_scope(&mut self.current_scope);
                func.verify(true);
            }
            AST::Module(l) => {
                let malloc = self.module.add_function("malloc", self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(&[self.context.i64_type().into()], false), None);
                Target::initialize_x86(&InitializationConfig::default());
                let get_default_triple = TargetMachine::get_default_triple();
                let target_machine: TargetMachine = Target::from_triple(&get_default_triple).unwrap().create_target_machine(
                        &get_default_triple,
                        "",
                        "",
                        inkwell::OptimizationLevel::None,
                        inkwell::targets::RelocMode::Default,
                        inkwell::targets::CodeModel::JITDefault
                    )
                    .unwrap();                
                let options = PassBuilderOptions::create();
                self.module.set_triple(&get_default_triple);
                self.module.set_data_layout(&target_machine.get_target_data().get_data_layout());
                for ast in l {
                    self.gen_code(*ast);

                }
                self.module.print_to_file("a.ir");
            
                self.module.run_passes("coro-early,coro-split,coro-elide,coro-cleanup,reassociate,instcombine,simplifycfg,mem2reg,gvn,lint", &target_machine, options).unwrap();
                self.module.print_to_file("b.ir");
   
            }
            AST::GenFunction(f) => {
                if let AST::Function(name, ty_name, args, stats) = *f {
                    let ty = self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(
                        &args.clone()
                            .into_iter()
                            .map(|a| Self::convert_type(&a.0, self.context).into())
                            .collect::<Vec<_>>(), false);
                    let func = self.module.add_function(&name, ty, None);
                    let basic_block = self.context.append_basic_block(func, "entry");
                    self.builder.position_at_end(basic_block);
                    let get_intrinsic = |s| Intrinsic::find(s)
                    .unwrap()
                    .get_declaration(self.module, &[])
                    .unwrap();



                    let co_id   = get_intrinsic("llvm.coro.id");
                    let co_alloc  = get_intrinsic("llvm.coro.alloc");
                    let co_size  = Intrinsic::find("llvm.coro.size.i64")
                    .unwrap()
                    .get_declaration(self.module, &[self.context.i64_type().as_basic_type_enum()])
                    .unwrap();
                    let co_begin = get_intrinsic("llvm.coro.begin");
                    let co_suspend = get_intrinsic("llvm.coro.suspend");
                    let co_free = get_intrinsic("llvm.coro.free");
                    let co_end = get_intrinsic("llvm.coro.end");

                    let promise = self
                        .builder
                        .build_alloca(
                            Self::convert_type(&ty_name,&self.context).as_basic_type_enum(),
                            "promise",
                        )
                        .unwrap();
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let id = self.manual_call(
                            co_id,
                            vec![
                                self.context.i32_type().const_int(0, false).as_value_ref(),
                                promise.as_value_ref(),
                                ptr_type.const_null().as_value_ref(),
                                ptr_type.const_null().as_value_ref(),
                            ],
                            "id".to_string(),
                        );
                    
                    let need_alloc = self.manual_call(co_alloc, vec![id.as_value_ref()], "need_alloc".to_owned());
                    
                    let dyn_alloc_block = self.context.append_basic_block(func, "dyn_alloc");
                    let coro_begin_block = self.context.append_basic_block(func, "coro_begin");
                    
                    self.builder.build_conditional_branch(need_alloc.try_as_basic_value().left().unwrap().into_int_value(), dyn_alloc_block, coro_begin_block).unwrap();

                    self.builder.position_at_end(dyn_alloc_block);
                    let size = self.builder.build_call(co_size, &[], "size").unwrap();
                    let alloca = self.builder.build_call(self.module.get_function("malloc").unwrap(), &[size.try_as_basic_value().unwrap_left().into()], "alloca");  //TODO make sure malloc is found   
                    self.builder.build_unconditional_branch(coro_begin_block).unwrap();

                    self.builder.position_at_end(coro_begin_block);
                    let phi = self.builder.build_phi(ptr_type.as_basic_type_enum(), "phi").unwrap();
                    phi.add_incoming(&[(&ptr_type.const_null().as_basic_value_enum(), basic_block),(&alloca.unwrap().try_as_basic_value().left().unwrap(),dyn_alloc_block)]);
                    
                    let hdl = self.manual_call(co_begin, vec![id.as_value_ref(),phi.as_value_ref()], "hdl".to_string());
                    
                    let basic_block = self.context.append_basic_block(func, "begin");
                    self.builder.build_unconditional_branch(basic_block).unwrap();
                    self.builder.position_at_end(basic_block);

                    let cleanup_block = self.context.append_basic_block(func, "cleanup");
                    let suspend_block = self.context.append_basic_block(func, "suspend");

                    Scope::open_scope(&mut self.current_scope, ScopeTy::GenFunction(GenFunction {co_suspend, suspend_block, cleanup_block, promise }));
                    self.current_scope.as_mut().unwrap().current_func = Some(func);
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
                        self.current_scope.as_mut().unwrap().set_value(arg.1.to_owned(), ty, ass_alloca.as_basic_value_enum());
                    }
                    self.get_value(&stats);
                    Scope::close_scope(&mut self.current_scope);




                    let token_none = unsafe { LLVMConstNull(LLVMTokenTypeInContext(self.context.as_ctx_ref())) };
                    let ch = self.manual_call(co_suspend, vec![token_none,self.context.bool_type().const_int(1, true).as_value_ref()], "ch".to_string());
                    let i8t = self.context.i8_type();
                    self.builder.build_switch(ch.as_any_value_enum().into_int_value(), suspend_block, &[(i8t.const_int(1, false),cleanup_block)]).unwrap();                    

                    self.builder.position_at_end(cleanup_block);
                    let mem = self.manual_call(co_free, vec![id.as_value_ref(),hdl.as_value_ref()], "mem".to_string());
                    self.builder.build_free(mem.as_any_value_enum().into_pointer_value()).unwrap();
                    self.builder.build_unconditional_branch(suspend_block).unwrap();

                    self.builder.position_at_end(suspend_block);
                    self.builder.build_call(co_end, &[hdl.try_as_basic_value().unwrap_left().into(), self.context.bool_type().const_int(0, false).into()], "unused").unwrap();
                    self.builder.build_return(Some(&hdl.try_as_basic_value().unwrap_left())).unwrap();
                    func.verify(true);
                    let s = "presplitcoroutine".to_string();
                    func.add_attribute(inkwell::attributes::AttributeLoc::Function, self.context.create_enum_attribute(unsafe { LLVMGetEnumAttributeKindForName(s.as_ptr() as *const i8, s.len()) }, 0))
                } else {
                    panic!()
                };
            }
        }
    }
    fn manual_call(&self,s:FunctionValue,mut arg: Vec<LLVMValueRef>, name:String) -> CallSiteValue<'ctx>  {
        let fn_ty_ref = s.get_type().as_type_ref();
        let to_c_str = |s:String| {   if !s.chars().rev().any(|ch| ch == '\0') {
            return Cow::from(CString::new(s).expect("unreachable since null bytes are checked"));
        }
        unsafe { Cow::from(CStr::from_ptr(s.as_ptr() as *const _)) }};
        let c_string = to_c_str(name);

        let value = unsafe {
            LLVMBuildCall2(
                self.builder.as_mut_ptr(),
                fn_ty_ref,
                s.as_value_ref(),
                arg.as_mut_ptr(),
                arg.len() as u32,
                c_string.as_ptr(),
            )
        };

        unsafe { CallSiteValue::new(value) }
    }
}
