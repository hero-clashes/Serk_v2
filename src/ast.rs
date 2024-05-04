use std::{borrow::Cow, collections::HashMap, ffi::{CStr, CString}, mem, ops::Range, process};

use codespan_reporting::{diagnostic::{Diagnostic, Label}, files::SimpleFiles, term::{self, termcolor::{ColorChoice, StandardStream}}};
use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::{AsContextRef, Context}, intrinsics::Intrinsic, llvm_sys::{core::{LLVMBuildCall2, LLVMConstNull, LLVMGetEnumAttributeKindForName, LLVMTokenTypeInContext}, prelude::LLVMValueRef}, module::Module, passes::PassBuilderOptions, targets::{InitializationConfig, Target, TargetMachine}, types::{AsTypeRef, BasicType, BasicTypeEnum, FunctionType}, values::{
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
pub struct Statement{
    loc: (usize,usize),
    data: StatementData,
}
impl Statement{
    pub fn new(loc:(usize,usize), data:StatementData)->Self{
        Statement { loc, data }
    }
    pub fn to_rng(&self) -> Range<usize>{
        self.loc.0..self.loc.1
    }
}
#[derive(Debug)]
pub enum StatementData {
    VariableDecl(String, Option<String>, Box<Statement>),
    Math(Box<Statement>, char, Box<Statement>),
    Num(i64),
    StringLet(String),
    If(Box<Statement>, Box<Statement>, Box<Statement>),
    Return(Option<Box<Statement>>),
    Identifier(String),
    Assignment(String, Box<Statement>),
    Block(Vec<Statement>),
    BoolLet(bool),
    PreFix(char, Box<Statement>),
    While(Box<Statement>, Box<Statement>),
    Break,
    Continue,
    Call(String, Vec<Statement>),
    Yield(Option<Box<Statement>>),
    For(String, Box<Statement>, Box<Statement>)
}

#[derive(Debug)]
pub struct AST{
    data: ASTData,
    loc: (usize,usize)
}
impl AST{
    pub fn new(loc: (usize,usize),data: ASTData) -> Self {
        Self { data, loc }
    }
    pub fn to_rng(&self) -> Range<usize>{
        self.loc.0..self.loc.1
    }
}
#[derive(Debug)]
pub enum ASTData {
    Module(Vec<Box<AST>>),
    Function(String, String, Vec<(String, String)>, Statement),
    GenFunction(Box<ASTData>),
}

static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

pub struct Backend<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a mut Builder<'ctx>,
    pub module: &'a mut Module<'ctx>,
    pub current_scope: Option<Box<Scope<'ctx>>>,
    pub current_file: usize,
    pub files: SimpleFiles<String,String>,
}

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn get_value(&mut self, stat: &Statement) -> Option<BasicValueEnum<'ctx>> {
        match &stat.data {
            StatementData::VariableDecl(n, t, s) => {
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
            StatementData::Math(l, op, r) => {
                let l = self.get_value(&l).unwrap();
                let r = self.get_value(&r).unwrap();
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
            StatementData::Num(n) => Some(BasicValueEnum::IntValue(
                self.context.i64_type().const_int(*n as u64, true),
            )),
            StatementData::StringLet(lateral) => Some(BasicValueEnum::ArrayValue(
                self.context.const_string(lateral.as_bytes(), false),
            )),
            StatementData::If(cond, stats, else_stats) => {
                let cond = self.get_value(&cond).unwrap();

                let function = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let then_block = self.context.append_basic_block(function, "if_then");
                let else_block = self.context.append_basic_block(function, "if_else");
                let after_block = self.context.append_basic_block(function, "if_after");
                self.builder
                    .build_conditional_branch(cond.into_int_value(), then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                Scope::open_scope(&mut self.current_scope, ScopeTy::Normal);
                self.get_value(&stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder
                    .build_unconditional_branch(after_block)
                    .unwrap();

                self.builder.position_at_end(else_block);
                Scope::open_scope(&mut self.current_scope, ScopeTy::Normal);
                self.get_value(&else_stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder
                    .build_unconditional_branch(after_block)
                    .unwrap();

                self.builder.position_at_end(after_block);

                None
            }
            StatementData::Return(s) => {
                if let Some(s) = s{
                    let get_value = self.get_value(&s).unwrap();

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
                }else{
                    self.builder.build_return(None);
                }
                None
            }
            StatementData::Identifier(name) => {
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
            StatementData::Assignment(name, s) => {
                Scope::open_scope(&mut self.current_scope, ScopeTy::Assign(name.to_string()));
                let val  = self.get_value(&s).unwrap();
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
                self.current_scope.as_mut().unwrap().ty = ScopeTy::default();
                Scope::close_scope(&mut self.current_scope);
                None
            }
            StatementData::Block(stats) => {
                if stats.len() == 1 {
                    return self.get_value(&stats[0]);
                }

                for stat in stats {
                    self.get_value(&stat);
                }

                None
            }
            StatementData::BoolLet(bool) => Some(
                self.context
                    .bool_type()
                    .const_int(*bool as u64, false)
                    .as_basic_value_enum(),
            ),
            StatementData::PreFix(p, s) => match p {
                '-' => {
                    let value = self.get_value(&s).unwrap().into_int_value();
                    Some(
                        self.builder
                            .build_int_neg(value, "Neg")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
                '!' => {
                    let value = self.get_value(&s).unwrap().into_int_value();
                    Some(
                        self.builder
                            .build_not(value, "Not")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
                _ => panic!(),
            },
            StatementData::While(cond, stats) => {
                let function = self.current_scope.as_ref().unwrap().get_function().unwrap();

                let cond_block = self.context.append_basic_block(function, "while_cond");
                let then_block = self.context.append_basic_block(function, "while_then");
                let after_block = self.context.append_basic_block(function, "while_after");

                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(cond_block);
                let cond = self.get_value(&cond).unwrap();
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
                self.get_value(&stats);
                Scope::close_scope(&mut self.current_scope);
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(after_block);
                None
            }
            StatementData::Break => {
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
            StatementData::Continue => {
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
            StatementData::Call(n, s) => {
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
            StatementData::Yield(s) => {
                let current_func = self.current_scope.as_ref().unwrap().get_function().unwrap();

                if let Some(s) = s{
                    let value = self.get_value(&s).unwrap();
                    let GenFunction { co_suspend, suspend_block, cleanup_block, promise } = self.current_scope.as_ref().unwrap().get_gen_function().unwrap();
                    self.builder.build_store(promise, value).unwrap();
                }
                let GenFunction { co_suspend, suspend_block, cleanup_block, promise } = self.current_scope.as_ref().unwrap().get_gen_function().unwrap();

                let next_block = self.context.append_basic_block(current_func, "next_block");
                let token_none = unsafe { LLVMConstNull(LLVMTokenTypeInContext(self.context.as_ctx_ref())) };
                
                let ch = self.manual_call(co_suspend, vec![token_none,self.context.bool_type().const_int(0, false).as_value_ref()], "ch".to_string());
                let i8t = self.context.i8_type();
                self.builder.build_switch(ch.as_any_value_enum().into_int_value(), suspend_block, &[(i8t.const_int(0, false),next_block),(i8t.const_int(1, false),cleanup_block)]).unwrap();
                self.builder.position_at_end(next_block);
                None
            }
            StatementData::For(var, func_name, stats) => {
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
                let ty = self.convert_type(&"i64".to_owned());
                self.current_scope.as_mut().unwrap().set_value(var.to_string(), ty, value); //TODO implement type
                let loop_block = self.context.append_basic_block(func, "for_block");
                let after_block= self.context.append_basic_block(func, "after_for");
                self.builder.build_unconditional_branch(loop_block).unwrap();
                self.builder.position_at_end(loop_block);
                self.get_value(&stats);
                
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
        match &stat.data {
            StatementData::VariableDecl(_, ty, assignments) => {
                let def_ty = if let Some(t) = ty {
                    Some(self.convert_type(&t))
                } else {
                    None
                };

                let assign_ty = self.get_type(&assignments).unwrap();

                if let Some(t) = def_ty {
                    if t == assign_ty {
                        return Some(t);
                    } else {
                        let d = Diagnostic::<usize>::error()
                        .with_message("assigned type isn't compatible with the variable type")
                        .with_labels(vec![Label::primary(self.current_file, stat.to_rng()),Label::secondary(self.current_file, assignments.to_rng())])
                        .with_notes(vec![format!("expected {} found {}",t.print_to_string().to_string(), assign_ty.print_to_string().to_string())]);
                        self.print_error(d,true);
                    }
                }
                Some(assign_ty)
            }
            StatementData::Math(l, op, r) => {
                let l_ty = self.get_type(&l).unwrap();
                let r_ty = self.get_type(&r).unwrap();
                if l_ty == r_ty {
                    Some(l_ty)
                } else {
                    let d = Diagnostic::<usize>::error()
                    .with_message("both operands should of the same type")
                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng()),
                    Label::secondary(self.current_file, l.to_rng()).with_message(format!("ty: {}",l_ty.print_to_string())),
                    Label::secondary(self.current_file, r.to_rng()).with_message(format!("ty: {}",r_ty.print_to_string()))]);
                    self.print_error(d,true);
                    None
                }
            }
            StatementData::Num(_) => Some(BasicTypeEnum::IntType(self.context.i64_type())),
            StatementData::StringLet(s) => Some(BasicTypeEnum::ArrayType(
                self.context.i8_type().array_type(s.len() as u32),
            )),
            StatementData::If(cond, stats, else_stats) => {
                if self.get_type(&cond).unwrap() != self.context.bool_type().as_basic_type_enum() {
                    let d = Diagnostic::<usize>::error()
                    .with_message("if condition has to be of type i1(bool)")
                    .with_labels(vec![Label::primary(self.current_file, cond.to_rng())]);
                    self.print_error(d,true);
                }
                let stats_ty = self.get_type(&stats);
                let else_stats_ty = self.get_type(&else_stats);

                if stats_ty == else_stats_ty {
                    return stats_ty;
                }else{
                    let d = Diagnostic::<usize>::error()
                    .with_message("both operands of if should return the same time")
                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng()),
                    Label::secondary(self.current_file, stat.to_rng()).with_message(format!("ty: {}",stats_ty.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string()))),
                    Label::secondary(self.current_file, else_stats.to_rng()).with_message(format!("ty: {}",else_stats_ty.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string())))]);
                    self.print_error(d,true);
                    None
                }
            },
            StatementData::Return(s) => {
                let f = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let ty = if s.is_some(){self.get_type(&s.as_ref().unwrap())} else{None};
                
                let get_return_type = f.get_type().get_return_type();

                if let Some(s) = s{
                    
                    if get_return_type != ty{
                        let d = Diagnostic::<usize>::error()
                        .with_message("Return Ty don't match function signature")
                        .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                        .with_notes(vec![format!("function returns type is {}, the return statement type is {}",get_return_type.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string()),ty.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string()))]);
                        self.print_error(d,true);
                    }
                }


                None
            },
            StatementData::Identifier(idenf) => Some(
                self.current_scope
                    .as_ref()
                    .unwrap()
                    .get_value(&idenf)
                    .unwrap()
                    .0,
            ),
            StatementData::Assignment(name, s) => {
                match self.get_type(&s){
                    Some(s) => {
                        let basic_type_enum = self.current_scope.as_ref().unwrap().get_value(&name).unwrap().0;
                        if basic_type_enum != s{
                            let d = Diagnostic::<usize>::error()
                            .with_message("Assign Ty doesn't match the variable ty")
                            .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                            .with_notes(vec![format!("Variable type is {}, the return statement type is {}",basic_type_enum.print_to_string(),s.print_to_string())]);
                            self.print_error(d,true);
                            return None;
                        }
                    },
                    None => {
                        let d = Diagnostic::<usize>::error()
                            .with_message("Can't Assign to a variable with an void type statement")
                            .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                            self.print_error(d,true);
                            return None;

                        return None;
                    },
                }
                None
            },
            StatementData::Block(stats) => match stats.len() {
                0 => None,
                1 => Some(self.get_type(&stats[0]).unwrap()),
                _ => {
                    let mut loc = Range { start: 0, end: 0 };
                    let mut ty = None;
                    for s in stats {
                        let get_type = &self.get_type(&s);
                        if get_type.is_some() {
                            if ty.is_some() {
                                let other_ty = get_type.unwrap();
                                if ty.unwrap() != other_ty {
                                    let d = Diagnostic::<usize>::error()
                                    .with_message("All returns in block should be the same type")
                                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng()),
                                    Label::secondary(self.current_file, loc.clone()).with_message(format!("Original Type Assumed here: {}",get_type.unwrap().print_to_string())),
                                    Label::secondary(self.current_file, s.to_rng()).with_message(format!("this returns type: {}",get_type.unwrap().print_to_string()))]);
                                    self.print_error(d,true);
                                }
                            } else {
                                ty = Some(self.get_type(&s)).unwrap();
                                loc = s.to_rng();
                            }
                        }
                    }
                    ty
                }
            },
            StatementData::BoolLet(_) => Some(self.context.bool_type().as_basic_type_enum()),
            StatementData::PreFix(_, s) => self.get_type(&s),
            StatementData::While(cond, s) => {
                if self.get_type(&cond).unwrap() != self.context.bool_type().as_basic_type_enum() {
                    let d = Diagnostic::<usize>::error()
                    .with_message("while condition has to be of type i1(bool)")
                    .with_labels(vec![Label::primary(self.current_file, cond.to_rng())]);
                    self.print_error(d,true);
                }
                return self.get_type(&s)
            },
            StatementData::Break => None,
            StatementData::Continue => None,
            StatementData::Call(s, _) => Some(
                self.module
                    .get_function(&s)
                    .unwrap()
                    .get_type()
                    .get_return_type()
                    .unwrap(),
            ),
            StatementData::Yield(s) => {
                // if let Some(s) = s{
                //     let f = self.current_scope.as_ref().unwrap().get_function().unwrap();
                //     let ty = self.get_type(&s).unwrap();
                    
                //     let get_return_type = f.get_type().get_return_type();
                //     if get_return_type != Some(ty){
                //         let d = Diagnostic::<usize>::error()
                //         .with_message("Return Ty don't match function signature")
                //         .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                //         .with_notes(vec![format!("function returns type is {}, the return statement type is {}",get_return_type.unwrap().print_to_string(),ty.print_to_string())]);
                //         self.print_error(d,true);
                //     }
                // }//TODO fix this since coroutines stores return data differently


                None
            },
            StatementData::For(_, _, s) => self.get_type(&s),
        }
    }

    pub fn convert_type(&self, ty: &String) -> BasicTypeEnum<'ctx> {
        if let Some(c) = INT_TYPE.captures(&ty) {
            return BasicTypeEnum::IntType(self.context.custom_width_int_type(
                u32::from_str_radix(c.get(1).unwrap().as_str(), 10).unwrap(),
            ));
        }

        if let Some(c) = FLOAT_TYPE.captures(&ty) {
            match c.get(1).unwrap().as_str() {
                "f16" => return BasicTypeEnum::FloatType(self.context.f16_type()),
                "f32" => return BasicTypeEnum::FloatType(self.context.f32_type()),
                "f64" => return BasicTypeEnum::FloatType(self.context.f64_type()),
                "f128" => return BasicTypeEnum::FloatType(self.context.f128_type()),
                _ => {},
            }
        }

        if ty == "bool" {
            return self.context.bool_type().as_basic_type_enum();
        }

        let d = Diagnostic::<usize>::error()
        .with_message(format!("Can't find type {ty}"));
        self.print_error(d,true);
        panic!();
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
                    .map(|a| self.convert_type(&a.0).into())
                    .collect::<Vec<_>>(),
                false,
            );
        } else {
            return self.convert_type(ty).fn_type(
                &args
                    .into_iter()
                    .map(|a| self.convert_type(&a.0).into())
                    .collect::<Vec<_>>(),
                false,
            );
        }
    }

    pub fn gen_code(&mut self, ast: AST) {
        let loc = ast.to_rng();
        match ast.data {
            ASTData::Function(name, ty_name, args, stats) => {
                let ty = self.convert_type_to_func(&ty_name, &args);
                let func = self.module.add_function(&name, ty, None);
                let basic_block = self.context.append_basic_block(func, "entry");
                self.builder.position_at_end(basic_block);

                Scope::open_scope(&mut self.current_scope, ScopeTy::Function());
                self.current_scope.as_mut().unwrap().current_func = Some(func);
                for (i, arg) in args.iter().enumerate() {
                    let param = func.get_nth_param(i as u32).unwrap();
                    let ty = self.convert_type(&arg.0);
                    let ass_alloca = self.builder.build_alloca(ty, &arg.1).unwrap();
                    self.current_scope.as_mut().unwrap().set_value(
                        arg.0.clone(),
                        ty,
                        BasicValueEnum::PointerValue(ass_alloca),
                    );
                    self.builder.build_store(ass_alloca, param).unwrap();
                    self.current_scope.as_mut().unwrap().set_value(arg.1.to_owned(), ty, ass_alloca.as_basic_value_enum());
                }
                self.get_type(&stats);
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
                        let d = Diagnostic::<usize>::error()
                        .with_message(format!("Function \"{}\" Doesn't have terminator",name))
                        .with_labels(vec![Label::primary(self.current_file, loc)]);
                        self.print_error(d,true);
                    }
                };
                Scope::close_scope(&mut self.current_scope);
                func.verify(true);
            }
            ASTData::Module(l) => {
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
            ASTData::GenFunction(f) => {
                if let ASTData::Function(name, ty_name, args, stats) = *f {
                    let ty = self.context.i8_type().ptr_type(AddressSpace::default()).fn_type(
                        &args.clone()
                            .into_iter()
                            .map(|a| self.convert_type(&a.0).into())
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
                            self.convert_type(&ty_name).as_basic_type_enum(),
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
                        let ty = self.convert_type(&arg.0);
                        let ass_alloca = self.builder.build_alloca(ty, &arg.1).unwrap();
                        self.current_scope.as_mut().unwrap().set_value(
                            arg.0.clone(),
                            ty,
                            BasicValueEnum::PointerValue(ass_alloca),
                        );
                        self.builder.build_store(ass_alloca, param).unwrap();
                        self.current_scope.as_mut().unwrap().set_value(arg.1.to_owned(), ty, ass_alloca.as_basic_value_enum());
                    }
                    self.get_type(&stats);
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

    fn print_error(&self, d:Diagnostic<usize>, exit: bool){  
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer.lock(), &config, &self.files, &d).unwrap();
        if exit{
            process::exit(1)
        }
    }
}
