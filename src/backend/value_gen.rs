
use std::{borrow::Cow, ffi::{CStr, CString}};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use inkwell::{
    context::AsContextRef, intrinsics::Intrinsic, llvm_sys::{core::{LLVMBuildCall2, LLVMConstNull, LLVMTokenTypeInContext}, prelude::LLVMValueRef}, types::AsTypeRef, values::{
        AnyValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum::{self, PointerValue}, CallSiteValue, FunctionValue
    }
};
use crate::{scope::*, statement::*};
use super::Backend;



impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn get_value(&mut self, stat: &Statement) -> Option<BasicValueEnum<'ctx>> {
        match &stat.data {
            StatementData::VariableDecl(n, _, s) => {
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
                    self.builder.build_return(None).unwrap();
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
                _ => {
                    let d = Diagnostic::<usize>::error()
                    .with_message(format!("Unknown Prefix Operator: \"{p}\""))
                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                    self.print_error(d);
                },
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
                    cond_block:_,
                    after_block,
                } = self.current_scope.as_ref().unwrap().get_while().unwrap()
                {
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();
                } else {
                    let d = Diagnostic::<usize>::error()
                    .with_message("can't use Break outside of While loop")
                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                    self.print_error(d);
                };

                None
            }
            StatementData::Continue => {
                if let ScopeTy::Loop {
                    cond_block,
                    after_block:_,
                } = self.current_scope.as_ref().unwrap().get_while().unwrap()
                {
                    self.builder.build_unconditional_branch(cond_block).unwrap();
                } else {
                    let d = Diagnostic::<usize>::error()
                    .with_message("can't use Continue outside of While loop")
                    .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                    self.print_error(d);
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
                    let GenFunction { co_suspend:_, suspend_block:_, cleanup_block:_, promise } = self.current_scope.as_ref().unwrap().get_gen_function().unwrap();
                    self.builder.build_store(promise, value).unwrap();
                }
                let GenFunction { co_suspend, suspend_block, cleanup_block, promise:_ } = self.current_scope.as_ref().unwrap().get_gen_function().unwrap();

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


    pub fn manual_call(&self,s:FunctionValue,mut arg: Vec<LLVMValueRef>, name:String) -> CallSiteValue<'ctx>  {
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