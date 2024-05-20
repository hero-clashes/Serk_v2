use std::fs;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use inkwell::{
    context::AsContextRef,
    intrinsics::Intrinsic,
    llvm_sys::core::{LLVMConstNull, LLVMGetEnumAttributeKindForName, LLVMTokenTypeInContext},
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetMachine},
    types::BasicType,
    values::{
        AnyValue, AsValueRef, BasicValue,
        BasicValueEnum::{self},
    },
    AddressSpace,
};

use crate::{
    ast::{ASTData, AST},
    scope::*,
};

use super::{type_gen::{self, Decl}, Backend};

impl<'a, 'ctx> Backend<'a, 'ctx> {
    pub fn gen_code(&mut self, ast: AST) {
        let loc = ast.to_rng();
        match ast.data {
            ASTData::Module(l) => {
                let _malloc = self.module.add_function(
                    "malloc",
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .fn_type(&[self.context.i64_type().into()], false),
                    None,
                );
                let options = PassBuilderOptions::create();
                self.module.set_triple(&self.target_machine.get_triple());
                self.module
                    .set_data_layout(&self.target_machine.get_target_data().get_data_layout());
                for ast in l {
                    self.gen_code(*ast);
                }
                let before = self.module.print_to_string().to_string();
                self.module.run_passes("coro-early,coro-split,coro-elide,coro-cleanup,reassociate,instcombine,simplifycfg,mem2reg,gvn,lint", &self.target_machine, options).unwrap();
                let after = self.module.print_to_string().to_string();
                let _ = fs::write("output.ir", format!("\\\\-----Before Optimization----\n\n\n{before}\n\n\n\n\\\\-----After Optimization----\n\n\n{after}"));
                let _ = self.module.verify().unwrap();
            }
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
                    self.builder.build_store(ass_alloca, param).unwrap();
                    self.current_scope.as_mut().unwrap().set_value(
                        arg.0.clone(),
                        Decl::VariableDecl { ty, val: Some(BasicValueEnum::PointerValue(ass_alloca))}
                    );
                }
                self.get_type(&stats);
                self.get_value(&stats);

                if func
                    .get_last_basic_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    if ty_name == "()" {
                        self.builder.build_return(None).unwrap();
                    } else {
                        let d = Diagnostic::<usize>::error()
                            .with_message(format!("Function \"{}\" Doesn't have terminator", name))
                            .with_labels(vec![Label::primary(self.current_file, loc)]);
                        self.print_error(d);
                    }
                };
                Scope::close_scope(&mut self.current_scope);
                func.verify(true);
            }
            ASTData::GenFunction(f) => {
                if let ASTData::Function(name, ty_name, args, stats) = *f {
                    let ty = self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .fn_type(
                            &args
                                .clone()
                                .into_iter()
                                .map(|a| self.convert_type(&a.0).into())
                                .collect::<Vec<_>>(),
                            false,
                        );
                    let func = self.module.add_function(&name, ty, None);
                    let basic_block = self.context.append_basic_block(func, "entry");
                    self.builder.position_at_end(basic_block);
                    let get_intrinsic = |s| {
                        Intrinsic::find(s)
                            .unwrap()
                            .get_declaration(self.module, &[])
                            .unwrap()
                    };

                    let co_id = get_intrinsic("llvm.coro.id");
                    let co_alloc = get_intrinsic("llvm.coro.alloc");
                    let co_size = Intrinsic::find("llvm.coro.size.i64")
                        .unwrap()
                        .get_declaration(
                            self.module,
                            &[self.context.i64_type().as_basic_type_enum()],
                        )
                        .unwrap();
                    let co_begin = get_intrinsic("llvm.coro.begin");
                    let co_suspend = get_intrinsic("llvm.coro.suspend");
                    let co_free = get_intrinsic("llvm.coro.free");
                    let co_end = get_intrinsic("llvm.coro.end");

                    let promise = self
                        .builder
                        .build_alloca(self.convert_type(&ty_name).as_basic_type_enum(), "promise")
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

                    let need_alloc = self.manual_call(
                        co_alloc,
                        vec![id.as_value_ref()],
                        "need_alloc".to_owned(),
                    );

                    let dyn_alloc_block = self.context.append_basic_block(func, "dyn_alloc");
                    let coro_begin_block = self.context.append_basic_block(func, "coro_begin");

                    self.builder
                        .build_conditional_branch(
                            need_alloc
                                .try_as_basic_value()
                                .left()
                                .unwrap()
                                .into_int_value(),
                            dyn_alloc_block,
                            coro_begin_block,
                        )
                        .unwrap();

                    self.builder.position_at_end(dyn_alloc_block);
                    let size = self.builder.build_call(co_size, &[], "size").unwrap();
                    let alloca = self.builder.build_call(
                        self.module.get_function("malloc").unwrap(),
                        &[size.try_as_basic_value().unwrap_left().into()],
                        "alloca",
                    ); //TODO make sure malloc is found
                    self.builder
                        .build_unconditional_branch(coro_begin_block)
                        .unwrap();

                    self.builder.position_at_end(coro_begin_block);
                    let phi = self
                        .builder
                        .build_phi(ptr_type.as_basic_type_enum(), "phi")
                        .unwrap();
                    phi.add_incoming(&[
                        (&ptr_type.const_null().as_basic_value_enum(), basic_block),
                        (
                            &alloca.unwrap().try_as_basic_value().left().unwrap(),
                            dyn_alloc_block,
                        ),
                    ]);

                    let hdl = self.manual_call(
                        co_begin,
                        vec![id.as_value_ref(), phi.as_value_ref()],
                        "hdl".to_string(),
                    );

                    let basic_block = self.context.append_basic_block(func, "begin");
                    self.builder
                        .build_unconditional_branch(basic_block)
                        .unwrap();
                    self.builder.position_at_end(basic_block);

                    let cleanup_block = self.context.append_basic_block(func, "cleanup");
                    let suspend_block = self.context.append_basic_block(func, "suspend");

                    Scope::open_scope(
                        &mut self.current_scope,
                        ScopeTy::GenFunction(GenFunction {
                            co_suspend,
                            suspend_block,
                            cleanup_block,
                            promise,
                        }),
                    );
                    self.current_scope.as_mut().unwrap().current_func = Some(func);
                    for (i, arg) in args.iter().enumerate() {
                        let param = func.get_nth_param(i as u32).unwrap();
                        let ty = self.convert_type(&arg.0);
                        let ass_alloca = self.builder.build_alloca(ty, &arg.1).unwrap();
                        self.builder.build_store(ass_alloca, param).unwrap();
                        self.current_scope.as_mut().unwrap().set_value(
                            arg.1.to_owned(),
                            Decl::VariableDecl { ty, val: Some(ass_alloca.as_basic_value_enum()) }
                        );
                    }
                    self.get_type(&stats);
                    self.get_value(&stats);
                    Scope::close_scope(&mut self.current_scope);

                    let token_none =
                        unsafe { LLVMConstNull(LLVMTokenTypeInContext(self.context.as_ctx_ref())) };
                    let ch = self.manual_call(
                        co_suspend,
                        vec![
                            token_none,
                            self.context.bool_type().const_int(1, true).as_value_ref(),
                        ],
                        "ch".to_string(),
                    );
                    let i8t = self.context.i8_type();
                    self.builder
                        .build_switch(
                            ch.as_any_value_enum().into_int_value(),
                            suspend_block,
                            &[(i8t.const_int(1, false), cleanup_block)],
                        )
                        .unwrap();

                    self.builder.position_at_end(cleanup_block);
                    let mem = self.manual_call(
                        co_free,
                        vec![id.as_value_ref(), hdl.as_value_ref()],
                        "mem".to_string(),
                    );
                    self.builder
                        .build_free(mem.as_any_value_enum().into_pointer_value())
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(suspend_block)
                        .unwrap();

                    self.builder.position_at_end(suspend_block);
                    self.builder
                        .build_call(
                            co_end,
                            &[
                                hdl.try_as_basic_value().unwrap_left().into(),
                                self.context.bool_type().const_int(0, false).into(),
                            ],
                            "unused",
                        )
                        .unwrap();
                    self.builder
                        .build_return(Some(&hdl.try_as_basic_value().unwrap_left()))
                        .unwrap();
                    func.verify(true);
                    let s = "presplitcoroutine".to_string();
                    func.add_attribute(
                        inkwell::attributes::AttributeLoc::Function,
                        self.context.create_enum_attribute(
                            unsafe {
                                LLVMGetEnumAttributeKindForName(s.as_ptr() as *const i8, s.len())
                            },
                            0,
                        ),
                    )
                } else {
                };
            }
        }
    }
}
