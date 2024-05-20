use inkwell::values::{BasicValueEnum, FunctionValue};

use inkwell::types::BasicTypeEnum;

use std::collections::HashMap;
use std::mem;

use inkwell::basic_block::BasicBlock;
use inkwell::values::PointerValue as Op;

use crate::backend::type_gen::Decl;

#[derive(Clone)]
pub struct GenFunction<'ctx> {
    pub co_suspend: FunctionValue<'ctx>,
    pub suspend_block: BasicBlock<'ctx>,
    pub cleanup_block: BasicBlock<'ctx>,
    pub promise: Op<'ctx>,
}

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
    pub(crate) parent_scope: Option<Box<Scope<'ctx>>>,
    pub(crate) defs: HashMap<String, Decl<'ctx>>,
    pub(crate) ty: ScopeTy<'ctx>,
    pub current_func: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Scope<'ctx> {
    pub(crate) fn open(self: Box<Self>, ty: ScopeTy<'ctx>) -> Box<Self> {
        let mut new_scope = Scope::default();
        new_scope.parent_scope = Some(self);
        new_scope.ty = ty;
        Box::new(new_scope)
    }

    pub(crate) fn close(self) -> Box<Self> {
        self.parent_scope.unwrap()
    }

    pub(crate) fn open_scope(s: &mut Option<Box<Self>>, ty: ScopeTy<'ctx>) {
        *s = Some(mem::take(s).unwrap().open(ty));
    }

    pub(crate) fn close_scope(s: &mut Option<Box<Self>>) {
        *s = Some(mem::take(s).unwrap().close())
    }

    pub(crate) fn get_value(
        &self,
        name: &String,
    ) -> Option<&Decl> {
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

    pub(crate) fn set_value(
        &mut self,
        name: String,
        decl: Decl<'ctx>
    ) -> bool {
        self.defs.insert(name, decl).is_some()
    }

    pub(crate) fn get_function(&self) -> Option<FunctionValue<'ctx>> {
        let s = &self.current_func;
        if s.is_some() {
            return *s;
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if current_scope.current_func.is_some() {
                    return current_scope.current_func;
                }
            }
        }

        None
    }
    pub(crate) fn get_gen_function(&self) -> Option<GenFunction> {
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
    pub(crate) fn get_assign(&self) -> Option<String> {
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

    pub(crate) fn get_while(&self) -> Option<ScopeTy> {
        let s = &self.ty;
        if let ScopeTy::Loop {
            cond_block: _,
            after_block: _,
        } = s
        {
            return Some(s.clone());
        } else {
            let mut current_scope = self;
            while current_scope.parent_scope.is_some() {
                current_scope = current_scope.parent_scope.as_ref().unwrap();
                if let ScopeTy::Assign(_) = &current_scope.ty {
                    return Some(current_scope.ty.clone());
                }
            }
        }

        None
    }
}
