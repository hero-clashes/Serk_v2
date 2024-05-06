
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use inkwell::
    types::{BasicType, BasicTypeEnum, FunctionType}
;

use once_cell::sync::Lazy;
use regex::Regex;

use crate::statement::*;
use super::Backend;

static INT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"[ui]([0-9]+)").unwrap());
static FLOAT_TYPE: Lazy<Regex> = Lazy::new(|| Regex::new(r"f([0-9]+)").unwrap());

impl<'a, 'ctx> Backend<'a, 'ctx> {
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
                        self.print_error(d);
                    }
                }
                Some(assign_ty)
            }
            StatementData::Math(l, _, r) => {
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
                    self.print_error(d);
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
                    self.print_error(d);
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
                    self.print_error(d);
                }
            },
            StatementData::Return(s) => {
                let f = self.current_scope.as_ref().unwrap().get_function().unwrap();
                let ty = if s.is_some(){self.get_type(&s.as_ref().unwrap())} else{None};
                
                let get_return_type = f.get_type().get_return_type();

                if let Some(_) = s{
                    
                    if get_return_type != ty{
                        let d = Diagnostic::<usize>::error()
                        .with_message("Return Ty don't match function signature")
                        .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                        .with_notes(vec![format!("function returns type is {}, the return statement type is {}",get_return_type.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string()),ty.map(|s| s.print_to_string().to_string()).unwrap_or("()".to_string()))]);
                        self.print_error(d);
                    }
                    return ty;
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
                        let get_value = self.current_scope.as_ref().unwrap().get_value(&name);
                        if get_value.is_none() {
                            let d = Diagnostic::<usize>::error()
                            .with_message(format!("Can't find variable named: {name}"))
                            .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                            self.print_error(d);
                        }
                        let basic_type_enum = get_value.unwrap().0;
                        if basic_type_enum != s{
                            let d = Diagnostic::<usize>::error()
                            .with_message("Assign Ty doesn't match the variable ty")
                            .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                            .with_notes(vec![format!("Variable type is {}, the return statement type is {}",basic_type_enum.print_to_string(),s.print_to_string())]);
                            self.print_error(d);
                        }
                    },
                    None => {
                        let d = Diagnostic::<usize>::error()
                            .with_message("Can't Assign to a variable with an void type statement")
                            .with_labels(vec![Label::primary(self.current_file, stat.to_rng())]);
                        self.print_error(d);
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
                                    self.print_error(d);
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
                    self.print_error(d);
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
            StatementData::Yield(_) => {
                // if let Some(s) = s{
                //     let f = self.current_scope.as_ref().unwrap().get_function().unwrap();
                //     let ty = self.get_type(&s).unwrap();
                    
                //     let get_return_type = f.get_type().get_return_type();
                //     if get_return_type != Some(ty){
                //         let d = Diagnostic::<usize>::error()
                //         .with_message("Return Ty don't match function signature")
                //         .with_labels(vec![Label::primary(self.current_file, stat.to_rng())])
                //         .with_notes(vec![format!("function returns type is {}, the return statement type is {}",get_return_type.unwrap().print_to_string(),ty.print_to_string())]);
                //         self.print_error(d);
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
        self.print_error(d);

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
}