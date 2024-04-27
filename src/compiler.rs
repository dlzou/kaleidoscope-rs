use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{FloatPredicate, OptimizationLevel};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

use crate::parser::*;

pub struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,

    func: &'a Function,
    sym_tab: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        func: &'a Function,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            func,
            sym_tab: HashMap::new(),
        }
    }

    fn create_entry_block_alloca(&self, fn_value: FunctionValue<'ctx>, name: &str) -> PointerValue<'ctx> {
        // mem2reg block only checks entry block
        let tmp_b = self.context.create_builder();
        let entry = fn_value.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(inst) => tmp_b.position_before(&inst),
            None => tmp_b.position_at_end(entry),
        }

        tmp_b.build_alloca(self.context.f64_type(), name).unwrap()
    }
    
    fn build_load(&self, val: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder.build_load(self.context.f64_type(), val, name).unwrap()
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        match *expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(n).into()),

            Expr::Variable(ref name) => match self.sym_tab.get(name.as_str()) {
                Some(val) => Ok(self.build_load(*val, name)),
                None => Err(CompileError(format!("variable '{name}' undefined"))),
            },

            Expr::Unary {
                ref op,
                ref operand,
            } => {
                let arg = self.compile_expr(operand)?.into_float_value();
                if let Some(fv) = self.module.get_function(op.as_str()) {
                    let a = vec![arg];
                    let a_val: Vec<BasicMetadataValueEnum> =
                        a.iter().map(|&val| val.into()).collect();

                    if let Some(val) = self
                        .builder
                        .build_call(fv, a_val.as_slice(), "tmp")
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                    {
                        Ok(val)
                    } else {
                        Err(CompileError("invalid call".into()))
                    }
                } else {
                    Err(CompileError(format!("unknown unary operator '{op}'")))
                }
            }

            Expr::Binary {
                ref op,
                ref lhs,
                ref rhs,
            } => {
                let l = self.compile_expr(lhs)?.into_float_value();
                let r = self.compile_expr(rhs)?.into_float_value();

                match (*op).as_str() {
                    "+" => Ok(self.builder.build_float_add(l, r, "addtmp").unwrap().into()),
                    "-" => Ok(self.builder.build_float_sub(l, r, "subtmp").unwrap().into()),
                    "*" => Ok(self.builder.build_float_mul(l, r, "multmp").unwrap().into()),
                    "/" => Ok(self.builder.build_float_div(l, r, "divtmp").unwrap().into()),
                    "<" => {
                        let cmp = self
                            .builder
                            .build_float_compare(FloatPredicate::ULT, l, r, "cmptmp")
                            .unwrap();
                        Ok(self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .unwrap()
                            .into())
                    }
                    ">" => {
                        let cmp = self
                            .builder
                            .build_float_compare(FloatPredicate::ULT, r, l, "cmptmp")
                            .unwrap();
                        Ok(self
                            .builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
                            .unwrap()
                            .into())
                    }

                    b => {
                        if let Some(fv) = self.module.get_function(b) {
                            let a = vec![l, r];
                            let a_val: Vec<BasicMetadataValueEnum> =
                                a.iter().map(|&val| val.into()).collect();

                            if let Some(val) = self
                                .builder
                                .build_call(fv, a_val.as_slice(), "tmp")
                                .unwrap()
                                .try_as_basic_value()
                                .left()
                            {
                                Ok(val)
                            } else {
                                Err(CompileError("invalid call".into()))
                            }
                        } else {
                            Err(CompileError(format!("unknown binary operator '{b}'")))
                        }
                    }
                }
            }

            Expr::Call {
                ref callee,
                ref args,
            } => match self.module.get_function(callee.as_str()) {
                Some(fv) => {
                    let mut a = Vec::with_capacity(args.len());

                    for arg in args {
                        a.push(self.compile_expr(arg)?);
                    }

                    let a_val: Vec<BasicMetadataValueEnum> =
                        a.iter().map(|&val| val.into()).collect();

                    match self
                        .builder
                        .build_call(fv, a_val.as_slice(), "tmp")
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                    {
                        Some(val) => Ok(val),
                        None => Err(CompileError("invalid call".into())),
                    }
                }
                None => Err(CompileError(format!("unknown function '{callee}'"))),
            },

            Expr::If {
                ref cond_expr,
                ref then_expr,
                ref else_expr,
            } => {
                let cond_val = self.compile_expr(cond_expr)?.into_float_value();
                let cond_val = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE, // Ordered and Not Equal
                        cond_val,
                        self.context.f64_type().const_float(0.0).into(),
                        "ifcond",
                    )
                    .unwrap();

                let fv = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let then_bb = self.context.append_basic_block(fv, "then");
                let else_bb = self.context.append_basic_block(fv, "else");
                let cont_bb = self.context.append_basic_block(fv, "ifcont");
                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .unwrap();

                // Build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(then_expr)?;
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                let then_bb = self.builder.get_insert_block().unwrap(); // Compiling then_expr may change current block

                // Build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(else_expr)?;
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                let else_bb = self.builder.get_insert_block().unwrap();

                // Build continued block with Phi node
                self.builder.position_at_end(cont_bb);
                let phi_val = self
                    .builder
                    .build_phi(self.context.f64_type(), "iftmp")
                    .unwrap();
                phi_val.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi_val.as_basic_value())
            }

            Expr::For {
                ref var,
                ref start,
                ref end,
                ref step,
                ref body,
            } => {
                let prehead_bb = self.builder.get_insert_block().unwrap();
                let fv = prehead_bb.get_parent().unwrap();
                
                // Allocate stack space for loop variable
                let loop_var = self.create_entry_block_alloca(fv, var);
                let start_val = self.compile_expr(start)?.into_float_value();
                self.builder.build_store(loop_var, start_val).unwrap();

                let head_bb = self.context.append_basic_block(fv, "head");
                self.builder.build_unconditional_branch(head_bb).unwrap();

                // Build head block, which evaluates end condition
                self.builder.position_at_end(head_bb);
                // let phi_val = self
                //     .builder
                //     .build_phi(self.context.f64_type(), var)
                //     .unwrap();
                // phi_val.add_incoming(&[(&start_val, prehead_bb)]);

                // Set loop variable in symbol table
                let old_var = self.sym_tab.remove(var);
                self.sym_tab
                    .insert(var.to_string(), loop_var);

                let end_val = self.compile_expr(end)?.into_float_value();
                let end_val = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        end_val,
                        self.context.f64_type().const_float(0.0).into(),
                        "endcond",
                    )
                    .unwrap();

                let body_bb = self.context.append_basic_block(fv, "loop");
                let cont_bb = self.context.append_basic_block(fv, "loopcont");
                self.builder
                    .build_conditional_branch(end_val, body_bb, cont_bb)
                    .unwrap();

                // Build loop block
                self.builder.position_at_end(body_bb);
                self.compile_expr(body)?;

                // Update loop variable
                let step_val = match step {
                    Some(expr) => self.compile_expr(expr)?.into_float_value(),
                    None => self.context.f64_type().const_float(1.0),
                };
                let next_val = self
                    .builder
                    .build_float_add(
                        self.build_load(loop_var, "curtmp").into_float_value(),
                        step_val,
                        "nexttmp",
                    )
                    .unwrap();
                self.builder.build_store(loop_var, next_val).unwrap();
                // let body_bb = self.builder.get_insert_block().unwrap();
                // phi_val.add_incoming(&[(&next_val, body_bb)]);
                self.builder.build_unconditional_branch(head_bb).unwrap();

                // Build continued block
                self.builder.position_at_end(cont_bb);
                match old_var {
                    Some(v) => {
                        self.sym_tab.insert(var.to_string(), v);
                    }
                    None => {
                        self.sym_tab.remove(var);
                    }
                }

                Ok(self.context.f64_type().const_float(0.0).into())
            }
        }
    }

    fn compile_proto(&self, proto: &Prototype) -> FunctionValue<'ctx> {
        // Build type information for function, assuming everything is f64
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.params.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = self
            .context
            .f64_type()
            .fn_type(args_types.as_slice(), false);
        let fv = self.module.add_function(proto.name.as_str(), fn_type, None);

        // Name the parameters
        for (i, param) in fv.get_param_iter().enumerate() {
            param.into_float_value().set_name(proto.params[i].as_str());
        }

        fv
    }

    fn compile_func(
        &mut self,
        op_prec: &mut HashMap<String, usize>,
    ) -> Result<FunctionValue<'ctx>> {
        // Check for previous declarations or definitions
        let proto = &self.func.proto;
        let fv_old = self.module.get_function(proto.name.as_str());
        let fv = match fv_old {
            Some(fv) => {
                let fv_params = fv.get_params(); // fv moved?
                if fv_params.len() != proto.params.len() {
                    return Err(CompileError(
                        "function signature does not match previous declaration".into(),
                    ));
                }

                let matches = fv_params
                    .iter()
                    .zip(proto.params.clone())
                    .filter(|(f, p)| f.get_name().to_str().unwrap() == p)
                    .count();
                if matches < std::cmp::max(fv_params.len(), proto.params.len()) {
                    return Err(CompileError(
                        "function signature does not match previous declaration".into(),
                    ));
                }

                if fv.count_basic_blocks() > 0 {
                    return Err(CompileError("function cannot be redefined".into()));
                }
                fv
            }
            None => self.compile_proto(proto),
        };

        if self.func.body.is_none() {
            return Ok(fv);
        }

        if proto.is_op && proto.params.len() == 2 {
            op_prec.insert(proto.name.clone(), proto.precedence);
        }

        // Create basic block in current context
        let bb = self.context.append_basic_block(fv, "entry");
        // Set builder cursor position to basic block
        self.builder.position_at_end(bb);

        self.sym_tab.clear();
        self.sym_tab.reserve(proto.params.len());
        for (i, param) in fv.get_param_iter().enumerate() {
            let name = proto.params[i].clone();
            let alloca = self.create_entry_block_alloca(fv, &name);
            self.builder.build_store(alloca, param).unwrap();
            self.sym_tab.insert(name, alloca);
        }

        // Builder generates code at cursor position
        let body = match self.compile_expr(self.func.body.as_ref().unwrap()) {
            Ok(expr) => expr,
            Err(e) => {
                unsafe {
                    fv.delete();
                }
                return Err(e.into());
            }
        };
        self.builder.build_return(Some(&body)).unwrap();

        if fv.verify(true) {
            Ok(fv)
        } else {
            unsafe {
                fv.delete();
            }
            Err(CompileError("invalid function".into()))
        }
    }
}

pub fn compile<'a, 'ctx>(
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    func: &Function,
    op_prec: &mut HashMap<String, usize>,
) -> Result<FunctionValue<'ctx>> {
    let mut compiler = Compiler::new(context, builder, module, func);
    compiler.compile_func(op_prec)
}

// #[llvm_versions(16.0..=latest)]
pub fn run_passes(module: &Module) {
    // New LLVM  pass manager
    targets::Target::initialize_all(&targets::InitializationConfig::default());
    let target_triple = targets::TargetMachine::get_default_triple();
    let target = targets::Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::None,
            targets::RelocMode::PIC, // i.e. link-time relocation; PIC = position-independent code
            targets::CodeModel::Default,
        )
        .unwrap();

    let passes: &[&str] = &[
        "instcombine", // Some peephole optimizations
        "reassociate", // Re-associate expressions to match more common expressions
        "gvn",         // Common sub-expression elimination
        "simplifycfg", // Remove unreachable blocks
        "mem2reg",     // Promote alloca to register
    ];

    module
        .run_passes(
            passes.join(",").as_str(),
            &target_machine,
            PassBuilderOptions::create(),
        )
        .unwrap();
}

type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError(String);

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Error for CompileError {}
