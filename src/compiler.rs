use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
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
    sym_tab: HashMap<String, BasicValueEnum<'ctx>>,
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

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        match *expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(n).into()),

            Expr::Variable(ref name) => match self.sym_tab.get(name.as_str()) {
                Some(val) => Ok(val.clone()),
                None => Err(CompileError(format!("variable '{name}' undefined"))),
            },

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
                    _ => Err(CompileError(format!("unknown operator '{op}'"))),
                }
            }

            Expr::Call {
                ref callee,
                ref args,
            } => match self.module.get_function(callee.as_str()) {
                Some(func) => {
                    let mut a = Vec::with_capacity(args.len());

                    for arg in args {
                        a.push(self.compile_expr(arg)?);
                    }

                    let a_val: Vec<BasicMetadataValueEnum> =
                        a.iter().map(|&val| val.into()).collect();

                    match self
                        .builder
                        .build_call(func, a_val.as_slice(), "tmp")
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
                let cond = self.compile_expr(cond_expr)?.into_float_value();
                let cond = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        cond,
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
                    .build_conditional_branch(cond, then_bb, else_bb)
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
                let phi = self
                    .builder
                    .build_phi(self.context.f64_type(), "iftmp")
                    .unwrap();
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value())
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

        // Name the arguments
        for (i, arg) in fv.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.params[i].as_str());
        }

        fv
    }

    fn compile_func(&mut self) -> Result<FunctionValue<'ctx>> {
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

        // Create basic block in current context
        let bb = self.context.append_basic_block(fv, "entry");
        // Set builder cursor position to basic block
        self.builder.position_at_end(bb);

        self.sym_tab.clear();
        self.sym_tab.reserve(proto.params.len());
        for (i, param) in fv.get_param_iter().enumerate() {
            self.sym_tab.insert(proto.params[i].clone(), param);
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
) -> Result<FunctionValue<'ctx>> {
    let mut compiler = Compiler::new(context, builder, module, func);
    compiler.compile_func()
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
