//! Formal verification support for the deliberately small 0.2 pure subset.
//!
//! The translator emits SMT-LIB for acyclic `Int`/`Bool` MIR. Each control-flow
//! path is checked independently, including arithmetic safety. Solvers are
//! optional: lack of a configured solver is reported as `UNKNOWN`, never proof.

use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use crate::mir::{
    BinOp, Constant, Function, Operand, Place, Program, ProjectionElem, Rvalue, StatementKind,
    Terminator, UnOp,
};
use crate::parser::{BinOp as AstBinOp, Expr, ExprKind, LiteralKind, UnaryOp};
use crate::types::Ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmtObligation {
    pub function: String,
    /// Decision-only proof query. This exits cleanly for `unsat`.
    pub script: String,
    /// Proof query followed by a model-value request, used only after `sat`.
    pub counterexample_script: String,
    /// Satisfiability query for the declared parameter domain and preconditions.
    /// An `unsat` result means the main proof would be vacuous.
    pub assumptions_script: String,
    pub parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormalResult {
    Proved,
    Counterexample(Vec<(String, String)>),
    Unknown(String),
}

pub fn build_smt_obligation(function: &Function) -> Result<SmtObligation, String> {
    build_smt_obligation_inner(None, function)
}

/// Build an obligation with access to other pure functions. Direct calls are
/// symbolically inlined; recursive calls remain unsupported until function
/// invariants are part of the proof language.
pub fn build_smt_obligation_in_program(
    program: &Program,
    function: &Function,
) -> Result<SmtObligation, String> {
    build_smt_obligation_inner(Some(program), function)
}

fn build_smt_obligation_inner(
    program: Option<&Program>,
    function: &Function,
) -> Result<SmtObligation, String> {
    let mut locals = HashMap::new();
    let mut declarations = Vec::new();
    let mut domain_constraints = Vec::new();
    let mut parameters = Vec::new();
    for (index, (local, ty)) in function.params.iter().enumerate() {
        let name = function
            .param_names
            .get(index)
            .map(|(name, _)| smt_name(name))
            .unwrap_or_else(|| format!("arg{index}"));
        let value = symbolic_input(
            program,
            &name,
            ty,
            &mut declarations,
            &mut domain_constraints,
        )?;
        value.collect_scalar_expressions(&mut parameters);
        locals.insert(local.0, value);
    }
    let contract_names: HashMap<String, SymbolicValue> = function
        .param_names
        .iter()
        .enumerate()
        .filter_map(|(index, (name, _))| {
            function
                .params
                .get(index)
                .and_then(|(local, _)| locals.get(&local.0))
                .cloned()
                .map(|value| (name.clone(), value))
        })
        .collect();
    let mut preconditions = function
        .preconditions
        .iter()
        .filter_map(|contract| contract.condition.as_deref())
        .map(|condition| translate_contract_expr(condition, &contract_names))
        .collect::<Result<Vec<_>, _>>()?;
    let mut precondition_safety = function
        .preconditions
        .iter()
        .filter_map(|contract| contract.condition.as_deref())
        .map(|condition| contract_arithmetic_safety(condition, &contract_names))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    if let Some(program) = program {
        for (local, _) in &function.params {
            let value = locals
                .get(&local.0)
                .ok_or_else(|| format!("parameter {local} has no symbolic value"))?;
            let invariants = symbolic_invariants(program, value)?;
            preconditions.extend(invariants.conditions);
            precondition_safety.extend(invariants.safety);
        }
    }

    let domain = conjunction(&domain_constraints);
    let pre = conjunction(&preconditions);
    let pre_safety = conjunction(&precondition_safety);
    let mut paths = Vec::new();
    let mut active = HashSet::new();
    explore_block(
        program,
        function,
        function.entry_block,
        locals,
        Vec::new(),
        Vec::new(),
        &mut active,
        &mut paths,
    )?;
    if paths.is_empty() {
        return Err("formal subset found no value-returning path".into());
    }
    let bad_paths = paths
        .into_iter()
        .map(|path| {
            let mut names = contract_names.clone();
            names.insert("result".to_string(), path.result.clone());
            let conditions = function
                .postconditions
                .iter()
                .filter_map(|contract| contract.condition.as_deref())
                .collect::<Vec<_>>();
            let mut postconditions = conditions
                .iter()
                .map(|condition| translate_contract_expr(condition, &names))
                .collect::<Result<Vec<_>, _>>()?;
            let path_condition = conjunction(&path.conditions);
            let mut safety_conditions = path.safety;
            for condition in conditions {
                safety_conditions.extend(contract_arithmetic_safety(condition, &names)?);
            }
            if let Some(program) = program {
                let invariants = symbolic_invariants(program, &path.result)?;
                postconditions.extend(invariants.conditions);
                safety_conditions.extend(invariants.safety);
            }
            let safety = conjunction(&safety_conditions);
            Ok(format!(
                "(and {path_condition} (or (not {safety}) (and {safety} (not {}))))",
                conjunction(&postconditions)
            ))
        })
        .collect::<Result<Vec<_>, String>>()?;

    let mut assumption_lines = vec!["(set-logic ALL)".to_string()];
    assumption_lines.extend(declarations.clone());
    assumption_lines.push(format!("(assert (and {domain} {pre_safety} {pre}))"));
    assumption_lines.push("(check-sat)".to_string());
    if !parameters.is_empty() {
        assumption_lines.push(format!("(get-value ({}))", parameters.join(" ")));
    }
    let mut lines = vec!["(set-logic ALL)".to_string()];
    lines.extend(declarations);
    lines.push(format!(
        "(assert (and {domain} (or (not {pre_safety}) (and {pre_safety} {pre} {}))))",
        disjunction(&bad_paths)
    ));
    lines.push("(check-sat)".to_string());
    let mut counterexample_lines = lines.clone();
    if !parameters.is_empty() {
        counterexample_lines.push(format!("(get-value ({}))", parameters.join(" ")));
    }
    Ok(SmtObligation {
        function: function.name.clone(),
        script: format!("{}\n", lines.join("\n")),
        counterexample_script: format!("{}\n", counterexample_lines.join("\n")),
        assumptions_script: format!("{}\n", assumption_lines.join("\n")),
        parameters,
    })
}

pub fn solver_version(solver: &str) -> Result<String, String> {
    let output = Command::new(solver)
        .arg("--version")
        .output()
        .map_err(|error| format!("cannot start SMT solver: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "SMT solver version command failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

pub fn check_assumptions(
    obligation: &SmtObligation,
    solver: &str,
    timeout: Duration,
) -> FormalResult {
    let assumption_obligation = SmtObligation {
        function: obligation.function.clone(),
        script: obligation.assumptions_script.clone(),
        counterexample_script: obligation.assumptions_script.clone(),
        assumptions_script: obligation.assumptions_script.clone(),
        parameters: obligation.parameters.clone(),
    };
    run_solver_with_timeout(&assumption_obligation, solver, timeout)
}

const MAX_FORMAL_PATHS: usize = 4096;

#[derive(Debug)]
struct SymbolicPath {
    result: SymbolicValue,
    conditions: Vec<String>,
    safety: Vec<String>,
}

/// Solver-facing values produced by the symbolic MIR interpreter.
///
/// Aggregates remain structural here instead of becoming a second executable
/// program representation. Only scalar leaves are emitted into SMT.
#[derive(Debug, Clone, PartialEq, Eq)]
enum SymbolicValue {
    Scalar {
        expression: String,
        ty: Ty,
    },
    Tuple(Vec<SymbolicValue>),
    Struct {
        type_name: String,
        fields: Vec<(String, SymbolicValue)>,
    },
}

impl SymbolicValue {
    fn scalar(expression: impl Into<String>, ty: Ty) -> Self {
        Self::Scalar {
            expression: expression.into(),
            ty,
        }
    }

    fn scalar_expression(&self) -> Result<&str, String> {
        match self {
            Self::Scalar { expression, .. } => Ok(expression),
            _ => Err("aggregate value used where an SMT scalar was required".into()),
        }
    }

    fn bool_expression(&self) -> Result<&str, String> {
        match self {
            Self::Scalar {
                expression,
                ty: Ty::Bool,
            } => Ok(expression),
            Self::Scalar { ty, .. } => {
                Err(format!("expected a Boolean symbolic value, found `{ty}`"))
            }
            _ => Err("expected a Boolean symbolic value, found an aggregate".into()),
        }
    }

    fn collect_scalar_expressions(&self, output: &mut Vec<String>) {
        match self {
            Self::Scalar { expression, .. } => output.push(expression.clone()),
            Self::Tuple(elements) => {
                for element in elements {
                    element.collect_scalar_expressions(output);
                }
            }
            Self::Struct { fields, .. } => {
                for (_, value) in fields {
                    value.collect_scalar_expressions(output);
                }
            }
        }
    }

    fn ty(&self) -> Ty {
        match self {
            Self::Scalar { ty, .. } => ty.clone(),
            Self::Tuple(elements) => Ty::Tuple(elements.iter().map(Self::ty).collect()),
            Self::Struct { type_name, .. } => {
                Ty::Named(crate::types::TypeId::new(type_name), vec![])
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn explore_block(
    program: Option<&Program>,
    function: &Function,
    block_id: crate::mir::BlockId,
    mut locals: HashMap<u32, SymbolicValue>,
    conditions: Vec<String>,
    mut safety: Vec<String>,
    active: &mut HashSet<(String, crate::mir::BlockId)>,
    paths: &mut Vec<SymbolicPath>,
) -> Result<(), String> {
    if paths.len() >= MAX_FORMAL_PATHS {
        return Err(format!(
            "formal path limit of {MAX_FORMAL_PATHS} exceeded; split the function or add summaries"
        ));
    }
    let active_key = (function.name.clone(), block_id);
    if !active.insert(active_key.clone()) {
        return Err(format!(
            "formal subset does not yet support loops (cycle reaches {block_id})"
        ));
    }
    let result = (|| {
        let block = function
            .blocks
            .get(block_id.0 as usize)
            .filter(|block| block.id == block_id)
            .ok_or_else(|| format!("invalid MIR block {block_id}"))?;
        for statement in &block.stmts {
            match &statement.kind {
                StatementKind::Assign(local, rvalue) => {
                    if function
                        .params
                        .iter()
                        .any(|(parameter, _)| parameter == local)
                    {
                        return Err(format!(
                            "formal parameter reassignment of {local} requires distinct entry/exit contract symbols"
                        ));
                    }
                    let destination = function
                        .locals
                        .get(local.0 as usize)
                        .ok_or_else(|| format!("assignment targets undeclared local {local}"))?;
                    validate_rvalue_type(program, function, rvalue, &destination.ty)?;
                    let translated =
                        translate_rvalue(program, rvalue, &locals, Some(&destination.ty))?;
                    locals.insert(local.0, translated.value);
                    safety.extend(translated.safety);
                }
                StatementKind::AssignPlace(place, rvalue) => {
                    let destination_ty = translate_place(place, &locals)?.ty();
                    validate_rvalue_type(program, function, rvalue, &destination_ty)?;
                    let translated =
                        translate_rvalue(program, rvalue, &locals, Some(&destination_ty))?;
                    assign_symbolic_place(place, &mut locals, translated.value)?;
                    safety.extend(translated.safety);
                }
                StatementKind::Drop(_) | StatementKind::Nop => {}
                _ => {
                    return Err(
                        "formal subset does not support indexed mutation or drop glue".into(),
                    );
                }
            }
        }
        match block
            .terminator
            .as_ref()
            .ok_or_else(|| format!("MIR block {block_id} has no terminator"))?
        {
            Terminator::Return(Some(returned)) => {
                let returned = translate_operand(returned, &locals)?;
                if returned.ty() != function.return_ty {
                    return Err(format!(
                        "MIR return type mismatch: expected `{}`, found `{}`",
                        function.return_ty,
                        returned.ty()
                    ));
                }
                paths.push(SymbolicPath {
                    result: returned,
                    conditions,
                    safety,
                });
                Ok(())
            }
            Terminator::Return(None) => {
                Err("formal subset requires a value-returning function".into())
            }
            Terminator::Goto(next) => explore_block(
                program, function, *next, locals, conditions, safety, active, paths,
            ),
            Terminator::If {
                cond,
                then_block,
                else_block,
            } => {
                let condition = translate_operand(cond, &locals)?
                    .bool_expression()?
                    .to_string();
                let mut then_conditions = conditions.clone();
                then_conditions.push(condition.clone());
                explore_block(
                    program,
                    function,
                    *then_block,
                    locals.clone(),
                    then_conditions,
                    safety.clone(),
                    active,
                    paths,
                )?;
                let mut else_conditions = conditions;
                else_conditions.push(format!("(not {condition})"));
                explore_block(
                    program,
                    function,
                    *else_block,
                    locals,
                    else_conditions,
                    safety,
                    active,
                    paths,
                )
            }
            Terminator::Switch {
                operand,
                targets,
                default,
            } => {
                let value = translate_operand(operand, &locals)?
                    .scalar_expression()?
                    .to_string();
                let mut excluded = Vec::new();
                for (literal, target) in targets {
                    let equality = format!("(= {value} {literal})");
                    excluded.push(format!("(not {equality})"));
                    let mut branch_conditions = conditions.clone();
                    branch_conditions.push(equality);
                    explore_block(
                        program,
                        function,
                        *target,
                        locals.clone(),
                        branch_conditions,
                        safety.clone(),
                        active,
                        paths,
                    )?;
                }
                let mut default_conditions = conditions;
                default_conditions.extend(excluded);
                explore_block(
                    program,
                    function,
                    *default,
                    locals,
                    default_conditions,
                    safety,
                    active,
                    paths,
                )
            }
            Terminator::Call {
                func,
                args,
                dest,
                next,
                ..
            } => {
                let program =
                    program.ok_or("direct calls require program-aware formal verification")?;
                let callee = program
                    .functions
                    .get(func)
                    .ok_or_else(|| format!("formal call target `{func}` is unavailable"))?;
                if args.len() != callee.params.len() {
                    return Err(format!(
                        "formal call `{func}` has {} argument(s), expected {}",
                        args.len(),
                        callee.params.len()
                    ));
                }
                let Some(destination) = *dest else {
                    return Err("formal pure calls currently require a value destination".into());
                };
                let destination_ty = function
                    .locals
                    .get(destination.0 as usize)
                    .ok_or_else(|| format!("call targets undeclared local {destination}"))?;
                if destination_ty.ty != callee.return_ty {
                    return Err(format!(
                        "formal call result type mismatch: `{func}` returns `{}`, destination is `{}`",
                        callee.return_ty, destination_ty.ty
                    ));
                }

                let mut callee_locals = HashMap::new();
                let mut callee_names = HashMap::new();
                let mut callee_entry_invariants = SymbolicInvariants::default();
                for (index, ((parameter, parameter_ty), argument)) in
                    callee.params.iter().zip(args).enumerate()
                {
                    let argument_ty = translate_operand(argument, &locals)?.ty();
                    if argument_ty != *parameter_ty {
                        return Err(format!(
                            "formal call argument {index} type mismatch: expected `{parameter_ty}`, found `{argument_ty}`"
                        ));
                    }
                    let value = translate_operand(argument, &locals)?;
                    let invariants = symbolic_invariants(program, &value)?;
                    callee_entry_invariants
                        .conditions
                        .extend(invariants.conditions);
                    callee_entry_invariants.safety.extend(invariants.safety);
                    callee_locals.insert(parameter.0, value.clone());
                    if let Some((name, _)) = callee.param_names.get(index) {
                        callee_names.insert(name.clone(), value);
                    }
                }
                let mut call_safety = safety.clone();
                call_safety.extend(callee_entry_invariants.safety);
                call_safety.extend(callee_entry_invariants.conditions);
                for condition in callee
                    .preconditions
                    .iter()
                    .filter_map(|contract| contract.condition.as_deref())
                {
                    call_safety.extend(contract_arithmetic_safety(condition, &callee_names)?);
                    call_safety.push(translate_contract_expr(condition, &callee_names)?);
                }
                let mut callee_paths = Vec::new();
                explore_block(
                    Some(program),
                    callee,
                    callee.entry_block,
                    callee_locals,
                    conditions.clone(),
                    call_safety,
                    active,
                    &mut callee_paths,
                )?;
                for mut callee_path in callee_paths {
                    let invariants = symbolic_invariants(program, &callee_path.result)?;
                    callee_path.safety.extend(invariants.safety);
                    callee_path.safety.extend(invariants.conditions);
                    let mut continued_locals = locals.clone();
                    continued_locals.insert(destination.0, callee_path.result);
                    explore_block(
                        Some(program),
                        function,
                        *next,
                        continued_locals,
                        callee_path.conditions,
                        callee_path.safety,
                        active,
                        paths,
                    )?;
                }
                Ok(())
            }
            Terminator::Unreachable => Ok(()),
            _ => Err("tasks and indirect control flow require formal summaries".into()),
        }
    })();
    active.remove(&active_key);
    result
}

pub fn run_solver(obligation: &SmtObligation, solver: &str) -> FormalResult {
    run_solver_with_timeout(obligation, solver, Duration::from_secs(5))
}

pub fn run_solver_with_timeout(
    obligation: &SmtObligation,
    solver: &str,
    timeout: Duration,
) -> FormalResult {
    let decision = match run_solver_query(&obligation.script, solver, timeout) {
        Ok(output) => output,
        Err(reason) => return FormalResult::Unknown(reason),
    };
    match decision.stdout.lines().next().map(str::trim) {
        Some("unsat") => FormalResult::Proved,
        Some("sat") if obligation.parameters.is_empty() => FormalResult::Counterexample(vec![]),
        Some("sat") => {
            let model = match run_solver_query(&obligation.counterexample_script, solver, timeout) {
                Ok(output) => output,
                Err(reason) => return FormalResult::Unknown(reason),
            };
            match model.stdout.lines().next().map(str::trim) {
                Some("sat") => FormalResult::Counterexample(parse_model_values(
                    &model.stdout,
                    &obligation.parameters,
                )),
                Some("unknown") => {
                    FormalResult::Unknown("solver returned unknown while extracting model".into())
                }
                _ if !model.success => FormalResult::Unknown(format!(
                    "SMT solver model error: {}",
                    model.stderr.trim()
                )),
                other => FormalResult::Unknown(format!(
                    "unexpected SMT solver model response: {other:?}"
                )),
            }
        }
        Some("unknown") => FormalResult::Unknown("solver returned unknown".to_string()),
        _ if !decision.success => {
            FormalResult::Unknown(format!("SMT solver error: {}", decision.stderr.trim()))
        }
        other => FormalResult::Unknown(format!("unexpected SMT solver response: {other:?}")),
    }
}

struct SolverQueryOutput {
    success: bool,
    stdout: String,
    stderr: String,
}

fn run_solver_query(
    script: &str,
    solver: &str,
    timeout: Duration,
) -> Result<SolverQueryOutput, String> {
    let mut command = Command::new(solver);
    command
        .args(["-in", "-smt2"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        // A private process group lets a timeout terminate solver descendants,
        // not just the immediate process.
        command.process_group(0);
    }
    let mut child = match command.spawn() {
        Ok(child) => child,
        Err(error) => return Err(format!("cannot start SMT solver: {error}")),
    };
    if let Some(stdin) = child.stdin.as_mut()
        && let Err(error) = stdin.write_all(script.as_bytes())
    {
        return Err(format!("cannot write SMT obligation: {error}"));
    }
    // Batch solvers and solver wrappers commonly wait for EOF before answering.
    // Closing stdin also prevents the verifier from deadlocking until timeout.
    drop(child.stdin.take());
    let deadline = Instant::now() + timeout;
    loop {
        match child.try_wait() {
            Ok(Some(_)) => break,
            Ok(None) if Instant::now() < deadline => {
                std::thread::sleep(Duration::from_millis(10));
            }
            Ok(None) => {
                terminate_solver(&mut child);
                let _ = child.wait();
                return Err(format!(
                    "SMT solver exceeded {} ms and was terminated",
                    timeout.as_millis()
                ));
            }
            Err(error) => {
                terminate_solver(&mut child);
                let _ = child.wait();
                return Err(format!("cannot monitor SMT solver: {error}"));
            }
        }
    }
    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(error) => return Err(format!("SMT solver failed: {error}")),
    };
    Ok(SolverQueryOutput {
        success: output.status.success(),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    })
}

fn terminate_solver(child: &mut std::process::Child) {
    #[cfg(unix)]
    {
        // SAFETY: the child was placed in a fresh process group whose ID is its
        // PID. A negative PID targets exactly that group.
        unsafe {
            libc::kill(-(child.id() as i32), libc::SIGKILL);
        }
    }
    #[cfg(not(unix))]
    {
        let _ = child.kill();
    }
}

fn symbolic_input(
    program: Option<&Program>,
    name: &str,
    ty: &Ty,
    declarations: &mut Vec<String>,
    domain_constraints: &mut Vec<String>,
) -> Result<SymbolicValue, String> {
    match ty {
        Ty::Tuple(elements) => elements
            .iter()
            .enumerate()
            .map(|(index, element)| {
                symbolic_input(
                    program,
                    &format!("{name}__{index}"),
                    element,
                    declarations,
                    domain_constraints,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .map(SymbolicValue::Tuple),
        Ty::Named(id, arguments) if arguments.is_empty() => {
            let program = program.ok_or_else(|| {
                format!(
                    "named aggregate `{}` requires program-aware formal verification",
                    id.name
                )
            })?;
            let layout = program
                .struct_fields
                .get(&id.name)
                .ok_or_else(|| format!("named type `{}` has no formal struct layout", id.name))?;
            let fields = layout
                .iter()
                .map(|(field, field_ty)| {
                    symbolic_input(
                        Some(program),
                        &format!("{name}__{}", smt_component(field)),
                        field_ty,
                        declarations,
                        domain_constraints,
                    )
                    .map(|value| (field.clone(), value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(SymbolicValue::Struct {
                type_name: id.name.clone(),
                fields,
            })
        }
        _ => {
            let sort = smt_sort(ty)?;
            declarations.push(format!("(declare-const {name} {sort})"));
            if let Some((minimum, maximum)) = integer_bounds(ty)? {
                domain_constraints
                    .push(format!("(and (<= {minimum} {name}) (<= {name} {maximum}))"));
            }
            Ok(SymbolicValue::scalar(name, ty.clone()))
        }
    }
}

fn smt_component(name: &str) -> String {
    name.chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || character == '_' {
                character
            } else {
                '_'
            }
        })
        .collect()
}

fn smt_sort(ty: &Ty) -> Result<&'static str, String> {
    match ty {
        Ty::Int | Ty::I64 | Ty::Isize => Ok("Int"),
        Ty::Bool => Ok("Bool"),
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::I128
        | Ty::UInt
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::U64
        | Ty::U128
        | Ty::Usize => Err(format!(
            "type `{ty}` awaits runtime-aligned fixed-width SMT semantics"
        )),
        _ => Err(format!("type `{ty}` is outside the formal pure subset")),
    }
}

fn integer_bounds(ty: &Ty) -> Result<Option<(String, String)>, String> {
    match ty {
        Ty::Int | Ty::I64 | Ty::Isize => Ok(Some((smt_integer(i64::MIN), smt_integer(i64::MAX)))),
        Ty::Bool => Ok(None),
        _ => {
            smt_sort(ty)?;
            Ok(None)
        }
    }
}

#[derive(Debug)]
struct TranslatedRvalue {
    value: SymbolicValue,
    safety: Vec<String>,
}

fn translate_rvalue(
    program: Option<&Program>,
    rvalue: &Rvalue,
    locals: &HashMap<u32, SymbolicValue>,
    destination_ty: Option<&Ty>,
) -> Result<TranslatedRvalue, String> {
    match rvalue {
        Rvalue::Use(operand) => Ok(TranslatedRvalue {
            value: translate_operand(operand, locals)?,
            safety: Vec::new(),
        }),
        Rvalue::BinaryOp(operator, left, right) => {
            let left = translate_operand(left, locals)?;
            let right = translate_operand(right, locals)?;
            let destination_ty = destination_ty.ok_or("binary destination has no type")?;
            let expression = match operator {
                BinOp::Eq => symbolic_equality(&left, &right)?,
                BinOp::Ne => format!("(not {})", symbolic_equality(&left, &right)?),
                BinOp::Div | BinOp::Rem => truncating_integer_operation(
                    *operator,
                    left.scalar_expression()?,
                    right.scalar_expression()?,
                ),
                _ => format!(
                    "({} {} {})",
                    mir_binop(*operator)?,
                    left.scalar_expression()?,
                    right.scalar_expression()?
                ),
            };
            let mut safety = Vec::new();
            if matches!(
                operator,
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
            ) && let Some((minimum, maximum)) = integer_bounds(destination_ty)?
            {
                safety.push(format!(
                    "(and (<= {minimum} {expression}) (<= {expression} {maximum}))"
                ));
            }
            if matches!(operator, BinOp::Div | BinOp::Rem) {
                let left = left.scalar_expression()?;
                let right = right.scalar_expression()?;
                safety.push(format!("(distinct {right} 0)"));
                safety.push(format!(
                    "(not (and (= {left} {}) (= {right} (- 1))))",
                    smt_integer(i64::MIN)
                ));
            }
            Ok(TranslatedRvalue {
                value: SymbolicValue::scalar(expression, destination_ty.clone()),
                safety,
            })
        }
        Rvalue::UnaryOp(operator, operand) => {
            let operand = translate_operand(operand, locals)?;
            let operand = operand.scalar_expression()?;
            let expression = format!(
                "({} {operand})",
                match operator {
                    UnOp::Neg => "-",
                    UnOp::Not => "not",
                    UnOp::BitNot => return Err("bitwise not is outside the SMT subset".into()),
                }
            );
            let mut safety = Vec::new();
            if *operator == UnOp::Neg
                && let Some((minimum, maximum)) =
                    integer_bounds(destination_ty.ok_or("negation destination has no type")?)?
            {
                safety.push(format!(
                    "(and (<= {minimum} {expression}) (<= {expression} {maximum}))"
                ));
            }
            Ok(TranslatedRvalue {
                value: SymbolicValue::scalar(
                    expression,
                    destination_ty
                        .ok_or("unary destination has no type")?
                        .clone(),
                ),
                safety,
            })
        }
        Rvalue::Tuple(operands) => Ok(TranslatedRvalue {
            value: SymbolicValue::Tuple(
                operands
                    .iter()
                    .map(|operand| translate_operand(operand, locals))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            safety: Vec::new(),
        }),
        Rvalue::Struct(type_name, fields) => {
            let value = SymbolicValue::Struct {
                type_name: type_name.clone(),
                fields: fields
                    .iter()
                    .map(|(name, operand)| {
                        translate_operand(operand, locals).map(|value| (name.clone(), value))
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            };
            let invariants = match program {
                Some(program) => symbolic_invariants(program, &value)?,
                None => SymbolicInvariants::default(),
            };
            let mut safety = invariants.safety;
            safety.extend(invariants.conditions);
            Ok(TranslatedRvalue { value, safety })
        }
        Rvalue::Field(operand, field) => {
            let value = translate_operand(operand, locals)?;
            let SymbolicValue::Struct { fields, .. } = value else {
                return Err("field projection requires a symbolic struct".into());
            };
            let value = fields
                .into_iter()
                .find_map(|(name, value)| (name == *field).then_some(value))
                .ok_or_else(|| format!("symbolic struct has no field `{field}`"))?;
            Ok(TranslatedRvalue {
                value,
                safety: Vec::new(),
            })
        }
        Rvalue::TupleField(operand, index) => {
            let value = translate_operand(operand, locals)?;
            let SymbolicValue::Tuple(elements) = value else {
                return Err("tuple projection requires a symbolic tuple".into());
            };
            let value = elements
                .get(*index)
                .cloned()
                .ok_or_else(|| format!("symbolic tuple field {index} is out of range"))?;
            Ok(TranslatedRvalue {
                value,
                safety: Vec::new(),
            })
        }
        _ => Err("rvalue is outside the formal pure subset".to_string()),
    }
}

fn translate_operand(
    operand: &Operand,
    locals: &HashMap<u32, SymbolicValue>,
) -> Result<SymbolicValue, String> {
    match operand {
        Operand::Constant(Constant::Int(value)) => {
            Ok(SymbolicValue::scalar(smt_integer(*value), Ty::Int))
        }
        Operand::Constant(Constant::Bool(value)) => {
            Ok(SymbolicValue::scalar(value.to_string(), Ty::Bool))
        }
        Operand::Local(local)
        | Operand::Copy(local)
        | Operand::Move(local)
        | Operand::Borrow(local, crate::mir::Mutability::Immutable) => locals
            .get(&local.0)
            .cloned()
            .ok_or_else(|| format!("local {local} has no symbolic value")),
        Operand::CopyPlace(place)
        | Operand::MovePlace(place)
        | Operand::BorrowPlace(place, crate::mir::Mutability::Immutable) => {
            translate_place(place, locals)
        }
        _ => Err("operand is outside the formal pure subset".to_string()),
    }
}

fn translate_place(
    place: &Place,
    locals: &HashMap<u32, SymbolicValue>,
) -> Result<SymbolicValue, String> {
    let mut value = locals
        .get(&place.local.0)
        .cloned()
        .ok_or_else(|| format!("local {} has no symbolic value", place.local))?;
    for projection in &place.projection {
        value = match (projection, value) {
            (ProjectionElem::Field(field), SymbolicValue::Struct { fields, .. }) => fields
                .into_iter()
                .find_map(|(name, value)| (name == *field).then_some(value))
                .ok_or_else(|| format!("symbolic struct has no field `{field}`"))?,
            (ProjectionElem::TupleField(index), SymbolicValue::Tuple(elements)) => elements
                .get(*index)
                .cloned()
                .ok_or_else(|| format!("symbolic tuple field {index} is out of range"))?,
            (ProjectionElem::Index(_), _) => {
                return Err("symbolic aggregate indexing is not implemented yet".into());
            }
            (ProjectionElem::Deref, _) => {
                return Err("symbolic dereference requires heap-region modeling".into());
            }
            (projection, _) => {
                return Err(format!(
                    "symbolic projection `{projection:?}` does not match its aggregate"
                ));
            }
        };
    }
    Ok(value)
}

fn assign_symbolic_place(
    place: &Place,
    locals: &mut HashMap<u32, SymbolicValue>,
    replacement: SymbolicValue,
) -> Result<(), String> {
    let root = locals
        .get_mut(&place.local.0)
        .ok_or_else(|| format!("local {} has no symbolic value", place.local))?;
    assign_symbolic_projection(root, &place.projection, replacement)
}

fn assign_symbolic_projection(
    current: &mut SymbolicValue,
    projection: &[ProjectionElem],
    replacement: SymbolicValue,
) -> Result<(), String> {
    let Some((head, tail)) = projection.split_first() else {
        *current = replacement;
        return Ok(());
    };
    let child = match (head, current) {
        (ProjectionElem::Field(field), SymbolicValue::Struct { fields, .. }) => fields
            .iter_mut()
            .find_map(|(name, value)| (name == field).then_some(value))
            .ok_or_else(|| format!("symbolic struct has no field `{field}`"))?,
        (ProjectionElem::TupleField(index), SymbolicValue::Tuple(elements)) => elements
            .get_mut(*index)
            .ok_or_else(|| format!("symbolic tuple field {index} is out of range"))?,
        (ProjectionElem::Index(_), _) => {
            return Err("symbolic aggregate indexing is not implemented yet".into());
        }
        (ProjectionElem::Deref, _) => {
            return Err("symbolic dereference requires heap-region modeling".into());
        }
        (projection, _) => {
            return Err(format!(
                "symbolic projection `{projection:?}` does not match its aggregate"
            ));
        }
    };
    assign_symbolic_projection(child, tail, replacement)
}

#[derive(Default)]
struct SymbolicInvariants {
    conditions: Vec<String>,
    safety: Vec<String>,
}

fn symbolic_invariants(
    program: &Program,
    value: &SymbolicValue,
) -> Result<SymbolicInvariants, String> {
    let mut result = SymbolicInvariants::default();
    match value {
        SymbolicValue::Scalar { .. } => {}
        SymbolicValue::Tuple(elements) => {
            for element in elements {
                let nested = symbolic_invariants(program, element)?;
                result.conditions.extend(nested.conditions);
                result.safety.extend(nested.safety);
            }
        }
        SymbolicValue::Struct { type_name, fields } => {
            for (_, field) in fields {
                let nested = symbolic_invariants(program, field)?;
                result.conditions.extend(nested.conditions);
                result.safety.extend(nested.safety);
            }
            let mut names = fields.iter().cloned().collect::<HashMap<_, _>>();
            names.insert("self".to_string(), value.clone());
            for invariant in program
                .struct_invariants
                .get(type_name)
                .into_iter()
                .flatten()
                .filter_map(|invariant| invariant.condition.as_deref())
            {
                result
                    .safety
                    .extend(contract_arithmetic_safety(invariant, &names)?);
                result
                    .conditions
                    .push(translate_contract_expr(invariant, &names)?);
            }
        }
    }
    Ok(result)
}

fn symbolic_equality(left: &SymbolicValue, right: &SymbolicValue) -> Result<String, String> {
    match (left, right) {
        (
            SymbolicValue::Scalar {
                expression: left, ..
            },
            SymbolicValue::Scalar {
                expression: right, ..
            },
        ) => Ok(format!("(= {left} {right})")),
        (SymbolicValue::Tuple(left), SymbolicValue::Tuple(right)) if left.len() == right.len() => {
            left.iter()
                .zip(right)
                .map(|(left, right)| symbolic_equality(left, right))
                .collect::<Result<Vec<_>, _>>()
                .map(|equalities| conjunction(&equalities))
        }
        (
            SymbolicValue::Struct {
                type_name: left_name,
                fields: left,
            },
            SymbolicValue::Struct {
                type_name: right_name,
                fields: right,
            },
        ) if left_name == right_name && left.len() == right.len() => left
            .iter()
            .zip(right)
            .map(|((left_field, left), (right_field, right))| {
                if left_field != right_field {
                    Err("symbolic struct layouts do not match".into())
                } else {
                    symbolic_equality(left, right)
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|equalities| conjunction(&equalities)),
        _ => Err("symbolic equality operands have incompatible shapes".into()),
    }
}

fn operand_type(
    program: Option<&Program>,
    function: &Function,
    operand: &Operand,
) -> Result<Ty, String> {
    match operand {
        Operand::Constant(Constant::Int(_)) => Ok(Ty::Int),
        Operand::Constant(Constant::Bool(_)) => Ok(Ty::Bool),
        Operand::Local(local)
        | Operand::Copy(local)
        | Operand::Move(local)
        | Operand::Borrow(local, _) => function
            .locals
            .get(local.0 as usize)
            .map(|declaration| declaration.ty.clone())
            .ok_or_else(|| format!("operand refers to undeclared local {local}")),
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            place_type(program, function, place)
        }
        _ => Err("projected operand is outside the scalar formal subset".into()),
    }
}

fn place_type(program: Option<&Program>, function: &Function, place: &Place) -> Result<Ty, String> {
    let mut ty = function
        .locals
        .get(place.local.0 as usize)
        .map(|declaration| declaration.ty.clone())
        .ok_or_else(|| format!("place refers to undeclared local {}", place.local))?;
    for projection in &place.projection {
        ty = match (projection, ty) {
            (ProjectionElem::Field(field), Ty::Named(id, arguments)) if arguments.is_empty() => {
                program
                    .ok_or("named field projection requires program-aware verification")?
                    .struct_fields
                    .get(&id.name)
                    .and_then(|fields| {
                        fields
                            .iter()
                            .find_map(|(name, ty)| (name == field).then_some(ty.clone()))
                    })
                    .ok_or_else(|| format!("named type `{}` has no field `{field}`", id.name))?
            }
            (ProjectionElem::TupleField(index), Ty::Tuple(elements)) => elements
                .get(*index)
                .cloned()
                .ok_or_else(|| format!("tuple field {index} is out of range"))?,
            (ProjectionElem::Deref, Ty::Ref(inner, _) | Ty::Ptr(inner, _)) => *inner,
            (ProjectionElem::Index(_), _) => {
                return Err("indexed place types are outside the formal subset".into());
            }
            (projection, ty) => {
                return Err(format!(
                    "projection `{projection:?}` is invalid for formal type `{ty}`"
                ));
            }
        };
    }
    Ok(ty)
}

fn validate_rvalue_type(
    program: Option<&Program>,
    function: &Function,
    rvalue: &Rvalue,
    destination: &Ty,
) -> Result<(), String> {
    let actual = match rvalue {
        Rvalue::Use(
            operand @ (Operand::CopyPlace(_) | Operand::MovePlace(_) | Operand::BorrowPlace(_, _)),
        ) => {
            let _ = operand;
            destination.clone()
        }
        Rvalue::Use(operand) => operand_type(program, function, operand)?,
        Rvalue::Tuple(operands) => Ty::Tuple(
            operands
                .iter()
                .map(|operand| operand_type(program, function, operand))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Rvalue::Struct(type_name, fields) => {
            let Ty::Named(destination_name, _) = destination else {
                return Err("MIR struct construction has a non-struct destination".into());
            };
            if destination_name.name != *type_name {
                return Err(format!(
                    "MIR struct construction type mismatch: `{type_name}` versus `{}`",
                    destination_name.name
                ));
            }
            for (_, operand) in fields {
                let _ = operand_type(program, function, operand)?;
            }
            destination.clone()
        }
        Rvalue::Field(operand, _) => {
            let _ = operand_type(program, function, operand)?;
            destination.clone()
        }
        Rvalue::TupleField(operand, index) => {
            let Ty::Tuple(elements) = operand_type(program, function, operand)? else {
                return Err("MIR tuple projection operand is not a tuple".into());
            };
            elements
                .get(*index)
                .cloned()
                .ok_or_else(|| format!("MIR tuple field {index} is out of range"))?
        }
        Rvalue::UnaryOp(UnOp::Not, operand) => {
            if operand_type(program, function, operand)? != Ty::Bool {
                return Err("MIR logical negation operand is not Bool".into());
            }
            Ty::Bool
        }
        Rvalue::UnaryOp(UnOp::Neg, operand) => {
            let ty = operand_type(program, function, operand)?;
            smt_sort(&ty)?;
            ty
        }
        Rvalue::UnaryOp(UnOp::BitNot, _) => {
            return Err("bitwise not is outside the SMT subset".into());
        }
        Rvalue::BinaryOp(operator, left, right) => {
            let left = operand_type(program, function, left)?;
            let right = operand_type(program, function, right)?;
            if left != right {
                return Err(format!(
                    "MIR binary operand type mismatch: `{left}` versus `{right}`"
                ));
            }
            match operator {
                BinOp::Eq | BinOp::Ne => Ty::Bool,
                BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                    smt_sort(&left)?;
                    Ty::Bool
                }
                BinOp::And | BinOp::Or => {
                    if left != Ty::Bool {
                        return Err("MIR logical operator operands are not Bool".into());
                    }
                    Ty::Bool
                }
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                    smt_sort(&left)?;
                    left
                }
                _ => return Err("bitwise operation is outside the SMT subset".into()),
            }
        }
        _ => return Err("rvalue is outside the formal pure subset".into()),
    };
    if actual != *destination {
        return Err(format!(
            "MIR assignment type mismatch: destination is `{destination}`, value is `{actual}`"
        ));
    }
    Ok(())
}

fn truncating_integer_operation(operator: BinOp, left: &str, right: &str) -> String {
    let abs_left = format!("(ite (< {left} 0) (- {left}) {left})");
    let abs_right = format!("(ite (< {right} 0) (- {right}) {right})");
    let quotient_abs = format!("(div {abs_left} {abs_right})");
    let quotient =
        format!("(ite (distinct (< {left} 0) (< {right} 0)) (- {quotient_abs}) {quotient_abs})");
    match operator {
        BinOp::Div => quotient,
        BinOp::Rem => format!("(- {left} (* {quotient} {right}))"),
        _ => unreachable!("only division and remainder use truncating translation"),
    }
}

fn smt_integer(value: i64) -> String {
    if value < 0 {
        format!("(- {})", value.unsigned_abs())
    } else {
        value.to_string()
    }
}

fn smt_integer_i128(value: i128) -> String {
    if value < 0 {
        format!("(- {})", value.unsigned_abs())
    } else {
        value.to_string()
    }
}

fn translate_contract_expr(
    expression: &Expr,
    names: &HashMap<String, SymbolicValue>,
) -> Result<String, String> {
    Ok(translate_contract_value(expression, names)?
        .scalar_expression()?
        .to_string())
}

fn translate_contract_value(
    expression: &Expr,
    names: &HashMap<String, SymbolicValue>,
) -> Result<SymbolicValue, String> {
    match &expression.kind {
        ExprKind::Literal(literal) => match &literal.kind {
            LiteralKind::Int(value) => Ok(SymbolicValue::scalar(smt_integer_i128(*value), Ty::Int)),
            LiteralKind::Bool(value) => Ok(SymbolicValue::scalar(value.to_string(), Ty::Bool)),
            _ => Err("literal is outside the SMT subset".into()),
        },
        ExprKind::Ident(identifier) => names
            .get(&identifier.name)
            .cloned()
            .ok_or_else(|| format!("unknown contract symbol `{}`", identifier.name)),
        ExprKind::Paren(inner) => translate_contract_value(inner, names),
        ExprKind::Unary(operator, inner) => {
            let value = translate_contract_value(inner, names)?;
            let ty = match operator {
                UnaryOp::Neg => Ty::Int,
                UnaryOp::Not => Ty::Bool,
                _ => return Err("reference operation is outside the SMT subset".into()),
            };
            Ok(SymbolicValue::scalar(
                format!(
                    "({} {})",
                    match operator {
                        UnaryOp::Neg => "-",
                        UnaryOp::Not => "not",
                        _ => unreachable!(),
                    },
                    value.scalar_expression()?
                ),
                ty,
            ))
        }
        ExprKind::Binary(left, operator, right) => {
            let left = translate_contract_value(left, names)?;
            let right = translate_contract_value(right, names)?;
            let expression = match operator {
                AstBinOp::Eq => symbolic_equality(&left, &right)?,
                AstBinOp::Ne => format!("(not {})", symbolic_equality(&left, &right)?),
                _ => format!(
                    "({} {} {})",
                    ast_binop(*operator)?,
                    left.scalar_expression()?,
                    right.scalar_expression()?
                ),
            };
            let ty = if matches!(
                operator,
                AstBinOp::Eq
                    | AstBinOp::Ne
                    | AstBinOp::Lt
                    | AstBinOp::Le
                    | AstBinOp::Gt
                    | AstBinOp::Ge
                    | AstBinOp::And
                    | AstBinOp::Or
            ) {
                Ty::Bool
            } else {
                Ty::Int
            };
            Ok(SymbolicValue::scalar(expression, ty))
        }
        ExprKind::Field(base, field) => {
            let value = translate_contract_value(base, names)?;
            let SymbolicValue::Struct { fields, .. } = value else {
                return Err("contract field projection requires a struct".into());
            };
            fields
                .into_iter()
                .find_map(|(name, value)| (name == field.name).then_some(value))
                .ok_or_else(|| format!("contract struct has no field `{}`", field.name))
        }
        ExprKind::TupleField(base, index) => {
            let value = translate_contract_value(base, names)?;
            let SymbolicValue::Tuple(elements) = value else {
                return Err("contract tuple projection requires a tuple".into());
            };
            elements
                .get(*index)
                .cloned()
                .ok_or_else(|| format!("contract tuple field {index} is out of range"))
        }
        ExprKind::Tuple(elements) => elements
            .iter()
            .map(|element| translate_contract_value(element, names))
            .collect::<Result<Vec<_>, _>>()
            .map(SymbolicValue::Tuple),
        ExprKind::Struct(path, fields, base) if base.is_none() => {
            let type_name = path
                .segments
                .last()
                .map(|segment| segment.name.name.clone())
                .ok_or("contract struct literal has no type name")?;
            let fields = fields
                .iter()
                .map(|field| {
                    let value = match &field.value {
                        Some(value) => translate_contract_value(value, names),
                        None => names.get(&field.name.name).cloned().ok_or_else(|| {
                            format!("unknown contract symbol `{}`", field.name.name)
                        }),
                    }?;
                    Ok((field.name.name.clone(), value))
                })
                .collect::<Result<Vec<_>, String>>()?;
            Ok(SymbolicValue::Struct { type_name, fields })
        }
        ExprKind::Call(callee, args)
            if matches!(&callee.kind, ExprKind::Ident(name) if name.name == "old")
                && args.len() == 1 =>
        {
            translate_contract_value(&args[0].value, names)
        }
        _ => Err("contract expression is outside the SMT subset".to_string()),
    }
}

fn contract_arithmetic_safety(
    expression: &Expr,
    names: &HashMap<String, SymbolicValue>,
) -> Result<Vec<String>, String> {
    let mut safety = Vec::new();
    match &expression.kind {
        ExprKind::Paren(inner) => safety.extend(contract_arithmetic_safety(inner, names)?),
        ExprKind::Unary(UnaryOp::Neg, inner) => {
            safety.extend(contract_arithmetic_safety(inner, names)?);
            let operand = translate_contract_expr(inner, names)?;
            safety.push(format!("(distinct {operand} {})", smt_integer(i64::MIN)));
        }
        ExprKind::Unary(_, inner) => {
            safety.extend(contract_arithmetic_safety(inner, names)?);
        }
        ExprKind::Binary(left, operator, right) => {
            safety.extend(contract_arithmetic_safety(left, names)?);
            safety.extend(contract_arithmetic_safety(right, names)?);
            if matches!(operator, AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul) {
                let expression = translate_contract_expr(expression, names)?;
                safety.push(format!(
                    "(and (<= {} {expression}) (<= {expression} {}))",
                    smt_integer(i64::MIN),
                    smt_integer(i64::MAX)
                ));
            }
        }
        ExprKind::Call(callee, args)
            if matches!(&callee.kind, ExprKind::Ident(name) if name.name == "old")
                && args.len() == 1 =>
        {
            safety.extend(contract_arithmetic_safety(&args[0].value, names)?);
        }
        ExprKind::Literal(_) | ExprKind::Ident(_) => {}
        _ => {
            // Keep unsupported contract constructs on the same honest UNKNOWN
            // path as translation rather than silently assuming safety.
            translate_contract_expr(expression, names)?;
        }
    }
    Ok(safety)
}

fn mir_binop(operator: BinOp) -> Result<&'static str, String> {
    match operator {
        BinOp::Add => Ok("+"),
        BinOp::Sub => Ok("-"),
        BinOp::Mul => Ok("*"),
        BinOp::Div => Ok("div"),
        BinOp::Rem => Ok("mod"),
        BinOp::Eq => Ok("="),
        BinOp::Ne => Ok("distinct"),
        BinOp::Lt => Ok("<"),
        BinOp::Le => Ok("<="),
        BinOp::Gt => Ok(">"),
        BinOp::Ge => Ok(">="),
        BinOp::And => Ok("and"),
        BinOp::Or => Ok("or"),
        _ => Err("bitwise operation is outside the SMT subset".into()),
    }
}

fn ast_binop(operator: AstBinOp) -> Result<&'static str, String> {
    match operator {
        AstBinOp::Add => Ok("+"),
        AstBinOp::Sub => Ok("-"),
        AstBinOp::Mul => Ok("*"),
        AstBinOp::Div | AstBinOp::Mod => {
            Err("division in contracts awaits an explicit truncating operator".into())
        }
        AstBinOp::Eq => Ok("="),
        AstBinOp::Ne => Ok("distinct"),
        AstBinOp::Lt => Ok("<"),
        AstBinOp::Le => Ok("<="),
        AstBinOp::Gt => Ok(">"),
        AstBinOp::Ge => Ok(">="),
        AstBinOp::And => Ok("and"),
        AstBinOp::Or => Ok("or"),
        _ => Err("bitwise operation is outside the SMT subset".into()),
    }
}

fn conjunction(expressions: &[String]) -> String {
    match expressions {
        [] => "true".to_string(),
        [only] => only.clone(),
        many => format!("(and {})", many.join(" ")),
    }
}

fn disjunction(expressions: &[String]) -> String {
    match expressions {
        [] => "false".to_string(),
        [only] => only.clone(),
        many => format!("(or {})", many.join(" ")),
    }
}

fn smt_name(name: &str) -> String {
    let mut result = String::from("v_");
    for character in name.chars() {
        if character.is_ascii_alphanumeric() || character == '_' {
            result.push(character);
        } else {
            result.push('_');
        }
    }
    result
}

fn parse_model_values(output: &str, parameters: &[String]) -> Vec<(String, String)> {
    parameters
        .iter()
        .filter_map(|parameter| {
            let marker = format!("({parameter} ");
            let start = output.find(&marker)? + marker.len();
            let value = parse_smt_value(&output[start..])?;
            Some((parameter.trim_start_matches("v_").to_string(), value))
        })
        .collect()
}

fn parse_smt_value(input: &str) -> Option<String> {
    let input = input.trim_start();
    if input.starts_with('(') {
        let mut depth = 0usize;
        for (index, character) in input.char_indices() {
            match character {
                '(' => depth += 1,
                ')' => {
                    depth = depth.checked_sub(1)?;
                    if depth == 0 {
                        return Some(input[..=index].to_string());
                    }
                }
                _ => {}
            }
        }
        None
    } else {
        let end = input
            .find(|character: char| character.is_whitespace() || character == ')')
            .unwrap_or(input.len());
        (end > 0).then(|| input[..end].to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{BasicBlock, BlockId, Local, LocalDecl, Statement};

    #[test]
    fn translates_straight_line_integer_contract_to_smt() {
        let mut function = Function::new("increment".into(), vec![(Local(0), Ty::Int)], Ty::Int);
        function.param_names.push(("x".into(), Ty::Int));
        function.locals = vec![
            LocalDecl {
                ty: Ty::Int,
                name: Some("x".into()),
            },
            LocalDecl {
                ty: Ty::Int,
                name: Some("result".into()),
            },
        ];
        function.blocks.push(BasicBlock {
            id: BlockId(0),
            stmts: vec![Statement {
                kind: StatementKind::Assign(
                    Local(1),
                    Rvalue::BinaryOp(
                        BinOp::Add,
                        Operand::Copy(Local(0)),
                        Operand::Constant(Constant::Int(1)),
                    ),
                ),
            }],
            terminator: Some(Terminator::Return(Some(Operand::Copy(Local(1))))),
        });
        let condition = Expr::new(
            ExprKind::Binary(
                Box::new(Expr::new(
                    ExprKind::Ident(crate::parser::Ident::new("result", Default::default())),
                    Default::default(),
                )),
                AstBinOp::Gt,
                Box::new(Expr::new(
                    ExprKind::Ident(crate::parser::Ident::new("x", Default::default())),
                    Default::default(),
                )),
            ),
            Default::default(),
        );
        function.postconditions.push(crate::mir::MirContract {
            expr_string: "result > x".into(),
            message: None,
            pattern_name: None,
            condition: Some(Box::new(condition)),
        });

        let obligation = build_smt_obligation(&function).unwrap();
        assert!(
            obligation
                .script
                .contains("(<= (- 9223372036854775808) v_x)")
        );
        assert!(obligation.script.contains("(not (> (+ v_x 1) v_x))"));
        assert!(
            obligation
                .script
                .contains("(<= (+ v_x 1) 9223372036854775807)")
        );
        assert!(!obligation.script.contains("(get-value"));
        assert!(
            obligation
                .counterexample_script
                .contains("(get-value (v_x))")
        );
    }

    #[test]
    fn translates_acyclic_short_circuit_control_flow_path_sensitively() {
        let mut function = Function::new(
            "both".into(),
            vec![(Local(0), Ty::Bool), (Local(1), Ty::Bool)],
            Ty::Bool,
        );
        function.param_names = vec![("left".into(), Ty::Bool), ("right".into(), Ty::Bool)];
        function.locals = vec![
            LocalDecl {
                ty: Ty::Bool,
                name: Some("left".into()),
            },
            LocalDecl {
                ty: Ty::Bool,
                name: Some("right".into()),
            },
            LocalDecl {
                ty: Ty::Bool,
                name: None,
            },
        ];
        function.blocks = vec![
            BasicBlock {
                id: BlockId(0),
                stmts: vec![],
                terminator: Some(Terminator::If {
                    cond: Operand::Copy(Local(0)),
                    then_block: BlockId(1),
                    else_block: BlockId(2),
                }),
            },
            BasicBlock {
                id: BlockId(1),
                stmts: vec![Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Copy(Local(1)))),
                }],
                terminator: Some(Terminator::Goto(BlockId(3))),
            },
            BasicBlock {
                id: BlockId(2),
                stmts: vec![Statement {
                    kind: StatementKind::Assign(
                        Local(2),
                        Rvalue::Use(Operand::Constant(Constant::Bool(false))),
                    ),
                }],
                terminator: Some(Terminator::Goto(BlockId(3))),
            },
            BasicBlock {
                id: BlockId(3),
                stmts: vec![],
                terminator: Some(Terminator::Return(Some(Operand::Copy(Local(2))))),
            },
        ];
        let ident = |name: &str| {
            Expr::new(
                ExprKind::Ident(crate::parser::Ident::new(name, Default::default())),
                Default::default(),
            )
        };
        function.postconditions.push(crate::mir::MirContract {
            expr_string: "result == (left && right)".into(),
            message: None,
            pattern_name: None,
            condition: Some(Box::new(Expr::new(
                ExprKind::Binary(
                    Box::new(ident("result")),
                    AstBinOp::Eq,
                    Box::new(Expr::new(
                        ExprKind::Binary(
                            Box::new(ident("left")),
                            AstBinOp::And,
                            Box::new(ident("right")),
                        ),
                        Default::default(),
                    )),
                ),
                Default::default(),
            ))),
        });

        let obligation = build_smt_obligation(&function).unwrap();
        assert!(obligation.script.contains("(and v_left"));
        assert!(obligation.script.contains("(and (not v_left)"));
        assert!(
            obligation
                .script
                .contains("(= v_right (and v_left v_right))")
        );
        assert!(obligation.script.contains("(= false (and v_left v_right))"));
    }

    #[test]
    fn rejects_cyclic_control_flow_without_a_loop_invariant() {
        let mut function = Function::new("looping".into(), vec![], Ty::Bool);
        function.blocks.push(BasicBlock {
            id: BlockId(0),
            stmts: vec![],
            terminator: Some(Terminator::Goto(BlockId(0))),
        });
        function.postconditions.push(crate::mir::MirContract {
            expr_string: "result".into(),
            message: None,
            pattern_name: None,
            condition: Some(Box::new(Expr::new(
                ExprKind::Ident(crate::parser::Ident::new("result", Default::default())),
                Default::default(),
            ))),
        });
        assert!(
            build_smt_obligation(&function)
                .unwrap_err()
                .contains("does not yet support loops")
        );
    }

    #[cfg(unix)]
    #[test]
    fn solver_timeout_terminates_an_unresponsive_process() {
        use std::os::unix::fs::PermissionsExt;

        let directory = tempfile::tempdir().unwrap();
        let solver = directory.path().join("solver");
        std::fs::write(&solver, "#!/bin/sh\nsleep 10\n").unwrap();
        let mut permissions = std::fs::metadata(&solver).unwrap().permissions();
        permissions.set_mode(0o700);
        std::fs::set_permissions(&solver, permissions).unwrap();
        let obligation = SmtObligation {
            function: "blocked".into(),
            script: "(check-sat)\n".into(),
            counterexample_script: "(check-sat)\n".into(),
            assumptions_script: "(check-sat)\n".into(),
            parameters: vec![],
        };

        let result = run_solver_with_timeout(
            &obligation,
            solver.to_str().unwrap(),
            Duration::from_millis(25),
        );
        assert!(matches!(
            result,
            FormalResult::Unknown(reason) if reason.contains("terminated")
        ));
    }

    #[cfg(unix)]
    #[test]
    fn solver_model_is_reported_with_source_names_and_negative_values() {
        use std::os::unix::fs::PermissionsExt;

        let directory = tempfile::tempdir().unwrap();
        let solver = directory.path().join("solver");
        std::fs::write(
            &solver,
            "#!/bin/sh\ncat >/dev/null\nprintf 'sat\\n((v_count 3) (v_delta (- 2)))\\n'\n",
        )
        .unwrap();
        let mut permissions = std::fs::metadata(&solver).unwrap().permissions();
        permissions.set_mode(0o700);
        std::fs::set_permissions(&solver, permissions).unwrap();
        let obligation = SmtObligation {
            function: "counterexample".into(),
            script: "(check-sat)\n".into(),
            counterexample_script: "(check-sat)\n(get-value (v_count v_delta))\n".into(),
            assumptions_script: "(check-sat)\n".into(),
            parameters: vec!["v_count".into(), "v_delta".into()],
        };
        let result = run_solver_with_timeout(
            &obligation,
            solver.to_str().unwrap(),
            Duration::from_secs(1),
        );
        assert_eq!(
            result,
            FormalResult::Counterexample(vec![
                ("count".into(), "3".into()),
                ("delta".into(), "(- 2)".into()),
            ])
        );
    }

    #[cfg(unix)]
    #[test]
    fn unsat_decision_is_kept_when_a_following_model_query_exits_nonzero() {
        use std::os::unix::fs::PermissionsExt;

        let directory = tempfile::tempdir().unwrap();
        let solver = directory.path().join("solver");
        std::fs::write(&solver, "#!/bin/sh\nprintf 'unsat\\n'\nexit 1\n").unwrap();
        let mut permissions = std::fs::metadata(&solver).unwrap().permissions();
        permissions.set_mode(0o700);
        std::fs::set_permissions(&solver, permissions).unwrap();
        let obligation = SmtObligation {
            function: "proved".into(),
            script: "(check-sat)\n".into(),
            counterexample_script: "(check-sat)\n(get-value (x))\n".into(),
            assumptions_script: "(check-sat)\n".into(),
            parameters: vec!["x".into()],
        };
        assert_eq!(
            run_solver_with_timeout(
                &obligation,
                solver.to_str().unwrap(),
                Duration::from_secs(1)
            ),
            FormalResult::Proved
        );
    }
}
