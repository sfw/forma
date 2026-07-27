//! Formal verification support for the deliberately small 0.2 pure subset.
//!
//! The translator emits SMT-LIB for straight-line `Int`/`Bool` MIR. Solvers are
//! optional: the obligation remains useful tooling output, while lack of a
//! configured solver is reported as `UNKNOWN` by the CLI.

use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use crate::mir::{BinOp, Constant, Function, Operand, Rvalue, StatementKind, Terminator, UnOp};
use crate::parser::{BinOp as AstBinOp, Expr, ExprKind, LiteralKind, UnaryOp};
use crate::types::Ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmtObligation {
    pub function: String,
    pub script: String,
    pub parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormalResult {
    Proved,
    Counterexample(Vec<(String, String)>),
    Unknown(String),
}

pub fn build_smt_obligation(function: &Function) -> Result<SmtObligation, String> {
    if function.blocks.len() != 1 {
        return Err("formal subset currently requires straight-line control flow".to_string());
    }
    let block = &function.blocks[0];
    let mut locals = HashMap::new();
    let mut declarations = Vec::new();
    let mut parameters = Vec::new();
    for (index, (local, ty)) in function.params.iter().enumerate() {
        let name = function
            .param_names
            .get(index)
            .map(|(name, _)| smt_name(name))
            .unwrap_or_else(|| format!("arg{index}"));
        let sort = smt_sort(ty)?;
        declarations.push(format!("(declare-const {name} {sort})"));
        parameters.push(name.clone());
        locals.insert(local.0, name);
    }

    for statement in &block.stmts {
        match &statement.kind {
            StatementKind::Assign(local, rvalue) => {
                locals.insert(local.0, translate_rvalue(rvalue, &locals)?);
            }
            StatementKind::Drop(_) | StatementKind::Nop => {}
            _ => {
                return Err(
                    "formal subset does not support aggregate mutation or drop glue".into(),
                );
            }
        }
    }
    let Some(Terminator::Return(Some(returned))) = &block.terminator else {
        return Err("formal subset requires a value-returning straight-line function".into());
    };
    let result = translate_operand(returned, &locals)?;
    let mut names: HashMap<String, String> = function
        .param_names
        .iter()
        .map(|(name, _)| (name.clone(), smt_name(name)))
        .collect();
    names.insert("result".to_string(), result);

    let preconditions = function
        .preconditions
        .iter()
        .filter_map(|contract| contract.condition.as_deref())
        .map(|condition| translate_contract_expr(condition, &names))
        .collect::<Result<Vec<_>, _>>()?;
    let postconditions = function
        .postconditions
        .iter()
        .filter_map(|contract| contract.condition.as_deref())
        .map(|condition| translate_contract_expr(condition, &names))
        .collect::<Result<Vec<_>, _>>()?;
    if postconditions.is_empty() {
        return Err("formal proof requires at least one translatable postcondition".into());
    }

    let pre = conjunction(&preconditions);
    let post = conjunction(&postconditions);
    let mut lines = vec!["(set-logic ALL)".to_string()];
    lines.extend(declarations);
    lines.push(format!("(assert (and {pre} (not {post})))"));
    lines.push("(check-sat)".to_string());
    if !parameters.is_empty() {
        lines.push(format!("(get-value ({}))", parameters.join(" ")));
    }
    Ok(SmtObligation {
        function: function.name.clone(),
        script: format!("{}\n", lines.join("\n")),
        parameters,
    })
}

pub fn run_solver(obligation: &SmtObligation, solver: &str) -> FormalResult {
    run_solver_with_timeout(obligation, solver, Duration::from_secs(5))
}

pub fn run_solver_with_timeout(
    obligation: &SmtObligation,
    solver: &str,
    timeout: Duration,
) -> FormalResult {
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
        Err(error) => return FormalResult::Unknown(format!("cannot start SMT solver: {error}")),
    };
    if let Some(stdin) = child.stdin.as_mut()
        && let Err(error) = stdin.write_all(obligation.script.as_bytes())
    {
        return FormalResult::Unknown(format!("cannot write SMT obligation: {error}"));
    }
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
                return FormalResult::Unknown(format!(
                    "SMT solver exceeded {} ms and was terminated",
                    timeout.as_millis()
                ));
            }
            Err(error) => {
                terminate_solver(&mut child);
                let _ = child.wait();
                return FormalResult::Unknown(format!("cannot monitor SMT solver: {error}"));
            }
        }
    }
    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(error) => return FormalResult::Unknown(format!("SMT solver failed: {error}")),
    };
    if !output.status.success() {
        return FormalResult::Unknown(format!(
            "SMT solver error: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    match stdout.lines().next().map(str::trim) {
        Some("unsat") => FormalResult::Proved,
        Some("sat") => {
            FormalResult::Counterexample(parse_model_values(&stdout, &obligation.parameters))
        }
        Some("unknown") => FormalResult::Unknown("solver returned unknown".to_string()),
        other => FormalResult::Unknown(format!("unexpected SMT solver response: {other:?}")),
    }
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

fn smt_sort(ty: &Ty) -> Result<&'static str, String> {
    match ty {
        Ty::Int
        | Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::I64
        | Ty::I128
        | Ty::UInt
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::U64
        | Ty::U128
        | Ty::Isize
        | Ty::Usize => Ok("Int"),
        Ty::Bool => Ok("Bool"),
        _ => Err(format!("type `{ty}` is outside the formal pure subset")),
    }
}

fn translate_rvalue(rvalue: &Rvalue, locals: &HashMap<u32, String>) -> Result<String, String> {
    match rvalue {
        Rvalue::Use(operand) => translate_operand(operand, locals),
        Rvalue::BinaryOp(operator, left, right) => Ok(format!(
            "({} {} {})",
            mir_binop(*operator)?,
            translate_operand(left, locals)?,
            translate_operand(right, locals)?
        )),
        Rvalue::UnaryOp(operator, operand) => Ok(format!(
            "({} {})",
            match operator {
                UnOp::Neg => "-",
                UnOp::Not => "not",
                UnOp::BitNot => return Err("bitwise not is outside the SMT subset".into()),
            },
            translate_operand(operand, locals)?
        )),
        _ => Err("rvalue is outside the formal pure subset".to_string()),
    }
}

fn translate_operand(operand: &Operand, locals: &HashMap<u32, String>) -> Result<String, String> {
    match operand {
        Operand::Constant(Constant::Int(value)) => Ok(value.to_string()),
        Operand::Constant(Constant::Bool(value)) => Ok(value.to_string()),
        Operand::Local(local) | Operand::Copy(local) | Operand::Move(local) => locals
            .get(&local.0)
            .cloned()
            .ok_or_else(|| format!("local {local} has no symbolic value")),
        _ => Err("operand is outside the formal pure subset".to_string()),
    }
}

fn translate_contract_expr(
    expression: &Expr,
    names: &HashMap<String, String>,
) -> Result<String, String> {
    match &expression.kind {
        ExprKind::Literal(literal) => match &literal.kind {
            LiteralKind::Int(value) => Ok(value.to_string()),
            LiteralKind::Bool(value) => Ok(value.to_string()),
            _ => Err("literal is outside the SMT subset".into()),
        },
        ExprKind::Ident(identifier) => names
            .get(&identifier.name)
            .cloned()
            .ok_or_else(|| format!("unknown contract symbol `{}`", identifier.name)),
        ExprKind::Paren(inner) => translate_contract_expr(inner, names),
        ExprKind::Unary(operator, inner) => Ok(format!(
            "({} {})",
            match operator {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "not",
                _ => return Err("reference operation is outside the SMT subset".into()),
            },
            translate_contract_expr(inner, names)?
        )),
        ExprKind::Binary(left, operator, right) => Ok(format!(
            "({} {} {})",
            ast_binop(*operator)?,
            translate_contract_expr(left, names)?,
            translate_contract_expr(right, names)?
        )),
        ExprKind::Call(callee, args)
            if matches!(&callee.kind, ExprKind::Ident(name) if name.name == "old")
                && args.len() == 1 =>
        {
            translate_contract_expr(&args[0].value, names)
        }
        _ => Err("contract expression is outside the SMT subset".to_string()),
    }
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
        AstBinOp::Div => Ok("div"),
        AstBinOp::Mod => Ok("mod"),
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
                .contains("(assert (and true (not (> (+ v_x 1) v_x))))")
        );
        assert!(obligation.script.contains("(get-value (v_x))"));
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
            "#!/bin/sh\nprintf 'sat\\n((v_count 3) (v_delta (- 2)))\\n'\n",
        )
        .unwrap();
        let mut permissions = std::fs::metadata(&solver).unwrap().permissions();
        permissions.set_mode(0o700);
        std::fs::set_permissions(&solver, permissions).unwrap();
        let obligation = SmtObligation {
            function: "counterexample".into(),
            script: "(check-sat)\n".into(),
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
}
