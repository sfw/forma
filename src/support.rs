//! Published language profiles and backend support queries.

use std::collections::HashMap;

use crate::builtins::{self, Support};
use crate::mir::{Program, Terminator};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FeatureClass {
    Core,
    Hosted,
    Native,
    Experimental,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FeatureSpec {
    pub name: &'static str,
    pub class: FeatureClass,
    pub description: &'static str,
}

pub const FEATURES: &[FeatureSpec] = &[
    FeatureSpec {
        name: "affine-ownership",
        class: FeatureClass::Core,
        description: "Moves, copies, loans, partial moves, and deterministic lexical drops.",
    },
    FeatureSpec {
        name: "core-values",
        class: FeatureClass::Core,
        description: "Scalars, tuples, fixed arrays, structs, and uniform scalar-payload enums.",
    },
    FeatureSpec {
        name: "core-control-flow",
        class: FeatureClass::Core,
        description: "Calls, conditionals, finite matches, and loops over Core values.",
    },
    FeatureSpec {
        name: "hosted-runtime",
        class: FeatureClass::Hosted,
        description: "Dynamic collections, text, files, databases, networking, and process APIs.",
    },
    FeatureSpec {
        name: "structured-concurrency",
        class: FeatureClass::Hosted,
        description: "Affine tasks, cancellation, deadlines, channels, and mutexes.",
    },
    FeatureSpec {
        name: "native-runtime",
        class: FeatureClass::Native,
        description: "Runtime-backed native string, math, memory, and selected collection calls.",
    },
    FeatureSpec {
        name: "llvm-backend",
        class: FeatureClass::Experimental,
        description: "Native backend until all Core differential gates are complete.",
    },
    FeatureSpec {
        name: "formal-verification",
        class: FeatureClass::Experimental,
        description: "SMT proof for the documented pure finite/straight-line subset.",
    },
    FeatureSpec {
        name: "custom-destructors",
        class: FeatureClass::Experimental,
        description: "User-defined Drop bodies; compiler drop glue itself is Core.",
    },
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSupport {
    pub interpreter: Support,
    pub native: Support,
    pub verification: Support,
    pub reasons: Vec<String>,
}

pub type BackendSupportReport = HashMap<String, FunctionSupport>;

pub fn analyze(program: &Program) -> BackendSupportReport {
    let effects = builtins::infer_effects(program);
    let mut report = HashMap::new();
    let mut callees: HashMap<String, Vec<String>> = HashMap::new();

    for (name, function) in &program.functions {
        let mut native = Support::Supported;
        let mut verification = Support::Experimental;
        let mut reasons = Vec::new();
        if effects.get(name).is_some_and(|effects| !effects.is_empty()) {
            verification = Support::Unsupported;
            reasons.push("effectful functions are outside the formal subset".into());
        }
        for block in &function.blocks {
            match &block.terminator {
                Some(Terminator::Call { func, .. }) => {
                    if let Some(spec) = builtins::get(func) {
                        native = merge_support(native, spec.native);
                        verification = merge_support(verification, spec.verification);
                        if spec.native == Support::Unsupported {
                            reasons.push(format!("builtin `{func}` has no native implementation"));
                        }
                    } else if program.functions.contains_key(func) {
                        callees.entry(name.clone()).or_default().push(func.clone());
                    }
                }
                Some(Terminator::CallIndirect { .. }) => {
                    native = merge_support(native, Support::Experimental);
                    verification = Support::Unsupported;
                    reasons.push("indirect calls are outside the formal subset".into());
                }
                Some(Terminator::Spawn { .. }) | Some(Terminator::Await { .. }) => {
                    native = Support::Unsupported;
                    verification = Support::Unsupported;
                    reasons.push("task operations require the Hosted interpreter runtime".into());
                }
                _ => {}
            }
        }
        reasons.sort();
        reasons.dedup();
        report.insert(
            name.clone(),
            FunctionSupport {
                interpreter: Support::Supported,
                native,
                verification,
                reasons,
            },
        );
    }

    loop {
        let previous = report.clone();
        let mut changed = false;
        for (caller, targets) in &callees {
            let Some(current) = report.get_mut(caller) else {
                continue;
            };
            for target in targets {
                let Some(target_support) = previous.get(target) else {
                    continue;
                };
                let native = merge_support(current.native, target_support.native);
                let verification = merge_support(current.verification, target_support.verification);
                changed |= native != current.native || verification != current.verification;
                current.native = native;
                current.verification = verification;
            }
        }
        if !changed {
            break;
        }
    }
    report
}

fn merge_support(left: Support, right: Support) -> Support {
    match (left, right) {
        (Support::Unsupported, _) | (_, Support::Unsupported) => Support::Unsupported,
        (Support::Experimental, _) | (_, Support::Experimental) => Support::Experimental,
        _ => Support::Supported,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{BasicBlock, BlockId, Function};
    use crate::types::Ty;

    #[test]
    fn backend_limits_propagate_through_calls() {
        let mut hosted = Function::new("hosted".into(), vec![], Ty::Unit);
        hosted.blocks.push(BasicBlock {
            id: BlockId(0),
            stmts: vec![],
            terminator: Some(Terminator::Call {
                func: "file_read".into(),
                args: vec![],
                arg_pass_modes: vec![],
                dest: None,
                next: BlockId(1),
            }),
        });
        hosted.blocks.push(BasicBlock {
            id: BlockId(1),
            stmts: vec![],
            terminator: Some(Terminator::Return(None)),
        });
        let mut caller = Function::new("caller".into(), vec![], Ty::Unit);
        caller.blocks.push(BasicBlock {
            id: BlockId(0),
            stmts: vec![],
            terminator: Some(Terminator::Call {
                func: "hosted".into(),
                args: vec![],
                arg_pass_modes: vec![],
                dest: None,
                next: BlockId(1),
            }),
        });
        caller.blocks.push(BasicBlock {
            id: BlockId(1),
            stmts: vec![],
            terminator: Some(Terminator::Return(None)),
        });
        let mut program = Program::new();
        program.functions.insert("hosted".into(), hosted);
        program.functions.insert("caller".into(), caller);
        let support = analyze(&program);
        assert_eq!(support["hosted"].native, Support::Unsupported);
        assert_eq!(support["caller"].native, Support::Unsupported);
        assert_eq!(support["caller"].verification, Support::Unsupported);
    }
}
