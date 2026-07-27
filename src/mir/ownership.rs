//! Ownership validation over typed MIR.
//!
//! FORMA 0.2 makes MIR operands explicit about whether reading a local copies or
//! moves it. This module is the analysis boundary for that work. The current
//! lowerer still emits legacy [`Operand::Local`] reads, so the report records
//! those reads separately while strictly validating explicit `Copy` and `Move`
//! operands.

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;

use crate::types::Ty;

use super::mir::{
    BlockId, Function, Local, Mutability, Operand, PassMode, Place, Program, Rvalue, StatementKind,
    Terminator,
};

/// A source-language ownership violation found in MIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnershipError {
    pub function: String,
    pub block: BlockId,
    pub local: Local,
    pub kind: OwnershipErrorKind,
}

/// Kinds of ownership violation that can be represented by current MIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnershipErrorKind {
    /// An explicit copy was requested for a type that is not `Copy`.
    CopyOfNonCopy { ty: Ty },
    /// A local was read after an explicit move in the same basic block.
    UseAfterMove,
    /// MIR refers to a local that has no declaration.
    UnknownLocal,
    /// A new loan conflicts with an active loan of the same place.
    ConflictingLoan {
        requested: Mutability,
        existing: Mutability,
    },
    /// Ownership was transferred while the place was borrowed.
    MoveWhileBorrowed,
    /// A place was mutated while a loan prevented mutation.
    MutationWhileBorrowed,
    /// A local can be reached before it has been initialized.
    UseOfUninitialized,
    /// A value is destroyed more than once along a control-flow path.
    DoubleDrop,
    /// An aggregate or projection overlaps a previously moved subplace.
    UseOfPartiallyMoved { place: String },
    /// An aggregate cannot be dropped while one of its fields is absent.
    DropOfPartiallyMoved { place: String },
    /// References are second-class and cannot cross a task boundary.
    ReferenceAcrossTask,
    /// A captured value does not implement compiler-known `Send`.
    NonSendAcrossTask { ty: Ty },
}

impl fmt::Display for OwnershipError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}: ownership error for {}: ",
            self.function, self.block, self.local
        )?;
        match &self.kind {
            OwnershipErrorKind::CopyOfNonCopy { ty } => {
                write!(f, "cannot copy value of non-Copy type `{}`", ty)
            }
            OwnershipErrorKind::UseAfterMove => write!(f, "use of moved value"),
            OwnershipErrorKind::UnknownLocal => write!(f, "local has no MIR declaration"),
            OwnershipErrorKind::ConflictingLoan {
                requested,
                existing,
            } => write!(
                f,
                "cannot create {requested:?} loan while {existing:?} loan is active"
            ),
            OwnershipErrorKind::MoveWhileBorrowed => {
                write!(f, "cannot move value while it is borrowed")
            }
            OwnershipErrorKind::MutationWhileBorrowed => {
                write!(f, "cannot mutate value while it is borrowed")
            }
            OwnershipErrorKind::UseOfUninitialized => write!(f, "use of uninitialized value"),
            OwnershipErrorKind::DoubleDrop => write!(f, "value is dropped more than once"),
            OwnershipErrorKind::UseOfPartiallyMoved { place } => {
                write!(f, "use of partially moved place `{place}`")
            }
            OwnershipErrorKind::DropOfPartiallyMoved { place } => {
                write!(f, "cannot drop partially moved place `{place}`")
            }
            OwnershipErrorKind::ReferenceAcrossTask => {
                write!(f, "references cannot be captured or sent to a spawned task")
            }
            OwnershipErrorKind::NonSendAcrossTask { ty } => {
                write!(f, "value of type `{ty}` does not implement `Send`")
            }
        }
    }
}

impl std::error::Error for OwnershipError {}

/// Result of auditing a MIR program.
#[derive(Debug, Clone, Default)]
pub struct OwnershipReport {
    /// Strict violations involving explicit `Copy` and `Move` operands.
    pub errors: Vec<OwnershipError>,
    /// Reads that still use the pre-0.2 ambiguous `Operand::Local` form.
    pub legacy_local_reads: usize,
}

impl OwnershipReport {
    pub fn is_ready_for_enforcement(&self) -> bool {
        self.errors.is_empty() && self.legacy_local_reads == 0
    }
}

/// Audit ownership operations in a program.
///
/// Move state is initially block-local. Cross-block dataflow, loan regions, place
/// projections, and drop elaboration are added in the next ownership milestone.
pub fn analyze(program: &Program) -> OwnershipReport {
    let mut report = OwnershipReport::default();
    for function in program.functions.values() {
        analyze_function(function, &program.send_types, &mut report);
    }
    report.errors.retain(|error| {
        !matches!(&error.kind, OwnershipErrorKind::CopyOfNonCopy { ty } if program.is_copy_type(ty))
    });
    report
}

/// Replace every legacy local operand with an explicit copy, move, or borrow.
///
/// This compatibility normalization lets the existing AST lowerer migrate one
/// expression at a time while guaranteeing that every consumer after lowering
/// sees ownership-explicit MIR.
pub fn make_operands_explicit(program: &mut Program) {
    let copy_types = program.copy_types.clone();
    for function in program.functions.values_mut() {
        let local_types: Vec<Ty> = function.locals.iter().map(|decl| decl.ty.clone()).collect();
        let borrowed_params: LocalSet = function
            .params
            .iter()
            .zip(function.param_pass_modes.iter())
            .filter_map(|((local, _), mode)| (*mode != PassMode::Owned).then_some(*local))
            .collect();
        for block in &mut function.blocks {
            for statement in &mut block.stmts {
                match &mut statement.kind {
                    StatementKind::Assign(_, rvalue) | StatementKind::AssignPlace(_, rvalue) => {
                        explicit_rvalue(rvalue, &local_types, &copy_types, &borrowed_params)
                    }
                    StatementKind::IndexAssign(_, index, value) => {
                        explicit_operand(
                            index,
                            &local_types,
                            PassMode::Owned,
                            &copy_types,
                            &borrowed_params,
                        );
                        explicit_operand(
                            value,
                            &local_types,
                            PassMode::Owned,
                            &copy_types,
                            &borrowed_params,
                        );
                    }
                    StatementKind::Drop(_) | StatementKind::DropPlace(_) => {}
                    StatementKind::Nop => {}
                }
            }
            if let Some(terminator) = &mut block.terminator {
                explicit_terminator(terminator, &local_types, &copy_types, &borrowed_params);
            }
        }
    }
}

/// Insert deterministic reverse-declaration-order drops on every control-flow
/// edge where an owned value leaves its live region.
///
/// Return edges are cleaned up in place. Other edges are redirected through a
/// cleanup block when their target no longer needs one or more initialized
/// locals. This covers lexical exits lowered as `goto` (including `break` and
/// `continue`), call/await/spawn continuations, and the early-return edge of
/// `?`. Moved values and trivially `Copy` locals are excluded.
pub fn elaborate_drops(program: &mut Program) {
    let struct_fields = program.struct_fields.clone();
    let copy_types = program.copy_types.clone();
    for function in program.functions.values_mut() {
        elaborate_overwrite_drops(function, &copy_types);
        let original_block_count = function.blocks.len();
        let (live_in, _, _) = compute_liveness(function);
        let terminator_states = initialized_before_terminators(function);
        let moved_states = moved_places_before_terminators(function);
        let local_types: Vec<Ty> = function
            .locals
            .iter()
            .map(|local| local.ty.clone())
            .collect();
        let cleanup_context = CleanupContext {
            live_in: &live_in,
            local_types: &local_types,
            copy_types: &copy_types,
            struct_fields: &struct_fields,
        };

        for block_index in 0..original_block_count {
            let Some(mut terminator) = function.blocks[block_index].terminator.clone() else {
                continue;
            };
            if matches!(terminator, Terminator::Return(_) | Terminator::Unreachable) {
                continue;
            }
            let mut initialized = terminator_states
                .get(block_index)
                .and_then(Clone::clone)
                .unwrap_or_default();
            let mut moved = moved_states
                .get(block_index)
                .and_then(Clone::clone)
                .unwrap_or_default();

            transfer_initialization_terminator(&terminator, &mut initialized);
            record_moved_terminator(&terminator, &mut moved);

            // A value is eligible for cleanup at its last-live block, not in
            // every later block it merely flows through as initialized state.
            let mut edge_initialized = live_in.get(block_index).cloned().unwrap_or_default();
            for statement in &function.blocks[block_index].stmts {
                if let (_, Some(definition)) = statement_uses_and_def(statement) {
                    edge_initialized.insert(definition);
                }
            }
            if block_index == function.entry_block.0 as usize {
                edge_initialized.extend(function.params.iter().map(|(local, _)| *local));
            }
            match &terminator {
                Terminator::Call { dest, .. }
                | Terminator::CallIndirect { dest, .. }
                | Terminator::Spawn { dest, .. }
                | Terminator::Await { dest, .. } => {
                    edge_initialized.extend(dest.iter().copied());
                }
                _ => {}
            }
            edge_initialized.retain(|local| initialized.contains(local));

            redirect_successors_through_cleanup(
                function,
                &mut terminator,
                &edge_initialized,
                &moved,
                &cleanup_context,
            );
            function.blocks[block_index].terminator = Some(terminator);
        }

        // Recompute availability after inserting edge cleanup blocks so a
        // return never destroys a value already cleaned on its incoming edge.
        let terminator_states = initialized_before_terminators(function);
        let moved_states = moved_places_before_terminators(function);
        for block_index in 0..function.blocks.len() {
            let Some(Terminator::Return(return_value)) =
                function.blocks[block_index].terminator.clone()
            else {
                continue;
            };
            let mut initialized = terminator_states
                .get(block_index)
                .and_then(Clone::clone)
                .unwrap_or_default();
            let mut moved = moved_states
                .get(block_index)
                .and_then(Clone::clone)
                .unwrap_or_default();
            if let Some(operand) = &return_value {
                consume_initialization_operand(operand, &mut initialized);
                record_moved_operand(operand, &mut moved);
            }
            append_dead_drops(
                &mut function.blocks[block_index],
                &initialized,
                &LocalSet::new(),
                &moved,
                &local_types,
                &copy_types,
                &struct_fields,
            );
        }
    }
}

/// Make destruction on assignment explicit. MIR evaluates an assignment's
/// right-hand side before replacing the destination, so a self-referential
/// update is first materialized in a temporary, then the old owner is dropped.
fn elaborate_overwrite_drops(function: &mut Function, copy_types: &HashSet<String>) {
    let entries = initialized_at_block_entries(function);
    for block_index in 0..function.blocks.len() {
        let mut initialized = entries
            .get(block_index)
            .and_then(Clone::clone)
            .unwrap_or_default();
        let statements = std::mem::take(&mut function.blocks[block_index].stmts);
        let mut elaborated = Vec::with_capacity(statements.len());

        for statement in statements {
            let StatementKind::Assign(destination, rvalue) = &statement.kind else {
                transfer_initialization_statement(&statement, &mut initialized);
                elaborated.push(statement);
                continue;
            };

            let destination = *destination;
            let ty = function
                .locals
                .get(destination.0 as usize)
                .map(|decl| decl.ty.clone())
                .unwrap_or(Ty::Error);
            let overwrites_owner = initialized.contains(&destination)
                && !is_copy_type(&ty, copy_types)
                && !rvalue_moves_local(rvalue, destination);

            if overwrites_owner && rvalue_reads_local(rvalue, destination) {
                let temporary = function.add_local(ty.clone(), None);
                elaborated.push(super::mir::Statement {
                    kind: StatementKind::Assign(temporary, rvalue.clone()),
                });
                elaborated.push(super::mir::Statement {
                    kind: StatementKind::Drop(destination),
                });
                elaborated.push(super::mir::Statement {
                    kind: StatementKind::Assign(destination, Rvalue::Use(Operand::Move(temporary))),
                });
            } else {
                if overwrites_owner {
                    elaborated.push(super::mir::Statement {
                        kind: StatementKind::Drop(destination),
                    });
                    initialized.remove(&destination);
                }
                transfer_initialization_statement(&statement, &mut initialized);
                elaborated.push(statement);
                continue;
            }

            initialized.insert(destination);
        }
        function.blocks[block_index].stmts = elaborated;
    }
}

fn initialized_at_block_entries(function: &Function) -> Vec<Option<LocalSet>> {
    if function.blocks.is_empty() {
        return Vec::new();
    }
    let mut entries: Vec<Option<LocalSet>> = vec![None; function.blocks.len()];
    let entry_index = function.entry_block.0 as usize;
    entries[entry_index] = Some(function.params.iter().map(|(local, _)| *local).collect());
    let mut worklist = VecDeque::from([function.entry_block]);
    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let mut state = entries[index].clone().unwrap_or_default();
        for statement in &function.blocks[index].stmts {
            transfer_initialization_statement(statement, &mut state);
        }
        if let Some(terminator) = &function.blocks[index].terminator {
            transfer_initialization_terminator(terminator, &mut state);
        }
        for successor in block_successors(function.blocks[index].terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(entry) = entries.get_mut(successor_index) else {
                continue;
            };
            let changed = match entry {
                Some(existing) => {
                    let intersection = existing.intersection(&state).copied().collect();
                    if *existing == intersection {
                        false
                    } else {
                        *existing = intersection;
                        true
                    }
                }
                None => {
                    *entry = Some(state.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }
    entries
}

fn rvalue_reads_local(rvalue: &Rvalue, local: Local) -> bool {
    let reads = |operand: &Operand| match operand {
        Operand::Local(source)
        | Operand::Copy(source)
        | Operand::Move(source)
        | Operand::Borrow(source, _) => *source == local,
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            place.local == local
        }
        Operand::Constant(_) => false,
    };
    match rvalue {
        Rvalue::Use(op)
        | Rvalue::UnaryOp(_, op)
        | Rvalue::Deref(op)
        | Rvalue::Cast(op, _)
        | Rvalue::Field(op, _)
        | Rvalue::TupleField(op, _) => reads(op),
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            reads(left) || reads(right)
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => ops.iter().any(reads),
        Rvalue::Struct(_, fields) => fields.iter().any(|(_, operand)| reads(operand)),
        Rvalue::Enum { fields, .. } => fields.iter().any(reads),
        Rvalue::Closure { captures, .. } => captures.iter().any(reads),
        Rvalue::RefPlace(place, _) => place.local == local,
        Rvalue::Ref(source, _) | Rvalue::Discriminant(source) | Rvalue::EnumField(source, _) => {
            *source == local
        }
    }
}

fn rvalue_moves_local(rvalue: &Rvalue, local: Local) -> bool {
    let moves = |operand: &Operand| match operand {
        Operand::Move(source) => *source == local,
        Operand::MovePlace(place) => place.local == local && place.projection.is_empty(),
        _ => false,
    };
    match rvalue {
        Rvalue::Use(op)
        | Rvalue::UnaryOp(_, op)
        | Rvalue::Deref(op)
        | Rvalue::Cast(op, _)
        | Rvalue::Field(op, _)
        | Rvalue::TupleField(op, _) => moves(op),
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            moves(left) || moves(right)
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => ops.iter().any(moves),
        Rvalue::Struct(_, fields) => fields.iter().any(|(_, operand)| moves(operand)),
        Rvalue::Enum { fields, .. } => fields.iter().any(moves),
        Rvalue::Closure { captures, .. } => captures.iter().any(moves),
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => false,
    }
}

struct CleanupContext<'a> {
    live_in: &'a [LocalSet],
    local_types: &'a [Ty],
    copy_types: &'a HashSet<String>,
    struct_fields: &'a HashMap<String, Vec<(String, Ty)>>,
}

fn append_dead_drops(
    block: &mut super::mir::BasicBlock,
    initialized: &LocalSet,
    live_at_target: &LocalSet,
    moved: &HashSet<Place>,
    local_types: &[Ty],
    copy_types: &HashSet<String>,
    struct_fields: &HashMap<String, Vec<(String, Ty)>>,
) {
    for index in (0..local_types.len()).rev() {
        let local = Local(index as u32);
        if !initialized.contains(&local)
            || live_at_target.contains(&local)
            || is_copy_type(&local_types[index], copy_types)
        {
            continue;
        }
        let root = Place::new(local);
        let partial = moved
            .iter()
            .any(|place| place.local == local && !place.projection.is_empty());
        if partial {
            elaborate_partial_drops(
                block,
                &root,
                &local_types[index],
                moved,
                copy_types,
                struct_fields,
            );
        } else if !moved.contains(&root) {
            block.stmts.push(super::mir::Statement {
                kind: StatementKind::Drop(local),
            });
        }
    }
}

fn cleanup_target(
    function: &mut Function,
    target: BlockId,
    initialized: &LocalSet,
    moved: &HashSet<Place>,
    context: &CleanupContext<'_>,
) -> BlockId {
    let target_live = context
        .live_in
        .get(target.0 as usize)
        .cloned()
        .unwrap_or_default();
    let id = BlockId(function.blocks.len() as u32);
    let mut cleanup = super::mir::BasicBlock::new(id);
    append_dead_drops(
        &mut cleanup,
        initialized,
        &target_live,
        moved,
        context.local_types,
        context.copy_types,
        context.struct_fields,
    );
    if cleanup.stmts.is_empty() {
        return target;
    }
    cleanup.terminator = Some(Terminator::Goto(target));
    function.blocks.push(cleanup);
    id
}

fn redirect_successors_through_cleanup(
    function: &mut Function,
    terminator: &mut Terminator,
    initialized: &LocalSet,
    moved: &HashSet<Place>,
    context: &CleanupContext<'_>,
) {
    let mut redirect = |target: &mut BlockId| {
        *target = cleanup_target(function, *target, initialized, moved, context);
    };
    match terminator {
        Terminator::Goto(target) => redirect(target),
        Terminator::If {
            then_block,
            else_block,
            ..
        } => {
            redirect(then_block);
            redirect(else_block);
        }
        Terminator::Switch {
            targets, default, ..
        } => {
            for (_, target) in targets {
                redirect(target);
            }
            redirect(default);
        }
        Terminator::Call { next, .. }
        | Terminator::CallIndirect { next, .. }
        | Terminator::Spawn { next, .. }
        | Terminator::Await { next, .. } => redirect(next),
        Terminator::Return(_) | Terminator::Unreachable => {}
    }
}

fn elaborate_partial_drops(
    block: &mut super::mir::BasicBlock,
    root: &Place,
    ty: &Ty,
    moved: &HashSet<Place>,
    copy_types: &HashSet<String>,
    struct_fields: &HashMap<String, Vec<(String, Ty)>>,
) {
    let fields: Vec<(Place, Ty)> = match ty {
        Ty::Named(type_id, _) => struct_fields
            .get(&type_id.name)
            .map(|fields| {
                fields
                    .iter()
                    .map(|(name, ty)| (root.clone().field(name), ty.clone()))
                    .collect()
            })
            .unwrap_or_default(),
        Ty::Tuple(types) => types
            .iter()
            .enumerate()
            .map(|(index, ty)| (root.clone().tuple_field(index), ty.clone()))
            .collect(),
        _ => Vec::new(),
    };
    for (place, field_ty) in fields.into_iter().rev() {
        if !is_copy_type(&field_ty, copy_types)
            && !moved.iter().any(|moved_place| moved_place.overlaps(&place))
        {
            block.stmts.push(super::mir::Statement {
                kind: StatementKind::DropPlace(place),
            });
        }
    }
}

fn moved_places_before_terminators(function: &Function) -> Vec<Option<HashSet<Place>>> {
    if function.blocks.is_empty() {
        return Vec::new();
    }
    let mut entries: Vec<Option<HashSet<Place>>> = vec![None; function.blocks.len()];
    let mut before: Vec<Option<HashSet<Place>>> = vec![None; function.blocks.len()];
    let entry = function.entry_block.0 as usize;
    if entry >= function.blocks.len() {
        return before;
    }
    entries[entry] = Some(HashSet::new());
    let mut worklist = VecDeque::from([function.entry_block]);
    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut moved = entries[index].clone().unwrap_or_default();
        for statement in &block.stmts {
            match &statement.kind {
                StatementKind::Assign(destination, rvalue) => {
                    record_moved_rvalue(rvalue, &mut moved);
                    moved.retain(|place| place.local != *destination);
                }
                StatementKind::AssignPlace(destination, rvalue) => {
                    record_moved_rvalue(rvalue, &mut moved);
                    moved.retain(|place| !place.overlaps(destination));
                }
                StatementKind::IndexAssign(_, index, value) => {
                    record_moved_operand(index, &mut moved);
                    record_moved_operand(value, &mut moved);
                }
                StatementKind::Drop(local) => {
                    moved.insert(Place::new(*local));
                }
                StatementKind::DropPlace(place) => {
                    moved.insert(place.clone());
                }
                StatementKind::Nop => {}
            }
        }
        let changed = before[index].as_ref() != Some(&moved);
        before[index] = Some(moved.clone());
        if !changed {
            continue;
        }
        let mut outgoing = moved;
        if let Some(terminator) = &block.terminator {
            record_moved_terminator(terminator, &mut outgoing);
        }
        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(successor_entry) = entries.get_mut(successor_index) else {
                continue;
            };
            let changed = match successor_entry {
                Some(existing) => {
                    let old_len = existing.len();
                    existing.extend(outgoing.iter().cloned());
                    existing.len() != old_len
                }
                None => {
                    *successor_entry = Some(outgoing.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }
    before
}

fn record_moved_rvalue(rvalue: &Rvalue, moved: &mut HashSet<Place>) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            record_moved_operand(op, moved)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            record_moved_operand(left, moved);
            record_moved_operand(right, moved);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                record_moved_operand(op, moved);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                record_moved_operand(op, moved);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                record_moved_operand(op, moved);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => record_moved_operand(op, moved),
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                record_moved_operand(capture, moved);
            }
        }
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => {}
    }
}

fn record_moved_terminator(terminator: &Terminator, moved: &mut HashSet<Place>) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                record_moved_operand(op, moved);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            record_moved_operand(cond, moved)
        }
        Terminator::Call { args, dest, .. } => {
            for arg in args {
                record_moved_operand(arg, moved);
            }
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::CallIndirect {
            callee, args, dest, ..
        } => {
            record_moved_operand(callee, moved);
            for arg in args {
                record_moved_operand(arg, moved);
            }
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Spawn { expr, dest, .. } => {
            record_moved_operand(expr, moved);
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Await { task, dest, .. } => {
            record_moved_operand(task, moved);
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn record_moved_operand(operand: &Operand, moved: &mut HashSet<Place>) {
    match operand {
        Operand::Move(local) => {
            moved.insert(Place::new(*local));
        }
        Operand::MovePlace(place) => {
            moved.insert(place.clone());
        }
        _ => {}
    }
}

fn initialized_before_terminators(function: &Function) -> Vec<Option<LocalSet>> {
    if function.blocks.is_empty() {
        return Vec::new();
    }
    let mut entries: Vec<Option<LocalSet>> = vec![None; function.blocks.len()];
    let mut before_terminators: Vec<Option<LocalSet>> = vec![None; function.blocks.len()];
    let entry_index = function.entry_block.0 as usize;
    if entry_index >= function.blocks.len() {
        return before_terminators;
    }
    entries[entry_index] = Some(function.params.iter().map(|(local, _)| *local).collect());
    let mut worklist = VecDeque::from([function.entry_block]);
    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut initialized = entries[index].clone().unwrap_or_default();
        for statement in &block.stmts {
            transfer_initialization_statement(statement, &mut initialized);
        }
        let state_changed = before_terminators[index].as_ref() != Some(&initialized);
        before_terminators[index] = Some(initialized.clone());
        if !state_changed {
            continue;
        }

        let mut outgoing = initialized;
        if let Some(terminator) = &block.terminator {
            transfer_initialization_terminator(terminator, &mut outgoing);
        }
        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(successor_entry) = entries.get_mut(successor_index) else {
                continue;
            };
            let changed = match successor_entry {
                Some(existing) => {
                    let intersection: LocalSet =
                        existing.intersection(&outgoing).copied().collect();
                    if *existing == intersection {
                        false
                    } else {
                        *existing = intersection;
                        true
                    }
                }
                None => {
                    *successor_entry = Some(outgoing.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }
    before_terminators
}

fn transfer_initialization_statement(statement: &super::mir::Statement, state: &mut LocalSet) {
    match &statement.kind {
        StatementKind::Assign(destination, rvalue) => {
            consume_initialization_rvalue(rvalue, state);
            state.insert(*destination);
        }
        StatementKind::AssignPlace(_, rvalue) => consume_initialization_rvalue(rvalue, state),
        StatementKind::IndexAssign(_, index, value) => {
            consume_initialization_operand(index, state);
            consume_initialization_operand(value, state);
        }
        StatementKind::Drop(local) => {
            state.remove(local);
        }
        StatementKind::DropPlace(_) => {}
        StatementKind::Nop => {}
    }
}

fn consume_initialization_rvalue(rvalue: &Rvalue, state: &mut LocalSet) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            consume_initialization_operand(op, state)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            consume_initialization_operand(left, state);
            consume_initialization_operand(right, state);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                consume_initialization_operand(op, state);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                consume_initialization_operand(op, state);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                consume_initialization_operand(op, state);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            consume_initialization_operand(op, state)
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                consume_initialization_operand(capture, state);
            }
        }
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => {}
    }
}

fn transfer_initialization_terminator(terminator: &Terminator, state: &mut LocalSet) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                consume_initialization_operand(op, state);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            consume_initialization_operand(cond, state)
        }
        Terminator::Call { args, dest, .. } => {
            for arg in args {
                consume_initialization_operand(arg, state);
            }
            if let Some(dest) = dest {
                state.insert(*dest);
            }
        }
        Terminator::CallIndirect {
            callee, args, dest, ..
        } => {
            consume_initialization_operand(callee, state);
            for arg in args {
                consume_initialization_operand(arg, state);
            }
            if let Some(dest) = dest {
                state.insert(*dest);
            }
        }
        Terminator::Spawn { expr, dest, .. } => {
            consume_initialization_operand(expr, state);
            if let Some(dest) = dest {
                state.insert(*dest);
            }
        }
        Terminator::Await { task, dest, .. } => {
            consume_initialization_operand(task, state);
            if let Some(dest) = dest {
                state.insert(*dest);
            }
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn consume_initialization_operand(operand: &Operand, state: &mut LocalSet) {
    if let Operand::Move(local) = operand {
        state.remove(local);
    }
}

fn explicit_rvalue(
    rvalue: &mut Rvalue,
    local_types: &[Ty],
    copy_types: &HashSet<String>,
    borrowed_params: &LocalSet,
) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            explicit_operand(
                op,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
        }
        Rvalue::BinaryOp(_, left, right) => {
            // Operators observe their operands.  Their result is a fresh value;
            // spelling `a + b` must not silently transfer ownership of `a` or
            // `b` (notably for string concatenation).
            explicit_operand(
                left,
                local_types,
                PassMode::Ref,
                copy_types,
                borrowed_params,
            );
            explicit_operand(
                right,
                local_types,
                PassMode::Ref,
                copy_types,
                borrowed_params,
            );
            make_operand_borrow(left);
            make_operand_borrow(right);
        }
        Rvalue::Index(left, right) => {
            explicit_operand(
                left,
                local_types,
                PassMode::Ref,
                copy_types,
                borrowed_params,
            );
            explicit_operand(
                right,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
            make_operand_borrow(left);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                explicit_operand(
                    op,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                explicit_operand(
                    op,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                explicit_operand(
                    op,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            explicit_operand(
                op,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                explicit_operand(
                    capture,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => {}
    }
}

fn make_operand_borrow(operand: &mut Operand) {
    match operand {
        Operand::Local(local) | Operand::Move(local) => {
            *operand = Operand::Borrow(*local, Mutability::Immutable);
        }
        Operand::MovePlace(place) => {
            *operand = Operand::BorrowPlace(place.clone(), Mutability::Immutable);
        }
        Operand::Constant(_)
        | Operand::Copy(_)
        | Operand::Borrow(_, _)
        | Operand::CopyPlace(_)
        | Operand::BorrowPlace(_, _) => {}
    }
}

fn explicit_terminator(
    terminator: &mut Terminator,
    local_types: &[Ty],
    copy_types: &HashSet<String>,
    borrowed_params: &LocalSet,
) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                explicit_operand(
                    op,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            explicit_operand(
                cond,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
        }
        Terminator::Call {
            args,
            arg_pass_modes,
            ..
        }
        | Terminator::CallIndirect {
            args,
            arg_pass_modes,
            ..
        } => {
            for (index, arg) in args.iter_mut().enumerate() {
                let mode = arg_pass_modes
                    .get(index)
                    .copied()
                    .unwrap_or(PassMode::Owned);
                explicit_operand(arg, local_types, mode, copy_types, borrowed_params);
            }
            if let Terminator::CallIndirect { callee, .. } = terminator {
                explicit_operand(
                    callee,
                    local_types,
                    PassMode::Owned,
                    copy_types,
                    borrowed_params,
                );
            }
        }
        Terminator::Spawn { expr, .. } => {
            explicit_operand(
                expr,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
        }
        Terminator::Await { task, .. } => {
            explicit_operand(
                task,
                local_types,
                PassMode::Owned,
                copy_types,
                borrowed_params,
            );
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn explicit_operand(
    operand: &mut Operand,
    local_types: &[Ty],
    pass_mode: PassMode,
    copy_types: &HashSet<String>,
    borrowed_params: &LocalSet,
) {
    // FORGE-RUST-GAP: FRG-001. Projection lowering can already make a move
    // explicit before this normalization pass. A projection rooted at a
    // borrowed parameter must remain an observation of the referent.
    if let Operand::MovePlace(place) = operand
        && borrowed_params.contains(&place.local)
    {
        *operand = Operand::BorrowPlace(place.clone(), Mutability::Immutable);
        return;
    }

    let Operand::Local(local) = *operand else {
        return;
    };
    *operand = match pass_mode {
        PassMode::Ref => Operand::Borrow(local, Mutability::Immutable),
        PassMode::RefMut => Operand::Borrow(local, Mutability::Mutable),
        // FORGE-RUST-GAP: FRG-001. A shared or mutable-reference parameter is
        // an alias, never an owner. Observing one through an ownership-taking
        // MIR operation must therefore borrow instead of moving its referent.
        PassMode::Owned if borrowed_params.contains(&local) => {
            Operand::Borrow(local, Mutability::Immutable)
        }
        PassMode::Owned => match local_types.get(local.0 as usize) {
            Some(ty) if is_copy_type(ty, copy_types) => Operand::Copy(local),
            _ => Operand::Move(local),
        },
    };
}

fn is_copy_type(ty: &Ty, copy_types: &HashSet<String>) -> bool {
    match ty {
        Ty::Named(id, args) => {
            copy_types.contains(&id.name)
                && args
                    .iter()
                    .all(|argument| is_copy_type(argument, copy_types))
        }
        Ty::Tuple(fields) => fields.iter().all(|field| is_copy_type(field, copy_types)),
        Ty::Array(element, _) => is_copy_type(element, copy_types),
        _ => ty.is_copy(),
    }
}

fn analyze_function(
    function: &Function,
    send_types: &HashSet<String>,
    report: &mut OwnershipReport,
) {
    report.legacy_local_reads += count_legacy_reads(function);
    if function.blocks.is_empty() {
        return;
    }

    let mut entry_states: Vec<Option<HashSet<Local>>> = vec![None; function.blocks.len()];
    let mut exit_states: Vec<Option<HashSet<Local>>> = vec![None; function.blocks.len()];
    let entry = function.entry_block.0 as usize;
    if entry >= function.blocks.len() {
        return;
    }
    entry_states[entry] = Some(HashSet::new());
    let mut worklist = VecDeque::from([function.entry_block]);

    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut moved = entry_states[index].clone().unwrap_or_default();

        for statement in &block.stmts {
            match &statement.kind {
                StatementKind::Assign(destination, rvalue) => {
                    visit_rvalue(function, block.id, rvalue, &mut moved, report);
                    moved.remove(destination);
                }
                StatementKind::AssignPlace(destination, rvalue) => {
                    visit_read(function, block.id, destination.local, &moved, report);
                    visit_rvalue(function, block.id, rvalue, &mut moved, report);
                }
                StatementKind::IndexAssign(local, index, value) => {
                    visit_read(function, block.id, *local, &moved, report);
                    visit_operand(function, block.id, index, &mut moved, report);
                    visit_operand(function, block.id, value, &mut moved, report);
                }
                StatementKind::Drop(local) => {
                    visit_read(function, block.id, *local, &moved, report);
                    moved.insert(*local);
                }
                StatementKind::DropPlace(place) => {
                    visit_read(function, block.id, place.local, &moved, report);
                }
                StatementKind::Nop => {}
            }
        }
        if let Some(terminator) = &block.terminator {
            visit_terminator(function, block.id, terminator, &mut moved, report);
        }

        if exit_states[index].as_ref() == Some(&moved) {
            continue;
        }
        exit_states[index] = Some(moved.clone());

        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(successor_entry) = entry_states.get_mut(successor_index) else {
                continue;
            };
            let changed = match successor_entry {
                Some(existing) => {
                    let previous_len = existing.len();
                    existing.extend(moved.iter().copied());
                    existing.len() != previous_len
                }
                None => {
                    *successor_entry = Some(moved.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }

    analyze_projected_places(function, report);
    analyze_initialization(function, report);
    analyze_loans(function, report);
    analyze_task_boundaries(function, send_types, report);

    let mut seen = HashSet::new();
    report.errors.retain(|error| {
        seen.insert(format!(
            "{}:{}:{}:{:?}",
            error.function, error.block.0, error.local.0, error.kind
        ))
    });
}

fn analyze_task_boundaries(
    function: &Function,
    send_types: &HashSet<String>,
    report: &mut OwnershipReport,
) {
    let mut reference_closures = HashSet::new();
    let mut non_send_closures = HashSet::new();
    loop {
        let previous_len = (reference_closures.len(), non_send_closures.len());
        for block in &function.blocks {
            for statement in &block.stmts {
                let StatementKind::Assign(destination, rvalue) = &statement.kind else {
                    continue;
                };
                match rvalue {
                    Rvalue::Closure { captures, .. }
                        if captures.iter().any(|capture| {
                            operand_local(capture).is_some_and(|local| {
                                matches!(local_type(function, local), Some(Ty::Ref(_, _)))
                                    || reference_closures.contains(&local)
                            })
                        }) =>
                    {
                        reference_closures.insert(*destination);
                    }
                    Rvalue::Closure { captures, .. }
                        if captures.iter().any(|capture| {
                            operand_local(capture).is_some_and(|local| {
                                local_type(function, local)
                                    .is_some_and(|ty| !super::mir::is_send_type(ty, send_types))
                                    || non_send_closures.contains(&local)
                            })
                        }) =>
                    {
                        non_send_closures.insert(*destination);
                    }
                    Rvalue::Use(operand)
                        if operand_local(operand)
                            .is_some_and(|local| reference_closures.contains(&local)) =>
                    {
                        reference_closures.insert(*destination);
                    }
                    Rvalue::Use(operand)
                        if operand_local(operand)
                            .is_some_and(|local| non_send_closures.contains(&local)) =>
                    {
                        non_send_closures.insert(*destination);
                    }
                    _ => {}
                }
            }
        }
        if (reference_closures.len(), non_send_closures.len()) == previous_len {
            break;
        }
    }

    for block in &function.blocks {
        let Some(Terminator::Spawn { expr, .. }) = &block.terminator else {
            continue;
        };
        let Some(local) = operand_local(expr) else {
            continue;
        };
        if matches!(local_type(function, local), Some(Ty::Ref(_, _)))
            || reference_closures.contains(&local)
        {
            report.errors.push(OwnershipError {
                function: function.name.clone(),
                block: block.id,
                local,
                kind: OwnershipErrorKind::ReferenceAcrossTask,
            });
        } else if non_send_closures.contains(&local)
            || local_type(function, local).is_some_and(|ty| {
                !matches!(ty, Ty::Fn(_, _)) && !super::mir::is_send_type(ty, send_types)
            })
        {
            report.errors.push(OwnershipError {
                function: function.name.clone(),
                block: block.id,
                local,
                kind: OwnershipErrorKind::NonSendAcrossTask {
                    ty: local_type(function, local).cloned().unwrap_or(Ty::Error),
                },
            });
        }
    }
}

fn operand_local(operand: &Operand) -> Option<Local> {
    match operand {
        Operand::Local(local)
        | Operand::Copy(local)
        | Operand::Move(local)
        | Operand::Borrow(local, _) => Some(*local),
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            Some(place.local)
        }
        Operand::Constant(_) => None,
    }
}

fn analyze_projected_places(function: &Function, report: &mut OwnershipReport) {
    if function.blocks.is_empty() {
        return;
    }
    let entry_index = function.entry_block.0 as usize;
    if entry_index >= function.blocks.len() {
        return;
    }
    let mut entries: Vec<Option<HashSet<Place>>> = vec![None; function.blocks.len()];
    let mut exits: Vec<Option<HashSet<Place>>> = vec![None; function.blocks.len()];
    entries[entry_index] = Some(HashSet::new());
    let mut worklist = VecDeque::from([function.entry_block]);

    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut moved = entries[index].clone().unwrap_or_default();
        for statement in &block.stmts {
            match &statement.kind {
                StatementKind::Assign(destination, rvalue) => {
                    check_projected_rvalue(function, block.id, rvalue, &mut moved, report);
                    moved.retain(|place| place.local != *destination);
                }
                StatementKind::AssignPlace(destination, rvalue) => {
                    if moved.iter().any(|moved_place| {
                        moved_place.overlaps(destination)
                            && moved_place.projection.len() < destination.projection.len()
                    }) {
                        push_loan_error(
                            function,
                            block.id,
                            destination.local,
                            OwnershipErrorKind::UseOfPartiallyMoved {
                                place: destination.to_string(),
                            },
                            report,
                        );
                    }
                    check_projected_rvalue(function, block.id, rvalue, &mut moved, report);
                    moved.retain(|place| !place.overlaps(destination));
                }
                StatementKind::IndexAssign(destination, index, value) => {
                    check_projected_place_read(
                        function,
                        block.id,
                        &Place::new(*destination),
                        &moved,
                        report,
                    );
                    check_projected_operand(function, block.id, index, &mut moved, report);
                    check_projected_operand(function, block.id, value, &mut moved, report);
                }
                StatementKind::Drop(local) => {
                    let place = Place::new(*local);
                    if moved.iter().any(|moved_place| moved_place.overlaps(&place)) {
                        push_loan_error(
                            function,
                            block.id,
                            *local,
                            OwnershipErrorKind::DropOfPartiallyMoved {
                                place: place.to_string(),
                            },
                            report,
                        );
                    }
                    moved.insert(place);
                }
                StatementKind::DropPlace(place) => {
                    if moved.iter().any(|moved_place| moved_place.overlaps(place)) {
                        push_loan_error(
                            function,
                            block.id,
                            place.local,
                            OwnershipErrorKind::DoubleDrop,
                            report,
                        );
                    }
                    moved.insert(place.clone());
                }
                StatementKind::Nop => {}
            }
        }
        if let Some(terminator) = &block.terminator {
            check_projected_terminator(function, block.id, terminator, &mut moved, report);
        }

        if exits[index].as_ref() == Some(&moved) {
            continue;
        }
        exits[index] = Some(moved.clone());
        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(successor_entry) = entries.get_mut(successor_index) else {
                continue;
            };
            let changed = match successor_entry {
                Some(existing) => {
                    let old_len = existing.len();
                    existing.extend(moved.iter().cloned());
                    existing.len() != old_len
                }
                None => {
                    *successor_entry = Some(moved.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }
}

fn check_projected_rvalue(
    function: &Function,
    block: BlockId,
    rvalue: &Rvalue,
    moved: &mut HashSet<Place>,
    report: &mut OwnershipReport,
) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            check_projected_operand(function, block, op, moved, report)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            check_projected_operand(function, block, left, moved, report);
            check_projected_operand(function, block, right, moved, report);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                check_projected_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                check_projected_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                check_projected_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            check_projected_operand(function, block, op, moved, report)
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                check_projected_operand(function, block, capture, moved, report);
            }
        }
        Rvalue::RefPlace(place, _) => {
            check_projected_place_read(function, block, place, moved, report)
        }
        Rvalue::Ref(local, _) | Rvalue::Discriminant(local) | Rvalue::EnumField(local, _) => {
            check_projected_place_read(function, block, &Place::new(*local), moved, report)
        }
    }
}

fn check_projected_terminator(
    function: &Function,
    block: BlockId,
    terminator: &Terminator,
    moved: &mut HashSet<Place>,
    report: &mut OwnershipReport,
) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                check_projected_operand(function, block, op, moved, report);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            check_projected_operand(function, block, cond, moved, report)
        }
        Terminator::Call { args, dest, .. } => {
            for arg in args {
                check_projected_operand(function, block, arg, moved, report);
            }
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::CallIndirect {
            callee, args, dest, ..
        } => {
            check_projected_operand(function, block, callee, moved, report);
            for arg in args {
                check_projected_operand(function, block, arg, moved, report);
            }
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Spawn { expr, dest, .. } => {
            check_projected_operand(function, block, expr, moved, report);
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Await { task, dest, .. } => {
            check_projected_operand(function, block, task, moved, report);
            if let Some(dest) = dest {
                moved.retain(|place| place.local != *dest);
            }
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn check_projected_operand(
    function: &Function,
    block: BlockId,
    operand: &Operand,
    moved: &mut HashSet<Place>,
    report: &mut OwnershipReport,
) {
    let (place, is_move) = match operand {
        Operand::Local(local) | Operand::Copy(local) | Operand::Borrow(local, _) => {
            (Place::new(*local), false)
        }
        Operand::Move(local) => (Place::new(*local), true),
        Operand::CopyPlace(place) | Operand::BorrowPlace(place, _) => (place.clone(), false),
        Operand::MovePlace(place) => (place.clone(), true),
        Operand::Constant(_) => return,
    };
    check_projected_place_read(function, block, &place, moved, report);
    if is_move {
        moved.insert(place);
    }
}

fn check_projected_place_read(
    function: &Function,
    block: BlockId,
    place: &Place,
    moved: &HashSet<Place>,
    report: &mut OwnershipReport,
) {
    if moved.iter().any(|moved_place| moved_place.overlaps(place)) {
        push_loan_error(
            function,
            block,
            place.local,
            OwnershipErrorKind::UseOfPartiallyMoved {
                place: place.to_string(),
            },
            report,
        );
    }
}

fn analyze_initialization(function: &Function, report: &mut OwnershipReport) {
    if function.blocks.is_empty() {
        return;
    }
    let entry_index = function.entry_block.0 as usize;
    if entry_index >= function.blocks.len() {
        return;
    }

    let parameter_locals: LocalSet = function.params.iter().map(|(local, _)| *local).collect();
    let mut entry_states: Vec<Option<LocalSet>> = vec![None; function.blocks.len()];
    let mut exit_states: Vec<Option<LocalSet>> = vec![None; function.blocks.len()];
    entry_states[entry_index] = Some(parameter_locals);
    let mut worklist = VecDeque::from([function.entry_block]);

    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut initialized = entry_states[index].clone().unwrap_or_default();
        for statement in &block.stmts {
            match &statement.kind {
                StatementKind::Assign(destination, rvalue) => {
                    check_initialized_rvalue(function, block.id, rvalue, &mut initialized, report);
                    initialized.insert(*destination);
                }
                StatementKind::AssignPlace(destination, rvalue) => {
                    check_initialized_read(
                        function,
                        block.id,
                        destination.local,
                        &initialized,
                        report,
                    );
                    check_initialized_rvalue(function, block.id, rvalue, &mut initialized, report);
                }
                StatementKind::IndexAssign(destination, index, value) => {
                    check_initialized_read(function, block.id, *destination, &initialized, report);
                    check_initialized_operand(function, block.id, index, &mut initialized, report);
                    check_initialized_operand(function, block.id, value, &mut initialized, report);
                }
                StatementKind::Drop(local) => {
                    if !initialized.remove(local) {
                        push_loan_error(
                            function,
                            block.id,
                            *local,
                            OwnershipErrorKind::DoubleDrop,
                            report,
                        );
                    }
                }
                StatementKind::DropPlace(place) => {
                    check_initialized_read(function, block.id, place.local, &initialized, report);
                }
                StatementKind::Nop => {}
            }
        }
        if let Some(terminator) = &block.terminator {
            check_initialized_terminator(function, block.id, terminator, &mut initialized, report);
        }

        if exit_states[index].as_ref() == Some(&initialized) {
            continue;
        }
        exit_states[index] = Some(initialized.clone());
        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            let Some(successor_entry) = entry_states.get_mut(successor_index) else {
                continue;
            };
            let changed = match successor_entry {
                Some(existing) => {
                    let intersection: LocalSet =
                        existing.intersection(&initialized).copied().collect();
                    if *existing == intersection {
                        false
                    } else {
                        *existing = intersection;
                        true
                    }
                }
                None => {
                    *successor_entry = Some(initialized.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(successor);
            }
        }
    }
}

fn check_initialized_rvalue(
    function: &Function,
    block: BlockId,
    rvalue: &Rvalue,
    initialized: &mut LocalSet,
    report: &mut OwnershipReport,
) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            check_initialized_operand(function, block, op, initialized, report)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            check_initialized_operand(function, block, left, initialized, report);
            check_initialized_operand(function, block, right, initialized, report);
        }
        Rvalue::RefPlace(place, _) => {
            check_initialized_read(function, block, place.local, initialized, report)
        }
        Rvalue::Ref(local, _) | Rvalue::Discriminant(local) | Rvalue::EnumField(local, _) => {
            check_initialized_read(function, block, *local, initialized, report)
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                check_initialized_operand(function, block, op, initialized, report);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                check_initialized_operand(function, block, op, initialized, report);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                check_initialized_operand(function, block, op, initialized, report);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            check_initialized_operand(function, block, op, initialized, report)
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                check_initialized_operand(function, block, capture, initialized, report);
            }
        }
    }
}

fn check_initialized_terminator(
    function: &Function,
    block: BlockId,
    terminator: &Terminator,
    initialized: &mut LocalSet,
    report: &mut OwnershipReport,
) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                check_initialized_operand(function, block, op, initialized, report);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            check_initialized_operand(function, block, cond, initialized, report)
        }
        Terminator::Call { args, dest, .. } => {
            for arg in args {
                check_initialized_operand(function, block, arg, initialized, report);
            }
            if let Some(dest) = dest {
                initialized.insert(*dest);
            }
        }
        Terminator::CallIndirect {
            callee, args, dest, ..
        } => {
            check_initialized_operand(function, block, callee, initialized, report);
            for arg in args {
                check_initialized_operand(function, block, arg, initialized, report);
            }
            if let Some(dest) = dest {
                initialized.insert(*dest);
            }
        }
        Terminator::Spawn { expr, dest, .. } => {
            check_initialized_operand(function, block, expr, initialized, report);
            if let Some(dest) = dest {
                initialized.insert(*dest);
            }
        }
        Terminator::Await { task, dest, .. } => {
            check_initialized_operand(function, block, task, initialized, report);
            if let Some(dest) = dest {
                initialized.insert(*dest);
            }
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn check_initialized_operand(
    function: &Function,
    block: BlockId,
    operand: &Operand,
    initialized: &mut LocalSet,
    report: &mut OwnershipReport,
) {
    match operand {
        Operand::Local(local) | Operand::Copy(local) | Operand::Borrow(local, _) => {
            check_initialized_read(function, block, *local, initialized, report)
        }
        Operand::Move(local) => {
            check_initialized_read(function, block, *local, initialized, report);
            initialized.remove(local);
        }
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            check_initialized_read(function, block, place.local, initialized, report)
        }
        Operand::Constant(_) => {}
    }
}

fn check_initialized_read(
    function: &Function,
    block: BlockId,
    local: Local,
    initialized: &LocalSet,
    report: &mut OwnershipReport,
) {
    if initialized.contains(&local)
        || report.errors.iter().any(|error| {
            error.function == function.name
                && error.block == block
                && error.local == local
                && error.kind == OwnershipErrorKind::UseAfterMove
        })
    {
        return;
    }
    push_loan_error(
        function,
        block,
        local,
        OwnershipErrorKind::UseOfUninitialized,
        report,
    );
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Loan {
    holder: Local,
    source: Place,
    mutability: Mutability,
}

/// Infer non-lexical loan regions from reference-local liveness and validate
/// moves, mutations, and overlapping loans.
fn analyze_loans(function: &Function, report: &mut OwnershipReport) {
    if function.blocks.is_empty() {
        return;
    }
    let (live_in, live_out, live_after_statements) = compute_liveness(function);
    let mut entry_loans: Vec<HashSet<Loan>> = vec![HashSet::new(); function.blocks.len()];
    let mut exit_loans: Vec<Option<HashSet<Loan>>> = vec![None; function.blocks.len()];
    let mut reached = vec![false; function.blocks.len()];
    let entry = function.entry_block.0 as usize;
    if entry >= function.blocks.len() {
        return;
    }
    reached[entry] = true;
    let mut worklist = VecDeque::from([function.entry_block]);

    while let Some(block_id) = worklist.pop_front() {
        let index = block_id.0 as usize;
        let Some(block) = function.blocks.get(index) else {
            continue;
        };
        let mut active = entry_loans[index].clone();
        active.retain(|loan| live_in[index].contains(&loan.holder));

        for (statement_index, statement) in block.stmts.iter().enumerate() {
            analyze_loan_statement(function, block.id, statement, &mut active, report);
            if let Some(live_after) = live_after_statements[index].get(statement_index) {
                active.retain(|loan| live_after.contains(&loan.holder));
            }
        }
        if let Some(terminator) = &block.terminator {
            analyze_loan_terminator(function, block.id, terminator, &active, report);
        }
        active.retain(|loan| live_out[index].contains(&loan.holder));

        if exit_loans[index].as_ref() == Some(&active) {
            continue;
        }
        exit_loans[index] = Some(active.clone());
        for successor in block_successors(block.terminator.as_ref()) {
            let successor_index = successor.0 as usize;
            if successor_index >= function.blocks.len() {
                continue;
            }
            let previous_len = entry_loans[successor_index].len();
            entry_loans[successor_index].extend(active.iter().cloned());
            if !reached[successor_index] || entry_loans[successor_index].len() != previous_len {
                reached[successor_index] = true;
                worklist.push_back(successor);
            }
        }
    }
}

fn analyze_loan_statement(
    function: &Function,
    block: BlockId,
    statement: &super::mir::Statement,
    active: &mut HashSet<Loan>,
    report: &mut OwnershipReport,
) {
    match &statement.kind {
        StatementKind::Assign(destination, Rvalue::Ref(source, mutability)) => {
            active.retain(|loan| loan.holder != *destination);
            let source = Place::new(*source);
            check_new_loan(function, block, &source, *mutability, active, report);
            active.insert(Loan {
                holder: *destination,
                source,
                mutability: *mutability,
            });
        }
        StatementKind::Assign(destination, Rvalue::RefPlace(source, mutability)) => {
            active.retain(|loan| loan.holder != *destination);
            check_new_loan(function, block, source, *mutability, active, report);
            active.insert(Loan {
                holder: *destination,
                source: source.clone(),
                mutability: *mutability,
            });
        }
        StatementKind::Assign(destination, rvalue) => {
            let destination_place = Place::new(*destination);
            if active
                .iter()
                .any(|loan| loan.source.overlaps(&destination_place))
            {
                push_loan_error(
                    function,
                    block,
                    *destination,
                    OwnershipErrorKind::MutationWhileBorrowed,
                    report,
                );
            }
            active.retain(|loan| loan.holder != *destination);
            analyze_rvalue_loan_uses(function, block, rvalue, active, report);
        }
        StatementKind::AssignPlace(destination, rvalue) => {
            if active.iter().any(|loan| loan.source.overlaps(destination)) {
                push_loan_error(
                    function,
                    block,
                    destination.local,
                    OwnershipErrorKind::MutationWhileBorrowed,
                    report,
                );
            }
            analyze_rvalue_loan_uses(function, block, rvalue, active, report);
        }
        StatementKind::IndexAssign(destination, index, value) => {
            let destination_place = Place::new(*destination);
            if active
                .iter()
                .any(|loan| loan.source.overlaps(&destination_place))
            {
                push_loan_error(
                    function,
                    block,
                    *destination,
                    OwnershipErrorKind::MutationWhileBorrowed,
                    report,
                );
            }
            analyze_operand_loan_use(function, block, index, active, report);
            analyze_operand_loan_use(function, block, value, active, report);
        }
        StatementKind::Drop(local) => {
            let dropped = Place::new(*local);
            if active.iter().any(|loan| loan.source.overlaps(&dropped)) {
                push_loan_error(
                    function,
                    block,
                    *local,
                    OwnershipErrorKind::MoveWhileBorrowed,
                    report,
                );
            }
            active.retain(|loan| loan.holder != *local);
        }
        StatementKind::DropPlace(place) => {
            if active.iter().any(|loan| loan.source.overlaps(place)) {
                push_loan_error(
                    function,
                    block,
                    place.local,
                    OwnershipErrorKind::MoveWhileBorrowed,
                    report,
                );
            }
        }
        StatementKind::Nop => {}
    }
}

fn analyze_rvalue_loan_uses(
    function: &Function,
    block: BlockId,
    rvalue: &Rvalue,
    active: &HashSet<Loan>,
    report: &mut OwnershipReport,
) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            analyze_operand_loan_use(function, block, op, active, report)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            analyze_operand_loan_use(function, block, left, active, report);
            analyze_operand_loan_use(function, block, right, active, report);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                analyze_operand_loan_use(function, block, op, active, report);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                analyze_operand_loan_use(function, block, op, active, report);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                analyze_operand_loan_use(function, block, op, active, report);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            analyze_operand_loan_use(function, block, op, active, report)
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                analyze_operand_loan_use(function, block, capture, active, report);
            }
        }
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => {}
    }
}

fn analyze_loan_terminator(
    function: &Function,
    block: BlockId,
    terminator: &Terminator,
    active: &HashSet<Loan>,
    report: &mut OwnershipReport,
) {
    let mut ephemeral = active.clone();
    let mut visit = |operand: &Operand| {
        let borrowed = match operand {
            Operand::Borrow(source, mutability) => Some((Place::new(*source), *mutability)),
            Operand::BorrowPlace(source, mutability) => Some((source.clone(), *mutability)),
            _ => None,
        };
        if let Some((source, mutability)) = borrowed {
            check_new_loan(function, block, &source, mutability, &ephemeral, report);
            ephemeral.insert(Loan {
                holder: Local(u32::MAX),
                source,
                mutability,
            });
        } else {
            analyze_operand_loan_use(function, block, operand, active, report);
        }
    };
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                visit(op);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => visit(cond),
        Terminator::Call { args, .. } => args.iter().for_each(&mut visit),
        Terminator::CallIndirect { callee, args, .. } => {
            visit(callee);
            args.iter().for_each(&mut visit);
        }
        Terminator::Spawn { expr, .. } => visit(expr),
        Terminator::Await { task, .. } => visit(task),
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn analyze_operand_loan_use(
    function: &Function,
    block: BlockId,
    operand: &Operand,
    active: &HashSet<Loan>,
    report: &mut OwnershipReport,
) {
    let moved = match operand {
        Operand::Move(local) => Some(Place::new(*local)),
        Operand::MovePlace(place) => Some(place.clone()),
        _ => None,
    };
    if let Some(moved) = moved
        && active.iter().any(|loan| loan.source.overlaps(&moved))
    {
        push_loan_error(
            function,
            block,
            moved.local,
            OwnershipErrorKind::MoveWhileBorrowed,
            report,
        );
    }
}

fn check_new_loan(
    function: &Function,
    block: BlockId,
    source: &Place,
    requested: Mutability,
    active: &HashSet<Loan>,
    report: &mut OwnershipReport,
) {
    if let Some(existing) = active.iter().find(|loan| {
        loan.source.overlaps(source)
            && (requested == Mutability::Mutable || loan.mutability == Mutability::Mutable)
    }) {
        push_loan_error(
            function,
            block,
            source.local,
            OwnershipErrorKind::ConflictingLoan {
                requested,
                existing: existing.mutability,
            },
            report,
        );
    }
}

fn push_loan_error(
    function: &Function,
    block: BlockId,
    local: Local,
    kind: OwnershipErrorKind,
    report: &mut OwnershipReport,
) {
    report.errors.push(OwnershipError {
        function: function.name.clone(),
        block,
        local,
        kind,
    });
}

type LocalSet = HashSet<Local>;
type StatementLiveness = Vec<Vec<LocalSet>>;

fn compute_liveness(function: &Function) -> (Vec<LocalSet>, Vec<LocalSet>, StatementLiveness) {
    let block_count = function.blocks.len();
    let mut block_uses = vec![LocalSet::new(); block_count];
    let mut block_defs = vec![LocalSet::new(); block_count];

    for (index, block) in function.blocks.iter().enumerate() {
        for statement in &block.stmts {
            let (uses, definition) = statement_uses_and_def(statement);
            for local in uses {
                if !block_defs[index].contains(&local) {
                    block_uses[index].insert(local);
                }
            }
            if let Some(local) = definition {
                block_defs[index].insert(local);
            }
        }
        if let Some(terminator) = &block.terminator {
            for local in terminator_uses(terminator) {
                if !block_defs[index].contains(&local) {
                    block_uses[index].insert(local);
                }
            }
        }
    }

    let mut live_in = vec![LocalSet::new(); block_count];
    let mut live_out = vec![LocalSet::new(); block_count];
    loop {
        let mut changed = false;
        for (index, block) in function.blocks.iter().enumerate().rev() {
            let mut new_out = LocalSet::new();
            for successor in block_successors(block.terminator.as_ref()) {
                if let Some(successor_live) = live_in.get(successor.0 as usize) {
                    new_out.extend(successor_live.iter().copied());
                }
            }
            let mut new_in = block_uses[index].clone();
            new_in.extend(
                new_out
                    .iter()
                    .filter(|local| !block_defs[index].contains(local))
                    .copied(),
            );
            if new_out != live_out[index] || new_in != live_in[index] {
                live_out[index] = new_out;
                live_in[index] = new_in;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let mut live_after_statements = Vec::with_capacity(block_count);
    for (index, block) in function.blocks.iter().enumerate() {
        let mut live = live_out[index].clone();
        if let Some(terminator) = &block.terminator {
            live.extend(terminator_uses(terminator));
        }
        let mut after = vec![LocalSet::new(); block.stmts.len()];
        for statement_index in (0..block.stmts.len()).rev() {
            after[statement_index] = live.clone();
            let (uses, definition) = statement_uses_and_def(&block.stmts[statement_index]);
            if let Some(local) = definition {
                live.remove(&local);
            }
            live.extend(uses);
        }
        live_after_statements.push(after);
    }
    (live_in, live_out, live_after_statements)
}

fn statement_uses_and_def(statement: &super::mir::Statement) -> (LocalSet, Option<Local>) {
    match &statement.kind {
        StatementKind::Assign(destination, rvalue) => (rvalue_uses(rvalue), Some(*destination)),
        StatementKind::AssignPlace(destination, rvalue) => {
            let mut uses = rvalue_uses(rvalue);
            add_place_use(destination, &mut uses);
            (uses, None)
        }
        StatementKind::IndexAssign(destination, index, value) => {
            let mut uses = LocalSet::from([*destination]);
            add_operand_use(index, &mut uses);
            add_operand_use(value, &mut uses);
            (uses, None)
        }
        StatementKind::Drop(local) => (LocalSet::from([*local]), None),
        StatementKind::DropPlace(place) => (LocalSet::from([place.local]), None),
        StatementKind::Nop => (LocalSet::new(), None),
    }
}

fn rvalue_uses(rvalue: &Rvalue) -> LocalSet {
    let mut uses = LocalSet::new();
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            add_operand_use(op, &mut uses)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            add_operand_use(left, &mut uses);
            add_operand_use(right, &mut uses);
        }
        Rvalue::RefPlace(place, _) => add_place_use(place, &mut uses),
        Rvalue::Ref(local, _) | Rvalue::Discriminant(local) | Rvalue::EnumField(local, _) => {
            uses.insert(*local);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                add_operand_use(op, &mut uses);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                add_operand_use(op, &mut uses);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                add_operand_use(op, &mut uses);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => add_operand_use(op, &mut uses),
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                add_operand_use(capture, &mut uses);
            }
        }
    }
    uses
}

fn add_place_use(place: &Place, uses: &mut LocalSet) {
    uses.insert(place.local);
    for projection in &place.projection {
        if let super::mir::ProjectionElem::Index(local) = projection {
            uses.insert(*local);
        }
    }
}

fn terminator_uses(terminator: &Terminator) -> LocalSet {
    let mut uses = LocalSet::new();
    let mut add = |operand: &Operand| add_operand_use(operand, &mut uses);
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                add(op);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => add(cond),
        Terminator::Call { args, .. } => args.iter().for_each(&mut add),
        Terminator::CallIndirect { callee, args, .. } => {
            add(callee);
            args.iter().for_each(&mut add);
        }
        Terminator::Spawn { expr, .. } => add(expr),
        Terminator::Await { task, .. } => add(task),
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
    uses
}

fn add_operand_use(operand: &Operand, uses: &mut LocalSet) {
    match operand {
        Operand::Local(local)
        | Operand::Copy(local)
        | Operand::Move(local)
        | Operand::Borrow(local, _) => {
            uses.insert(*local);
        }
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            uses.insert(place.local);
            for projection in &place.projection {
                if let super::mir::ProjectionElem::Index(local) = projection {
                    uses.insert(*local);
                }
            }
        }
        Operand::Constant(_) => {}
    }
}

fn block_successors(terminator: Option<&Terminator>) -> Vec<BlockId> {
    match terminator {
        Some(Terminator::Goto(target)) => vec![*target],
        Some(Terminator::If {
            then_block,
            else_block,
            ..
        }) => vec![*then_block, *else_block],
        Some(Terminator::Switch {
            targets, default, ..
        }) => {
            let mut successors: Vec<_> = targets.iter().map(|(_, block)| *block).collect();
            successors.push(*default);
            successors
        }
        Some(Terminator::Call { next, .. })
        | Some(Terminator::CallIndirect { next, .. })
        | Some(Terminator::Spawn { next, .. })
        | Some(Terminator::Await { next, .. }) => vec![*next],
        Some(Terminator::Return(_) | Terminator::Unreachable) | None => Vec::new(),
    }
}

fn count_legacy_reads(function: &Function) -> usize {
    function
        .blocks
        .iter()
        .map(|block| {
            let statement_reads: usize = block
                .stmts
                .iter()
                .map(|statement| match &statement.kind {
                    StatementKind::Assign(_, rvalue) | StatementKind::AssignPlace(_, rvalue) => {
                        count_legacy_rvalue(rvalue)
                    }
                    StatementKind::IndexAssign(_, index, value) => {
                        usize::from(matches!(index, Operand::Local(_)))
                            + usize::from(matches!(value, Operand::Local(_)))
                    }
                    StatementKind::Drop(_) | StatementKind::DropPlace(_) => 0,
                    StatementKind::Nop => 0,
                })
                .sum();
            statement_reads
                + block
                    .terminator
                    .as_ref()
                    .map(count_legacy_terminator)
                    .unwrap_or(0)
        })
        .sum()
}

fn count_legacy_rvalue(rvalue: &Rvalue) -> usize {
    let count = |operand: &Operand| usize::from(matches!(operand, Operand::Local(_)));
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            count(op)
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => count(left) + count(right),
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => ops.iter().map(count).sum(),
        Rvalue::Struct(_, fields) => fields.iter().map(|(_, op)| count(op)).sum(),
        Rvalue::Enum { fields, .. } => fields.iter().map(count).sum(),
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => count(op),
        Rvalue::Closure { captures, .. } => captures.iter().map(count).sum(),
        Rvalue::RefPlace(_, _)
        | Rvalue::Ref(_, _)
        | Rvalue::Discriminant(_)
        | Rvalue::EnumField(_, _) => 0,
    }
}

fn count_legacy_terminator(terminator: &Terminator) -> usize {
    let count = |operand: &Operand| usize::from(matches!(operand, Operand::Local(_)));
    match terminator {
        Terminator::Return(op) => op.as_ref().map(count).unwrap_or(0),
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => count(cond),
        Terminator::Call { args, .. } => args.iter().map(count).sum(),
        Terminator::CallIndirect { callee, args, .. } => {
            count(callee) + args.iter().map(count).sum::<usize>()
        }
        Terminator::Spawn { expr, .. } => count(expr),
        Terminator::Await { task, .. } => count(task),
        Terminator::Goto(_) | Terminator::Unreachable => 0,
    }
}

fn visit_rvalue(
    function: &Function,
    block: BlockId,
    rvalue: &Rvalue,
    moved: &mut HashSet<Local>,
    report: &mut OwnershipReport,
) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) | Rvalue::Cast(op, _) => {
            visit_operand(function, block, op, moved, report);
        }
        Rvalue::BinaryOp(_, left, right) | Rvalue::Index(left, right) => {
            visit_operand(function, block, left, moved, report);
            visit_operand(function, block, right, moved, report);
        }
        Rvalue::RefPlace(place, _) => visit_read(function, block, place.local, moved, report),
        Rvalue::Ref(local, _) | Rvalue::Discriminant(local) | Rvalue::EnumField(local, _) => {
            visit_read(function, block, *local, moved, report);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                visit_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                visit_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                visit_operand(function, block, op, moved, report);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) => {
            visit_operand(function, block, op, moved, report);
        }
        Rvalue::Closure { captures, .. } => {
            for capture in captures {
                visit_operand(function, block, capture, moved, report);
            }
        }
    }
}

fn visit_terminator(
    function: &Function,
    block: BlockId,
    terminator: &Terminator,
    moved: &mut HashSet<Local>,
    report: &mut OwnershipReport,
) {
    match terminator {
        Terminator::Return(op) => {
            if let Some(op) = op {
                visit_operand(function, block, op, moved, report);
            }
        }
        Terminator::If { cond, .. } | Terminator::Switch { operand: cond, .. } => {
            visit_operand(function, block, cond, moved, report);
        }
        Terminator::Call { args, dest, .. } => {
            for arg in args {
                visit_operand(function, block, arg, moved, report);
            }
            if let Some(dest) = dest {
                moved.remove(dest);
            }
        }
        Terminator::CallIndirect {
            callee, args, dest, ..
        } => {
            visit_operand(function, block, callee, moved, report);
            for arg in args {
                visit_operand(function, block, arg, moved, report);
            }
            if let Some(dest) = dest {
                moved.remove(dest);
            }
        }
        Terminator::Spawn { expr, dest, .. } => {
            visit_operand(function, block, expr, moved, report);
            if let Some(dest) = dest {
                moved.remove(dest);
            }
        }
        Terminator::Await { task, dest, .. } => {
            visit_operand(function, block, task, moved, report);
            if let Some(dest) = dest {
                moved.remove(dest);
            }
        }
        Terminator::Goto(_) | Terminator::Unreachable => {}
    }
}

fn visit_operand(
    function: &Function,
    block: BlockId,
    operand: &Operand,
    moved: &mut HashSet<Local>,
    report: &mut OwnershipReport,
) {
    match operand {
        Operand::Constant(_) => {}
        Operand::Local(local) => {
            visit_read(function, block, *local, moved, report);
        }
        Operand::Copy(local) => {
            visit_read(function, block, *local, moved, report);
            match local_type(function, *local) {
                Some(ty) if !ty.is_copy() => report.errors.push(OwnershipError {
                    function: function.name.clone(),
                    block,
                    local: *local,
                    kind: OwnershipErrorKind::CopyOfNonCopy { ty: ty.clone() },
                }),
                Some(_) => {}
                None => push_unknown(function, block, *local, report),
            }
        }
        Operand::Move(local) => {
            visit_read(function, block, *local, moved, report);
            if local_type(function, *local).is_some() {
                moved.insert(*local);
            } else {
                push_unknown(function, block, *local, report);
            }
        }
        Operand::Borrow(local, _) => visit_read(function, block, *local, moved, report),
        Operand::CopyPlace(place) | Operand::MovePlace(place) | Operand::BorrowPlace(place, _) => {
            visit_read(function, block, place.local, moved, report)
        }
    }
}

fn visit_read(
    function: &Function,
    block: BlockId,
    local: Local,
    moved: &HashSet<Local>,
    report: &mut OwnershipReport,
) {
    if local_type(function, local).is_none() {
        push_unknown(function, block, local, report);
    } else if moved.contains(&local) {
        report.errors.push(OwnershipError {
            function: function.name.clone(),
            block,
            local,
            kind: OwnershipErrorKind::UseAfterMove,
        });
    }
}

fn local_type(function: &Function, local: Local) -> Option<&Ty> {
    function.locals.get(local.0 as usize).map(|decl| &decl.ty)
}

fn push_unknown(function: &Function, block: BlockId, local: Local, report: &mut OwnershipReport) {
    report.errors.push(OwnershipError {
        function: function.name.clone(),
        block,
        local,
        kind: OwnershipErrorKind::UnknownLocal,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::{BasicBlock, Constant, LocalDecl, Statement};

    fn function_with(statements: Vec<Statement>, locals: Vec<Ty>) -> Function {
        let mut function = Function::new("test".to_string(), vec![], Ty::Unit);
        function.locals = locals
            .into_iter()
            .map(|ty| LocalDecl { ty, name: None })
            .collect();
        if let Some(first) = function.locals.first() {
            function.params.push((Local(0), first.ty.clone()));
        }
        let mut block = BasicBlock::new(BlockId(0));
        block.stmts = statements;
        block.terminator = Some(Terminator::Return(None));
        function.blocks.push(block);
        function
    }

    fn analyze_one(function: Function) -> OwnershipReport {
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);
        analyze(&program)
    }

    #[test]
    fn copy_of_copy_type_is_valid() {
        let function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Copy(Local(0)))),
            }],
            vec![Ty::Int, Ty::Int],
        );
        let report = analyze_one(function);
        assert!(report.errors.is_empty());
        assert!(report.is_ready_for_enforcement());
    }

    #[test]
    fn copy_of_owned_type_is_rejected() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Copy(Local(0)))),
            }],
            vec![list_ty.clone(), list_ty.clone()],
        );
        let report = analyze_one(function);
        assert!(matches!(
            report.errors.as_slice(),
            [OwnershipError {
                kind: OwnershipErrorKind::CopyOfNonCopy { .. },
                ..
            }]
        ));
    }

    #[test]
    fn use_after_explicit_move_is_rejected() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Move(Local(0)))),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Move(Local(0)))),
                },
            ],
            vec![list_ty.clone(), list_ty.clone(), list_ty],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| error.kind == OwnershipErrorKind::UseAfterMove)
        );
    }

    #[test]
    fn assignment_reinitializes_a_moved_local() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Move(Local(0)))),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(0),
                        Rvalue::Array(vec![Operand::Constant(Constant::Int(1))]),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Move(Local(0)))),
                },
            ],
            vec![list_ty.clone(), list_ty.clone(), list_ty],
        );
        let report = analyze_one(function);
        assert!(report.errors.is_empty());
    }

    #[test]
    fn legacy_reads_are_reported_for_migration() {
        let function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Local(Local(0)))),
            }],
            vec![Ty::Int, Ty::Int],
        );
        let report = analyze_one(function);
        assert!(report.errors.is_empty());
        assert_eq!(report.legacy_local_reads, 1);
        assert!(!report.is_ready_for_enforcement());
    }

    #[test]
    fn move_state_flows_across_basic_blocks() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let mut function = Function::new("test".to_string(), vec![], Ty::Unit);
        function.locals = vec![list_ty.clone(), list_ty.clone(), list_ty.clone()]
            .into_iter()
            .map(|ty| LocalDecl { ty, name: None })
            .collect();
        function.params.push((Local(0), list_ty.clone()));

        let mut first = BasicBlock::new(BlockId(0));
        first.stmts.push(Statement {
            kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Move(Local(0)))),
        });
        first.terminator = Some(Terminator::Goto(BlockId(1)));
        let mut second = BasicBlock::new(BlockId(1));
        second.stmts.push(Statement {
            kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Move(Local(0)))),
        });
        second.terminator = Some(Terminator::Return(None));
        function.blocks = vec![first, second];

        let report = analyze_one(function);
        assert!(report.errors.iter().any(|error| {
            error.block == BlockId(1) && error.kind == OwnershipErrorKind::UseAfterMove
        }));
    }

    #[test]
    fn normalization_uses_borrow_for_reference_parameters() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let mut function = function_with(vec![], vec![list_ty]);
        function.blocks[0].terminator = Some(Terminator::Call {
            func: "inspect".to_string(),
            args: vec![Operand::Local(Local(0))],
            arg_pass_modes: vec![PassMode::Ref],
            dest: None,
            next: BlockId(0),
        });
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);

        make_operands_explicit(&mut program);
        let terminator = program.functions["test"].blocks[0]
            .terminator
            .as_ref()
            .unwrap();
        assert!(matches!(
            terminator,
            Terminator::Call {
                args,
                ..
            } if matches!(args.as_slice(), [Operand::Borrow(Local(0), Mutability::Immutable)])
        ));
    }

    #[test]
    fn normalization_borrows_projections_of_reference_parameters() {
        // FORGE-RUST-GAP: FRG-001. Reading an owned field through `ref state`
        // must not partially move or schedule projected drops for `state`.
        let state_ty = Ty::Named(crate::types::TypeId::new("WorkflowState"), vec![]);
        let mut function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(
                    Local(1),
                    Rvalue::Use(Operand::MovePlace(Place::new(Local(0)).field("status"))),
                ),
            }],
            vec![state_ty, Ty::Str],
        );
        function.param_pass_modes = vec![PassMode::Ref];
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);

        make_operands_explicit(&mut program);

        assert!(matches!(
            &program.functions["test"].blocks[0].stmts[0].kind,
            StatementKind::Assign(
                Local(1),
                Rvalue::Use(Operand::BorrowPlace(place, Mutability::Immutable))
            ) if place == &Place::new(Local(0)).field("status")
        ));
    }

    #[test]
    fn mutable_loan_conflicts_with_live_shared_loan() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let shared_ref = Ty::Ref(
            Box::new(list_ty.clone()),
            crate::types::Mutability::Immutable,
        );
        let mutable_ref = Ty::Ref(Box::new(list_ty.clone()), crate::types::Mutability::Mutable);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Ref(Local(0), Mutability::Immutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(2),
                        Rvalue::Ref(Local(0), Mutability::Mutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(3), Rvalue::Use(Operand::Copy(Local(1)))),
                },
            ],
            vec![list_ty, shared_ref.clone(), mutable_ref, shared_ref],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::ConflictingLoan { .. }))
        );
    }

    #[test]
    fn shared_loan_ends_after_its_last_use() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let shared_ref = Ty::Ref(
            Box::new(list_ty.clone()),
            crate::types::Mutability::Immutable,
        );
        let mutable_ref = Ty::Ref(Box::new(list_ty.clone()), crate::types::Mutability::Mutable);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Ref(Local(0), Mutability::Immutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Copy(Local(1)))),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(3),
                        Rvalue::Ref(Local(0), Mutability::Mutable),
                    ),
                },
            ],
            vec![list_ty, shared_ref.clone(), shared_ref, mutable_ref],
        );
        let report = analyze_one(function);
        assert!(
            !report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::ConflictingLoan { .. }))
        );
    }

    #[test]
    fn move_while_reference_remains_live_is_rejected() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let shared_ref = Ty::Ref(
            Box::new(list_ty.clone()),
            crate::types::Mutability::Immutable,
        );
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Ref(Local(0), Mutability::Immutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Move(Local(0)))),
                },
                Statement {
                    kind: StatementKind::Assign(Local(3), Rvalue::Use(Operand::Copy(Local(1)))),
                },
            ],
            vec![list_ty.clone(), shared_ref.clone(), list_ty, shared_ref],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| error.kind == OwnershipErrorKind::MoveWhileBorrowed)
        );
    }

    #[test]
    fn use_of_never_initialized_local_is_rejected() {
        let mut function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Copy(Local(1)))),
            }],
            vec![Ty::Int, Ty::Int, Ty::Int],
        );
        function.params.clear();
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| error.kind == OwnershipErrorKind::UseOfUninitialized)
        );
    }

    #[test]
    fn double_drop_is_rejected() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Drop(Local(0)),
                },
                Statement {
                    kind: StatementKind::Drop(Local(0)),
                },
            ],
            vec![list_ty],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| error.kind == OwnershipErrorKind::DoubleDrop)
        );
    }

    #[test]
    fn drop_elaboration_skips_moved_values() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(Local(1), Rvalue::Use(Operand::Move(Local(0)))),
            }],
            vec![list_ty.clone(), list_ty],
        );
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);
        elaborate_drops(&mut program);

        let statements = &program.functions["test"].blocks[0].stmts;
        assert!(matches!(
            statements.last().map(|statement| &statement.kind),
            Some(StatementKind::Drop(Local(1)))
        ));
        assert!(
            !statements
                .iter()
                .any(|statement| matches!(statement.kind, StatementKind::Drop(Local(0))))
        );
    }

    #[test]
    fn returned_owned_value_is_not_dropped() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let mut function = function_with(vec![], vec![list_ty]);
        function.return_ty = function.locals[0].ty.clone();
        function.blocks[0].terminator = Some(Terminator::Return(Some(Operand::Move(Local(0)))));
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);
        elaborate_drops(&mut program);

        assert!(
            !program.functions["test"].blocks[0]
                .stmts
                .iter()
                .any(|statement| matches!(statement.kind, StatementKind::Drop(_)))
        );
    }

    #[test]
    fn goto_edges_drop_values_that_leave_their_live_region_once() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let mut function = function_with(vec![], vec![list_ty]);
        function.blocks[0].terminator = Some(Terminator::Goto(BlockId(1)));
        let mut exit = BasicBlock::new(BlockId(1));
        exit.terminator = Some(Terminator::Return(None));
        function.blocks.push(exit);
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);

        elaborate_drops(&mut program);

        let function = &program.functions["test"];
        let Some(Terminator::Goto(cleanup_id)) = function.blocks[0].terminator.as_ref() else {
            panic!("edge was not redirected through cleanup");
        };
        assert_ne!(*cleanup_id, BlockId(1));
        assert!(matches!(
            function.blocks[cleanup_id.0 as usize].stmts.as_slice(),
            [Statement {
                kind: StatementKind::Drop(Local(0))
            }]
        ));
        assert!(function.blocks[1].stmts.is_empty());
        assert!(analyze(&program).errors.is_empty());
    }

    #[test]
    fn dead_call_results_are_dropped_after_the_call() {
        let list_ty = Ty::List(Box::new(Ty::Int));
        let mut function = function_with(vec![], vec![list_ty.clone(), list_ty]);
        function.blocks[0].terminator = Some(Terminator::Call {
            func: "identity".into(),
            args: vec![Operand::Move(Local(0))],
            arg_pass_modes: vec![PassMode::Owned],
            dest: Some(Local(1)),
            next: BlockId(1),
        });
        let mut exit = BasicBlock::new(BlockId(1));
        exit.terminator = Some(Terminator::Return(None));
        function.blocks.push(exit);
        let mut program = Program::new();
        program.functions.insert(function.name.clone(), function);

        elaborate_drops(&mut program);

        let function = &program.functions["test"];
        let Some(Terminator::Call { next, .. }) = function.blocks[0].terminator.as_ref() else {
            panic!("call terminator changed kind");
        };
        assert_ne!(*next, BlockId(1));
        assert!(matches!(
            function.blocks[next.0 as usize].stmts.as_slice(),
            [Statement {
                kind: StatementKind::Drop(Local(1))
            }]
        ));
        assert!(analyze(&program).errors.is_empty());
    }

    #[test]
    fn spawned_closure_cannot_capture_a_reference() {
        let reference = Ty::Ref(Box::new(Ty::Int), crate::types::Mutability::Immutable);
        let mut function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(
                    Local(1),
                    Rvalue::Closure {
                        func_name: "closure".into(),
                        captures: vec![Operand::Copy(Local(0))],
                    },
                ),
            }],
            vec![
                reference,
                Ty::Fn(vec![], Box::new(Ty::Unit)),
                Ty::Task(Box::new(Ty::Unit)),
            ],
        );
        function.blocks[0].terminator = Some(Terminator::Spawn {
            expr: Operand::Move(Local(1)),
            dest: Some(Local(2)),
            next: BlockId(1),
        });
        let mut exit = BasicBlock::new(BlockId(1));
        exit.terminator = Some(Terminator::Return(None));
        function.blocks.push(exit);

        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| error.kind == OwnershipErrorKind::ReferenceAcrossTask)
        );
    }

    #[test]
    fn spawned_closure_requires_send_captures() {
        let mut function = function_with(
            vec![Statement {
                kind: StatementKind::Assign(
                    Local(1),
                    Rvalue::Closure {
                        func_name: "closure".into(),
                        captures: vec![Operand::Move(Local(0))],
                    },
                ),
            }],
            vec![
                Ty::Database,
                Ty::Fn(vec![], Box::new(Ty::Unit)),
                Ty::Task(Box::new(Ty::Unit)),
            ],
        );
        function.blocks[0].terminator = Some(Terminator::Spawn {
            expr: Operand::Move(Local(1)),
            dest: Some(Local(2)),
            next: BlockId(1),
        });
        let mut exit = BasicBlock::new(BlockId(1));
        exit.terminator = Some(Terminator::Return(None));
        function.blocks.push(exit);

        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::NonSendAcrossTask { .. }))
        );
    }

    #[test]
    fn sibling_projection_remains_usable_after_partial_move() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Use(Operand::MovePlace(Place::new(Local(0)).field("left"))),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(2),
                        Rvalue::Use(Operand::MovePlace(Place::new(Local(0)).field("right"))),
                    ),
                },
            ],
            vec![pair_ty.clone(), pair_ty.clone(), pair_ty],
        );
        let report = analyze_one(function);
        assert!(
            !report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::UseOfPartiallyMoved { .. }))
        );
    }

    #[test]
    fn whole_aggregate_is_unavailable_after_field_move() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Use(Operand::MovePlace(Place::new(Local(0)).field("left"))),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::Move(Local(0)))),
                },
            ],
            vec![pair_ty.clone(), pair_ty.clone(), pair_ty],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::UseOfPartiallyMoved { .. }))
        );
    }

    #[test]
    fn dropping_partially_moved_aggregate_is_rejected() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Use(Operand::MovePlace(Place::new(Local(0)).field("left"))),
                    ),
                },
                Statement {
                    kind: StatementKind::Drop(Local(0)),
                },
            ],
            vec![pair_ty.clone(), pair_ty],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::DropOfPartiallyMoved { .. }))
        );
    }

    #[test]
    fn mutable_loans_of_disjoint_fields_do_not_conflict() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let string_ref = Ty::Ref(Box::new(Ty::Str), crate::types::Mutability::Mutable);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::RefPlace(Place::new(Local(0)).field("left"), Mutability::Mutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(2),
                        Rvalue::RefPlace(Place::new(Local(0)).field("right"), Mutability::Mutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(3), Rvalue::Use(Operand::Copy(Local(1)))),
                },
                Statement {
                    kind: StatementKind::Assign(Local(4), Rvalue::Use(Operand::Copy(Local(2)))),
                },
            ],
            vec![
                pair_ty,
                string_ref.clone(),
                string_ref.clone(),
                string_ref.clone(),
                string_ref,
            ],
        );
        let report = analyze_one(function);
        assert!(
            !report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::ConflictingLoan { .. }))
        );
    }

    #[test]
    fn root_and_field_loans_conflict() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let pair_ref = Ty::Ref(
            Box::new(pair_ty.clone()),
            crate::types::Mutability::Immutable,
        );
        let string_ref = Ty::Ref(Box::new(Ty::Str), crate::types::Mutability::Mutable);
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::RefPlace(Place::new(Local(0)), Mutability::Immutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(
                        Local(2),
                        Rvalue::RefPlace(Place::new(Local(0)).field("left"), Mutability::Mutable),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(3), Rvalue::Use(Operand::Copy(Local(1)))),
                },
            ],
            vec![pair_ty, pair_ref.clone(), string_ref, pair_ref],
        );
        let report = analyze_one(function);
        assert!(
            report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::ConflictingLoan { .. }))
        );
    }

    #[test]
    fn dynamic_index_places_conservatively_overlap() {
        assert!(
            Place::new(Local(0))
                .index(Local(1))
                .overlaps(&Place::new(Local(0)).index(Local(2)))
        );
    }

    #[test]
    fn assigning_a_moved_field_reinitializes_that_field() {
        let pair_ty = Ty::Named(crate::types::TypeId::new("Pair"), vec![]);
        let left = Place::new(Local(0)).field("left");
        let function = function_with(
            vec![
                Statement {
                    kind: StatementKind::Assign(
                        Local(1),
                        Rvalue::Use(Operand::MovePlace(left.clone())),
                    ),
                },
                Statement {
                    kind: StatementKind::AssignPlace(
                        left.clone(),
                        Rvalue::Use(Operand::Constant(Constant::Str("replacement".into()))),
                    ),
                },
                Statement {
                    kind: StatementKind::Assign(Local(2), Rvalue::Use(Operand::MovePlace(left))),
                },
            ],
            vec![pair_ty, Ty::Str, Ty::Str],
        );
        let report = analyze_one(function);
        assert!(
            !report
                .errors
                .iter()
                .any(|error| matches!(error.kind, OwnershipErrorKind::UseOfPartiallyMoved { .. }))
        );
    }
}
