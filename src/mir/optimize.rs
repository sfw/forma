//! MIR optimization passes.
//!
//! Runs between lowering and interpretation/codegen. Four passes run in rounds
//! to a fixed point (or max 3 rounds):
//!
//! 1. **Constant folding** — evaluate constant expressions at compile time
//! 2. **Copy propagation** — replace uses of a copy with the original
//! 3. **Dead block elimination** — remove unreachable blocks, simplify constant branches
//! 4. **Peephole optimizations** — local pattern replacements within a block

use std::collections::{HashMap, HashSet};

use super::mir::{
    BasicBlock, BinOp, BlockId, Constant, Function, Local, Operand, Program, Rvalue, Statement,
    StatementKind, Terminator, UnOp,
};

/// Statistics from one optimization round.
#[derive(Debug, Default, Clone)]
pub struct OptStats {
    pub constants_folded: usize,
    pub copies_propagated: usize,
    pub dead_blocks_removed: usize,
    pub branches_simplified: usize,
    pub peepholes_applied: usize,
    pub nops_removed: usize,
}

impl OptStats {
    pub fn total(&self) -> usize {
        self.constants_folded
            + self.copies_propagated
            + self.dead_blocks_removed
            + self.branches_simplified
            + self.peepholes_applied
            + self.nops_removed
    }

    fn merge(&mut self, other: &OptStats) {
        self.constants_folded += other.constants_folded;
        self.copies_propagated += other.copies_propagated;
        self.dead_blocks_removed += other.dead_blocks_removed;
        self.branches_simplified += other.branches_simplified;
        self.peepholes_applied += other.peepholes_applied;
        self.nops_removed += other.nops_removed;
    }
}

impl std::fmt::Display for OptStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "constants_folded={}, copies_propagated={}, dead_blocks={}, branches_simplified={}, peepholes={}, nops={}",
            self.constants_folded,
            self.copies_propagated,
            self.dead_blocks_removed,
            self.branches_simplified,
            self.peepholes_applied,
            self.nops_removed,
        )
    }
}

/// Run all optimization passes on the program.
///
/// Runs passes in rounds until a fixed point or max 3 rounds.
/// Returns cumulative statistics across all rounds.
pub fn optimize(program: &mut Program) -> OptStats {
    let mut total_stats = OptStats::default();

    for _round in 0..3 {
        let mut round_stats = OptStats::default();

        for func in program.functions.values_mut() {
            let mut s = OptStats::default();
            constant_fold(func, &mut s);
            copy_propagate(func, &mut s);
            dead_block_eliminate(func, &mut s);
            peephole(func, &mut s);
            round_stats.merge(&s);
        }

        total_stats.merge(&round_stats);

        // Fixed point: stop if nothing changed this round
        if round_stats.total() == 0 {
            break;
        }
    }

    total_stats
}

/// Validate MIR invariants after optimization (debug/test use).
///
/// Returns a list of errors. Empty means valid.
pub fn validate_mir(program: &Program) -> Vec<String> {
    let mut errors = Vec::new();

    for (name, func) in &program.functions {
        let num_blocks = func.blocks.len();

        // entry_block is in-bounds
        if func.entry_block.0 as usize >= num_blocks {
            errors.push(format!(
                "{}: entry_block {} out of bounds (have {} blocks)",
                name, func.entry_block, num_blocks
            ));
        }

        for (i, block) in func.blocks.iter().enumerate() {
            // Every block has a terminator
            if block.terminator.is_none() {
                errors.push(format!("{}: block {} has no terminator", name, i));
                continue;
            }

            // All referenced BlockIds are in-bounds
            let check_bid = |bid: &BlockId, ctx: &str| {
                if bid.0 as usize >= num_blocks {
                    Some(format!(
                        "{}: block {} terminator {} references out-of-bounds block {}",
                        name, i, ctx, bid
                    ))
                } else {
                    None
                }
            };

            if let Some(ref term) = block.terminator {
                match term {
                    Terminator::Return(_) | Terminator::Unreachable => {}
                    Terminator::Goto(bid) => {
                        if let Some(e) = check_bid(bid, "Goto") {
                            errors.push(e);
                        }
                    }
                    Terminator::If {
                        then_block,
                        else_block,
                        ..
                    } => {
                        if let Some(e) = check_bid(then_block, "If.then") {
                            errors.push(e);
                        }
                        if let Some(e) = check_bid(else_block, "If.else") {
                            errors.push(e);
                        }
                    }
                    Terminator::Switch {
                        targets, default, ..
                    } => {
                        for (_, bid) in targets {
                            if let Some(e) = check_bid(bid, "Switch.target") {
                                errors.push(e);
                            }
                        }
                        if let Some(e) = check_bid(default, "Switch.default") {
                            errors.push(e);
                        }
                    }
                    Terminator::Call { next, .. } => {
                        if let Some(e) = check_bid(next, "Call.next") {
                            errors.push(e);
                        }
                    }
                    Terminator::CallIndirect { next, .. } => {
                        if let Some(e) = check_bid(next, "CallIndirect.next") {
                            errors.push(e);
                        }
                    }
                    Terminator::Spawn { next, .. } => {
                        if let Some(e) = check_bid(next, "Spawn.next") {
                            errors.push(e);
                        }
                    }
                    Terminator::Await { next, .. } => {
                        if let Some(e) = check_bid(next, "Await.next") {
                            errors.push(e);
                        }
                    }
                }
            }
        }
    }

    errors
}

// ---------------------------------------------------------------------------
// Pass 1: Constant Folding
// ---------------------------------------------------------------------------

/// Evaluate constant expressions at compile time.
fn constant_fold(func: &mut Function, stats: &mut OptStats) {
    for block in &mut func.blocks {
        for stmt in &mut block.stmts {
            if let StatementKind::Assign(local, rvalue) = &stmt.kind
                && let Some(c) = try_fold_rvalue(rvalue)
            {
                stmt.kind = StatementKind::Assign(*local, Rvalue::Use(Operand::Constant(c)));
                stats.constants_folded += 1;
            }
        }

        // Also simplify constant-condition If → Goto and Switch with known discriminant
        if let Some(ref term) = block.terminator {
            match term {
                Terminator::If {
                    cond: Operand::Constant(Constant::Bool(val)),
                    then_block,
                    else_block,
                } => {
                    let target = if *val { *then_block } else { *else_block };
                    block.terminator = Some(Terminator::Goto(target));
                    stats.branches_simplified += 1;
                }
                Terminator::Switch {
                    operand: Operand::Constant(Constant::Int(val)),
                    targets,
                    default,
                } => {
                    let target = targets
                        .iter()
                        .find(|(v, _)| *v == *val)
                        .map(|(_, bid)| *bid)
                        .unwrap_or(*default);
                    block.terminator = Some(Terminator::Goto(target));
                    stats.branches_simplified += 1;
                }
                _ => {}
            }
        }
    }
}

/// Try to fold a constant rvalue. Returns Some(result) if foldable.
fn try_fold_rvalue(rvalue: &Rvalue) -> Option<Constant> {
    match rvalue {
        Rvalue::BinaryOp(op, left, right) => {
            let lc = operand_as_constant(left)?;
            let rc = operand_as_constant(right)?;
            fold_binop(*op, lc, rc)
        }
        Rvalue::UnaryOp(op, operand) => {
            let c = operand_as_constant(operand)?;
            fold_unop(*op, c)
        }
        _ => None,
    }
}

fn operand_as_constant(op: &Operand) -> Option<&Constant> {
    match op {
        Operand::Constant(c) => Some(c),
        _ => None,
    }
}

fn fold_binop(op: BinOp, left: &Constant, right: &Constant) -> Option<Constant> {
    match (left, right) {
        // Integer arithmetic
        (Constant::Int(a), Constant::Int(b)) => match op {
            BinOp::Add => a.checked_add(*b).map(Constant::Int),
            BinOp::Sub => a.checked_sub(*b).map(Constant::Int),
            BinOp::Mul => a.checked_mul(*b).map(Constant::Int),
            BinOp::Div => {
                if *b == 0 {
                    None // leave div-by-zero as runtime
                } else {
                    a.checked_div(*b).map(Constant::Int)
                }
            }
            BinOp::Rem => {
                if *b == 0 {
                    None
                } else {
                    a.checked_rem(*b).map(Constant::Int)
                }
            }
            BinOp::Eq => Some(Constant::Bool(a == b)),
            BinOp::Ne => Some(Constant::Bool(a != b)),
            BinOp::Lt => Some(Constant::Bool(a < b)),
            BinOp::Le => Some(Constant::Bool(a <= b)),
            BinOp::Gt => Some(Constant::Bool(a > b)),
            BinOp::Ge => Some(Constant::Bool(a >= b)),
            BinOp::BitAnd => Some(Constant::Int(a & b)),
            BinOp::BitOr => Some(Constant::Int(a | b)),
            BinOp::BitXor => Some(Constant::Int(a ^ b)),
            BinOp::Shl => {
                if *b < 0 || *b >= 64 {
                    None // invalid shift
                } else {
                    Some(Constant::Int(a << b))
                }
            }
            BinOp::Shr => {
                if *b < 0 || *b >= 64 {
                    None
                } else {
                    Some(Constant::Int(a >> b))
                }
            }
            _ => None,
        },
        // Float arithmetic
        (Constant::Float(a), Constant::Float(b)) => match op {
            BinOp::Add => Some(Constant::Float(a + b)),
            BinOp::Sub => Some(Constant::Float(a - b)),
            BinOp::Mul => Some(Constant::Float(a * b)),
            BinOp::Div => {
                if *b == 0.0 {
                    None
                } else {
                    Some(Constant::Float(a / b))
                }
            }
            BinOp::Eq => Some(Constant::Bool(a == b)),
            BinOp::Ne => Some(Constant::Bool(a != b)),
            BinOp::Lt => Some(Constant::Bool(a < b)),
            BinOp::Le => Some(Constant::Bool(a <= b)),
            BinOp::Gt => Some(Constant::Bool(a > b)),
            BinOp::Ge => Some(Constant::Bool(a >= b)),
            _ => None,
        },
        // Boolean logic
        (Constant::Bool(a), Constant::Bool(b)) => match op {
            BinOp::And => Some(Constant::Bool(*a && *b)),
            BinOp::Or => Some(Constant::Bool(*a || *b)),
            BinOp::Eq => Some(Constant::Bool(a == b)),
            BinOp::Ne => Some(Constant::Bool(a != b)),
            _ => None,
        },
        // String comparison
        (Constant::Str(a), Constant::Str(b)) => match op {
            BinOp::Eq => Some(Constant::Bool(a == b)),
            BinOp::Ne => Some(Constant::Bool(a != b)),
            _ => None,
        },
        // Char comparison
        (Constant::Char(a), Constant::Char(b)) => match op {
            BinOp::Eq => Some(Constant::Bool(a == b)),
            BinOp::Ne => Some(Constant::Bool(a != b)),
            BinOp::Lt => Some(Constant::Bool(a < b)),
            BinOp::Le => Some(Constant::Bool(a <= b)),
            BinOp::Gt => Some(Constant::Bool(a > b)),
            BinOp::Ge => Some(Constant::Bool(a >= b)),
            _ => None,
        },
        _ => None,
    }
}

fn fold_unop(op: UnOp, c: &Constant) -> Option<Constant> {
    match (op, c) {
        (UnOp::Neg, Constant::Int(n)) => n.checked_neg().map(Constant::Int),
        (UnOp::Neg, Constant::Float(n)) => Some(Constant::Float(-n)),
        (UnOp::Not, Constant::Bool(b)) => Some(Constant::Bool(!b)),
        (UnOp::BitNot, Constant::Int(n)) => Some(Constant::Int(!n)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Pass 2: Copy Propagation
// ---------------------------------------------------------------------------

/// Eliminate redundant temporaries by replacing uses of a copy with the original.
///
/// Only propagates compiler temporaries (LocalDecl.name == None) that are
/// assigned exactly once from a simple copy/local/move.
fn copy_propagate(func: &mut Function, stats: &mut OptStats) {
    // Phase 1: Build substitution map.
    // For each Assign(dest, Use(Copy(src)|Local(src)|Move(src))) where dest
    // is a compiler temporary assigned exactly once, record dest → src.

    // First count assignments per local
    let mut assign_count: HashMap<Local, usize> = HashMap::new();
    for block in &func.blocks {
        for stmt in &block.stmts {
            if let StatementKind::Assign(dest, _) = &stmt.kind {
                *assign_count.entry(*dest).or_insert(0) += 1;
            }
        }
    }

    // Build raw substitution map
    let mut subst: HashMap<Local, Local> = HashMap::new();
    for block in &func.blocks {
        for stmt in &block.stmts {
            if let StatementKind::Assign(dest, Rvalue::Use(operand)) = &stmt.kind {
                // Only compiler temporaries (no user name)
                let dest_idx = dest.0 as usize;
                if dest_idx >= func.locals.len() {
                    continue;
                }
                if func.locals[dest_idx].name.is_some() {
                    continue;
                }

                // Only singly-assigned
                if assign_count.get(dest).copied().unwrap_or(0) != 1 {
                    continue;
                }

                // Extract source local
                let src = match operand {
                    Operand::Copy(s) | Operand::Local(s) | Operand::Move(s) => *s,
                    _ => continue,
                };

                subst.insert(*dest, src);
            }
        }
    }

    if subst.is_empty() {
        return;
    }

    // Chain substitutions: if _1 → x and _2 → _1, then _2 → x
    let mut changed = true;
    while changed {
        changed = false;
        let keys: Vec<Local> = subst.keys().copied().collect();
        for key in keys {
            let val = subst[&key];
            if let Some(&transitive) = subst.get(&val) {
                // Only update if different from current mapping and avoids self-loops
                if transitive != subst[&key] && transitive != key {
                    subst.insert(key, transitive);
                    changed = true;
                }
            }
        }
    }

    // Phase 2: Apply substitution to all operands in the function.
    let mut count = 0usize;
    for block in &mut func.blocks {
        for stmt in &mut block.stmts {
            count += substitute_stmt(stmt, &subst);
        }
        if let Some(ref mut term) = block.terminator {
            count += substitute_terminator(term, &subst);
        }
    }

    stats.copies_propagated += count;
}

/// Substitute locals in a statement's operands. Returns number of substitutions made.
fn substitute_stmt(stmt: &mut Statement, subst: &HashMap<Local, Local>) -> usize {
    let mut count = 0;
    match &mut stmt.kind {
        StatementKind::Assign(_, rvalue) => {
            count += substitute_rvalue(rvalue, subst);
        }
        StatementKind::IndexAssign(_, index_op, val_op) => {
            count += substitute_operand(index_op, subst);
            count += substitute_operand(val_op, subst);
        }
        StatementKind::Nop => {}
    }
    count
}

fn substitute_rvalue(rvalue: &mut Rvalue, subst: &HashMap<Local, Local>) -> usize {
    let mut count = 0;
    match rvalue {
        Rvalue::Use(op) => count += substitute_operand(op, subst),
        Rvalue::BinaryOp(_, left, right) => {
            count += substitute_operand(left, subst);
            count += substitute_operand(right, subst);
        }
        Rvalue::UnaryOp(_, op) => count += substitute_operand(op, subst),
        Rvalue::Deref(op) => count += substitute_operand(op, subst),
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                count += substitute_operand(op, subst);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                count += substitute_operand(op, subst);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                count += substitute_operand(op, subst);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) | Rvalue::Cast(op, _) => {
            count += substitute_operand(op, subst);
        }
        Rvalue::Index(base, index) => {
            count += substitute_operand(base, subst);
            count += substitute_operand(index, subst);
        }
        Rvalue::Closure { captures, .. } => {
            for op in captures {
                count += substitute_operand(op, subst);
            }
        }
        Rvalue::Ref(_, _) | Rvalue::Discriminant(_) | Rvalue::EnumField(_, _) => {
            // These reference locals directly, not operands — don't substitute
        }
    }
    count
}

fn substitute_operand(op: &mut Operand, subst: &HashMap<Local, Local>) -> usize {
    match op {
        Operand::Copy(local) => {
            if let Some(&replacement) = subst.get(local) {
                *local = replacement;
                return 1;
            }
        }
        Operand::Local(local) => {
            if let Some(&replacement) = subst.get(local) {
                *local = replacement;
                return 1;
            }
        }
        Operand::Move(local) => {
            if let Some(&replacement) = subst.get(local) {
                *local = replacement;
                return 1;
            }
        }
        Operand::Constant(_) => {}
    }
    0
}

fn substitute_terminator(term: &mut Terminator, subst: &HashMap<Local, Local>) -> usize {
    let mut count = 0;
    match term {
        Terminator::Return(Some(op)) => {
            count += substitute_operand(op, subst);
        }
        Terminator::If { cond, .. } => {
            count += substitute_operand(cond, subst);
        }
        Terminator::Switch { operand, .. } => {
            count += substitute_operand(operand, subst);
        }
        Terminator::Call { args, .. } => {
            for arg in args {
                count += substitute_operand(arg, subst);
            }
        }
        Terminator::CallIndirect { callee, args, .. } => {
            count += substitute_operand(callee, subst);
            for arg in args {
                count += substitute_operand(arg, subst);
            }
        }
        Terminator::Spawn { expr, .. } => {
            count += substitute_operand(expr, subst);
        }
        Terminator::Await { task, .. } => {
            count += substitute_operand(task, subst);
        }
        Terminator::Return(None) | Terminator::Goto(_) | Terminator::Unreachable => {}
    }
    count
}

// ---------------------------------------------------------------------------
// Pass 3: Dead Block Elimination
// ---------------------------------------------------------------------------

/// Remove unreachable blocks and remap BlockIds.
fn dead_block_eliminate(func: &mut Function, stats: &mut OptStats) {
    if func.blocks.is_empty() {
        return;
    }

    // Optional: jump-thread Goto chains before reachability
    jump_thread(func);

    // Find reachable blocks from entry
    let mut reachable = HashSet::new();
    let mut worklist = vec![func.entry_block];

    while let Some(bid) = worklist.pop() {
        if !reachable.insert(bid) {
            continue;
        }
        let idx = bid.0 as usize;
        if idx >= func.blocks.len() {
            continue;
        }
        if let Some(ref term) = func.blocks[idx].terminator {
            for succ in terminator_successors(term) {
                if !reachable.contains(&succ) {
                    worklist.push(succ);
                }
            }
        }
    }

    let dead_count = func.blocks.len() - reachable.len();
    if dead_count == 0 {
        return;
    }

    stats.dead_blocks_removed += dead_count;

    // Build remap: old BlockId → new BlockId
    let mut remap: HashMap<BlockId, BlockId> = HashMap::new();
    let mut new_blocks: Vec<BasicBlock> = Vec::with_capacity(reachable.len());
    let mut new_idx = 0u32;

    for (old_idx, block) in func.blocks.iter().enumerate() {
        let old_bid = BlockId(old_idx as u32);
        if reachable.contains(&old_bid) {
            remap.insert(old_bid, BlockId(new_idx));
            new_blocks.push(block.clone());
            new_idx += 1;
        }
    }

    // Remap block IDs in new_blocks
    for block in &mut new_blocks {
        block.id = *remap.get(&block.id).unwrap_or(&block.id);
        if let Some(ref mut term) = block.terminator {
            remap_terminator(term, &remap);
        }
    }

    // Update entry block
    func.entry_block = *remap.get(&func.entry_block).unwrap_or(&func.entry_block);
    func.blocks = new_blocks;
}

/// Jump-thread: replace Goto(bbX) where bbX is empty with Goto(bbY) → Goto(bbY).
fn jump_thread(func: &mut Function) {
    // Build a map of "empty goto" blocks: block with no statements and Goto terminator
    let mut goto_target: HashMap<BlockId, BlockId> = HashMap::new();
    for block in &func.blocks {
        if block.stmts.is_empty()
            && let Some(Terminator::Goto(target)) = &block.terminator
        {
            goto_target.insert(block.id, *target);
        }
    }

    if goto_target.is_empty() {
        return;
    }

    // Resolve chains (bbA → bbB → bbC) with cycle detection
    let resolved: HashMap<BlockId, BlockId> = {
        let mut resolved = HashMap::new();
        for &start in goto_target.keys() {
            let mut current = start;
            let mut visited = HashSet::new();
            while let Some(&next) = goto_target.get(&current) {
                if !visited.insert(current) {
                    break; // cycle
                }
                current = next;
            }
            if current != start {
                resolved.insert(start, current);
            }
        }
        resolved
    };

    if resolved.is_empty() {
        return;
    }

    // Apply the resolved targets to all terminators
    for block in &mut func.blocks {
        if let Some(ref mut term) = block.terminator {
            remap_terminator(term, &resolved);
        }
    }
}

/// Get successor BlockIds from a terminator.
fn terminator_successors(term: &Terminator) -> Vec<BlockId> {
    match term {
        Terminator::Return(_) | Terminator::Unreachable => vec![],
        Terminator::Goto(bid) => vec![*bid],
        Terminator::If {
            then_block,
            else_block,
            ..
        } => vec![*then_block, *else_block],
        Terminator::Switch {
            targets, default, ..
        } => {
            let mut succs: Vec<BlockId> = targets.iter().map(|(_, bid)| *bid).collect();
            succs.push(*default);
            succs
        }
        Terminator::Call { next, .. }
        | Terminator::CallIndirect { next, .. }
        | Terminator::Spawn { next, .. }
        | Terminator::Await { next, .. } => vec![*next],
    }
}

/// Remap BlockIds in a terminator according to the remap table.
fn remap_terminator(term: &mut Terminator, remap: &HashMap<BlockId, BlockId>) {
    match term {
        Terminator::Return(_) | Terminator::Unreachable => {}
        Terminator::Goto(bid) => {
            if let Some(&new_bid) = remap.get(bid) {
                *bid = new_bid;
            }
        }
        Terminator::If {
            then_block,
            else_block,
            ..
        } => {
            if let Some(&new_bid) = remap.get(then_block) {
                *then_block = new_bid;
            }
            if let Some(&new_bid) = remap.get(else_block) {
                *else_block = new_bid;
            }
        }
        Terminator::Switch {
            targets, default, ..
        } => {
            for (_, bid) in targets.iter_mut() {
                if let Some(&new_bid) = remap.get(bid) {
                    *bid = new_bid;
                }
            }
            if let Some(&new_bid) = remap.get(default) {
                *default = new_bid;
            }
        }
        Terminator::Call { next, .. }
        | Terminator::CallIndirect { next, .. }
        | Terminator::Spawn { next, .. }
        | Terminator::Await { next, .. } => {
            if let Some(&new_bid) = remap.get(next) {
                *next = new_bid;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Pass 4: Peephole Optimizations
// ---------------------------------------------------------------------------

/// Local pattern replacements within a single block.
fn peephole(func: &mut Function, stats: &mut OptStats) {
    for block in &mut func.blocks {
        // Nop elimination
        let before = block.stmts.len();
        block
            .stmts
            .retain(|s| !matches!(s.kind, StatementKind::Nop));
        let removed = before - block.stmts.len();
        stats.nops_removed += removed;

        // Identity operations
        for stmt in &mut block.stmts {
            if let StatementKind::Assign(local, rvalue) = &stmt.kind
                && let Some(new_rvalue) = try_identity_fold(rvalue)
            {
                stmt.kind = StatementKind::Assign(*local, new_rvalue);
                stats.peepholes_applied += 1;
            }
        }

        // Double negation: Assign(_1, UnaryOp(Not, _2)); Assign(_3, UnaryOp(Not, Copy(_1)))
        // where _1 is used only in the second statement
        peephole_double_negation(func.locals.as_slice(), &mut block.stmts, stats);

        // Redundant return temp: Assign(_1, Use(op)); ... Return(Copy(_1))
        peephole_return_temp(func.locals.as_slice(), block, stats);
    }
}

/// Try to simplify identity operations.
fn try_identity_fold(rvalue: &Rvalue) -> Option<Rvalue> {
    match rvalue {
        // x + 0 → x, 0 + x → x
        Rvalue::BinaryOp(BinOp::Add, left, right) => {
            if is_int_zero(right) {
                return Some(Rvalue::Use(left.clone()));
            }
            if is_int_zero(left) {
                return Some(Rvalue::Use(right.clone()));
            }
            None
        }
        // x - 0 → x
        Rvalue::BinaryOp(BinOp::Sub, left, right) => {
            if is_int_zero(right) {
                return Some(Rvalue::Use(left.clone()));
            }
            None
        }
        // x * 1 → x, 1 * x → x
        Rvalue::BinaryOp(BinOp::Mul, left, right) => {
            if is_int_one(right) {
                return Some(Rvalue::Use(left.clone()));
            }
            if is_int_one(left) {
                return Some(Rvalue::Use(right.clone()));
            }
            // x * 0 → 0, 0 * x → 0
            if is_int_zero(right) || is_int_zero(left) {
                return Some(Rvalue::Use(Operand::Constant(Constant::Int(0))));
            }
            None
        }
        // x / 1 → x
        Rvalue::BinaryOp(BinOp::Div, left, right) => {
            if is_int_one(right) {
                return Some(Rvalue::Use(left.clone()));
            }
            None
        }
        _ => None,
    }
}

fn is_int_zero(op: &Operand) -> bool {
    matches!(op, Operand::Constant(Constant::Int(0)))
}

fn is_int_one(op: &Operand) -> bool {
    matches!(op, Operand::Constant(Constant::Int(1)))
}

/// Double negation elimination.
///
/// Look for patterns like:
///   Assign(_1, UnaryOp(Not, operand))
///   Assign(_3, UnaryOp(Not, Copy(_1)))  -- where _1 is a temp used only here
/// Replace the second with: Assign(_3, Use(operand))
fn peephole_double_negation(
    locals: &[super::mir::LocalDecl],
    stmts: &mut [Statement],
    stats: &mut OptStats,
) {
    // Build use-count for locals within this block's statements
    let mut use_count: HashMap<Local, usize> = HashMap::new();
    for stmt in stmts.iter() {
        count_operand_uses(&stmt.kind, &mut use_count);
    }

    // Look for the pattern
    let mut i = 0;
    while i + 1 < stmts.len() {
        let matches = {
            if let StatementKind::Assign(dest1, Rvalue::UnaryOp(UnOp::Not, inner_op)) =
                &stmts[i].kind
            {
                // dest1 must be a compiler temporary
                let d1 = dest1.0 as usize;
                if d1 < locals.len() && locals[d1].name.is_none() {
                    if let StatementKind::Assign(
                        _dest2,
                        Rvalue::UnaryOp(UnOp::Not, outer_operand),
                    ) = &stmts[i + 1].kind
                    {
                        // outer_operand must reference dest1
                        let refs_dest1 = match outer_operand {
                            Operand::Copy(l) | Operand::Local(l) | Operand::Move(l) => *l == *dest1,
                            _ => false,
                        };
                        if refs_dest1 && use_count.get(dest1).copied().unwrap_or(0) == 1 {
                            Some(inner_op.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        };

        if let Some(original_operand) = matches
            && let StatementKind::Assign(dest2, _) = &stmts[i + 1].kind
        {
            let d2 = *dest2;
            stmts[i + 1].kind = StatementKind::Assign(d2, Rvalue::Use(original_operand));
            stmts[i].kind = StatementKind::Nop;
            stats.peepholes_applied += 1;
        }
        i += 1;
    }
}

/// Count how many times each local is used as an operand in a statement.
fn count_operand_uses(kind: &StatementKind, counts: &mut HashMap<Local, usize>) {
    match kind {
        StatementKind::Assign(_, rvalue) => count_rvalue_uses(rvalue, counts),
        StatementKind::IndexAssign(_, idx, val) => {
            count_single_use(idx, counts);
            count_single_use(val, counts);
        }
        StatementKind::Nop => {}
    }
}

fn count_rvalue_uses(rvalue: &Rvalue, counts: &mut HashMap<Local, usize>) {
    match rvalue {
        Rvalue::Use(op) | Rvalue::UnaryOp(_, op) | Rvalue::Deref(op) => {
            count_single_use(op, counts)
        }
        Rvalue::BinaryOp(_, l, r) | Rvalue::Index(l, r) => {
            count_single_use(l, counts);
            count_single_use(r, counts);
        }
        Rvalue::Tuple(ops) | Rvalue::Array(ops) => {
            for op in ops {
                count_single_use(op, counts);
            }
        }
        Rvalue::Struct(_, fields) => {
            for (_, op) in fields {
                count_single_use(op, counts);
            }
        }
        Rvalue::Enum { fields, .. } => {
            for op in fields {
                count_single_use(op, counts);
            }
        }
        Rvalue::Field(op, _) | Rvalue::TupleField(op, _) | Rvalue::Cast(op, _) => {
            count_single_use(op, counts)
        }
        Rvalue::Closure { captures, .. } => {
            for op in captures {
                count_single_use(op, counts);
            }
        }
        Rvalue::Ref(_, _) | Rvalue::Discriminant(_) | Rvalue::EnumField(_, _) => {}
    }
}

fn count_single_use(op: &Operand, counts: &mut HashMap<Local, usize>) {
    match op {
        Operand::Copy(l) | Operand::Local(l) | Operand::Move(l) => {
            *counts.entry(*l).or_insert(0) += 1;
        }
        Operand::Constant(_) => {}
    }
}

/// Redundant return temp elimination.
///
/// Pattern: the last statement is Assign(_t, rvalue) and the terminator is
/// Return(Copy(_t)) where _t is a compiler temp. Replace Return operand with
/// the rvalue's operand if it's a simple Use.
fn peephole_return_temp(
    locals: &[super::mir::LocalDecl],
    block: &mut BasicBlock,
    stats: &mut OptStats,
) {
    if block.stmts.is_empty() {
        return;
    }

    let last_idx = block.stmts.len() - 1;

    // Check if terminator is Return(Copy(local)) or Return(Move(local))
    let return_local = match &block.terminator {
        Some(Terminator::Return(Some(Operand::Copy(l) | Operand::Move(l)))) => *l,
        _ => return,
    };

    // Check if last statement assigns to that local
    let (dest, rvalue_is_use) = match &block.stmts[last_idx].kind {
        StatementKind::Assign(dest, Rvalue::Use(op)) if *dest == return_local => {
            (*dest, Some(op.clone()))
        }
        _ => return,
    };

    // dest must be a compiler temporary
    let d = dest.0 as usize;
    if d >= locals.len() || locals[d].name.is_some() {
        return;
    }

    // Replace: Return(Copy(_t)) → Return(op), remove the assignment
    if let Some(op) = rvalue_is_use {
        block.terminator = Some(Terminator::Return(Some(op)));
        block.stmts[last_idx].kind = StatementKind::Nop;
        stats.peepholes_applied += 1;
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::mir::{LocalDecl, Statement};
    use crate::types::Ty;

    /// Helper: make a statement
    fn assign(local: u32, rvalue: Rvalue) -> Statement {
        Statement {
            kind: StatementKind::Assign(Local(local), rvalue),
        }
    }

    fn nop() -> Statement {
        Statement {
            kind: StatementKind::Nop,
        }
    }

    fn make_local(name: Option<&str>) -> LocalDecl {
        LocalDecl {
            ty: Ty::Int,
            name: name.map(|s| s.to_string()),
        }
    }

    fn make_block(id: u32, stmts: Vec<Statement>, term: Terminator) -> BasicBlock {
        BasicBlock {
            id: BlockId(id),
            stmts,
            terminator: Some(term),
        }
    }

    fn make_function(locals: Vec<LocalDecl>, blocks: Vec<BasicBlock>) -> Function {
        Function {
            name: "test".to_string(),
            params: vec![],
            param_names: vec![],
            param_pass_modes: vec![],
            return_ty: Ty::Int,
            locals,
            blocks,
            entry_block: BlockId(0),
            preconditions: vec![],
            postconditions: vec![],
        }
    }

    // ---- Constant Folding ----

    #[test]
    fn test_fold_int_add() {
        let result = fold_binop(BinOp::Add, &Constant::Int(1), &Constant::Int(2));
        assert!(matches!(result, Some(Constant::Int(3))));
    }

    #[test]
    fn test_fold_int_sub() {
        let result = fold_binop(BinOp::Sub, &Constant::Int(10), &Constant::Int(3));
        assert!(matches!(result, Some(Constant::Int(7))));
    }

    #[test]
    fn test_fold_int_mul() {
        let result = fold_binop(BinOp::Mul, &Constant::Int(4), &Constant::Int(5));
        assert!(matches!(result, Some(Constant::Int(20))));
    }

    #[test]
    fn test_fold_int_div() {
        let result = fold_binop(BinOp::Div, &Constant::Int(10), &Constant::Int(3));
        assert!(matches!(result, Some(Constant::Int(3))));
    }

    #[test]
    fn test_fold_div_by_zero_not_folded() {
        let result = fold_binop(BinOp::Div, &Constant::Int(10), &Constant::Int(0));
        assert!(result.is_none());
    }

    #[test]
    fn test_fold_rem_by_zero_not_folded() {
        let result = fold_binop(BinOp::Rem, &Constant::Int(10), &Constant::Int(0));
        assert!(result.is_none());
    }

    #[test]
    fn test_fold_int_comparison() {
        assert!(matches!(
            fold_binop(BinOp::Lt, &Constant::Int(1), &Constant::Int(2)),
            Some(Constant::Bool(true))
        ));
        assert!(matches!(
            fold_binop(BinOp::Ge, &Constant::Int(1), &Constant::Int(2)),
            Some(Constant::Bool(false))
        ));
    }

    #[test]
    fn test_fold_bool_and_or() {
        assert!(matches!(
            fold_binop(BinOp::And, &Constant::Bool(true), &Constant::Bool(false)),
            Some(Constant::Bool(false))
        ));
        assert!(matches!(
            fold_binop(BinOp::Or, &Constant::Bool(false), &Constant::Bool(true)),
            Some(Constant::Bool(true))
        ));
    }

    #[test]
    fn test_fold_unary_neg() {
        assert!(matches!(
            fold_unop(UnOp::Neg, &Constant::Int(5)),
            Some(Constant::Int(-5))
        ));
    }

    #[test]
    fn test_fold_unary_not() {
        assert!(matches!(
            fold_unop(UnOp::Not, &Constant::Bool(true)),
            Some(Constant::Bool(false))
        ));
    }

    #[test]
    fn test_fold_overflow_not_folded() {
        let result = fold_binop(BinOp::Add, &Constant::Int(i64::MAX), &Constant::Int(1));
        assert!(result.is_none());
    }

    #[test]
    fn test_fold_invalid_shift_not_folded() {
        assert!(fold_binop(BinOp::Shl, &Constant::Int(1), &Constant::Int(64)).is_none());
        assert!(fold_binop(BinOp::Shr, &Constant::Int(1), &Constant::Int(-1)).is_none());
    }

    // ---- Constant Folding: full pass on function ----

    #[test]
    fn test_constant_fold_pass() {
        let locals = vec![make_local(None), make_local(None)];
        let stmts = vec![assign(
            0,
            Rvalue::BinaryOp(
                BinOp::Add,
                Operand::Constant(Constant::Int(10)),
                Operand::Constant(Constant::Int(20)),
            ),
        )];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(0)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        constant_fold(&mut func, &mut stats);
        assert_eq!(stats.constants_folded, 1);

        // Check the statement was replaced
        if let StatementKind::Assign(_, Rvalue::Use(Operand::Constant(Constant::Int(30)))) =
            &func.blocks[0].stmts[0].kind
        {
            // good
        } else {
            panic!("Expected folded constant 30");
        }
    }

    #[test]
    fn test_branch_simplification_if() {
        let locals = vec![make_local(None)];
        let blocks = vec![
            make_block(
                0,
                vec![],
                Terminator::If {
                    cond: Operand::Constant(Constant::Bool(true)),
                    then_block: BlockId(1),
                    else_block: BlockId(2),
                },
            ),
            make_block(
                1,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(1)))),
            ),
            make_block(
                2,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(2)))),
            ),
        ];
        let mut func = make_function(locals, blocks);

        let mut stats = OptStats::default();
        constant_fold(&mut func, &mut stats);
        assert_eq!(stats.branches_simplified, 1);
        assert!(matches!(
            func.blocks[0].terminator,
            Some(Terminator::Goto(BlockId(1)))
        ));
    }

    // ---- Copy Propagation ----

    #[test]
    fn test_copy_propagation_simple() {
        // _0 = param; _1 = Copy(_0); return Copy(_1) → return Copy(_0)
        let locals = vec![
            make_local(Some("x")), // _0 = user var
            make_local(None),      // _1 = temp
        ];
        let stmts = vec![assign(1, Rvalue::Use(Operand::Copy(Local(0))))];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(1)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        copy_propagate(&mut func, &mut stats);
        assert!(stats.copies_propagated > 0);

        // The return should now reference _0
        if let Some(Terminator::Return(Some(Operand::Copy(Local(0))))) = &func.blocks[0].terminator
        {
            // good
        } else {
            panic!("Expected return to reference _0 after copy propagation");
        }
    }

    #[test]
    fn test_copy_propagation_chain() {
        // _1 = Copy(_0); _2 = Copy(_1); return Copy(_2) → return Copy(_0)
        let locals = vec![
            make_local(Some("x")), // _0
            make_local(None),      // _1
            make_local(None),      // _2
        ];
        let stmts = vec![
            assign(1, Rvalue::Use(Operand::Copy(Local(0)))),
            assign(2, Rvalue::Use(Operand::Copy(Local(1)))),
        ];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(2)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        copy_propagate(&mut func, &mut stats);
        assert!(stats.copies_propagated > 0);

        if let Some(Terminator::Return(Some(Operand::Copy(Local(0))))) = &func.blocks[0].terminator
        {
            // good
        } else {
            panic!("Expected chain propagation to _0");
        }
    }

    #[test]
    fn test_copy_prop_no_user_vars() {
        // User-named variables should NOT be propagated
        let locals = vec![
            make_local(Some("x")), // _0
            make_local(Some("y")), // _1 — user var, should not propagate
        ];
        let stmts = vec![assign(1, Rvalue::Use(Operand::Copy(Local(0))))];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(1)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        copy_propagate(&mut func, &mut stats);
        // _1 is a user variable, should NOT be substituted
        assert_eq!(stats.copies_propagated, 0);
    }

    // ---- Dead Block Elimination ----

    #[test]
    fn test_dead_block_removal() {
        let locals = vec![make_local(None)];
        let blocks = vec![
            make_block(
                0,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(0)))),
            ),
            make_block(
                1,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(1)))),
            ), // unreachable
        ];
        let mut func = make_function(locals, blocks);

        let mut stats = OptStats::default();
        dead_block_eliminate(&mut func, &mut stats);
        assert_eq!(stats.dead_blocks_removed, 1);
        assert_eq!(func.blocks.len(), 1);
    }

    #[test]
    fn test_dead_block_remap() {
        // bb0 → Goto(bb2), bb1 is dead, bb2 returns
        let locals = vec![make_local(None)];
        let blocks = vec![
            make_block(0, vec![], Terminator::Goto(BlockId(2))),
            make_block(
                1,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(1)))),
            ),
            make_block(
                2,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(2)))),
            ),
        ];
        let mut func = make_function(locals, blocks);

        let mut stats = OptStats::default();
        dead_block_eliminate(&mut func, &mut stats);
        assert_eq!(func.blocks.len(), 2);

        // bb0 should now Goto the remapped bb2 → bb1
        if let Some(Terminator::Goto(bid)) = &func.blocks[0].terminator {
            assert_eq!(bid.0, 1);
        } else {
            panic!("Expected Goto with remapped block ID");
        }
    }

    #[test]
    fn test_jump_threading() {
        // bb0 → Goto(bb1), bb1 → Goto(bb2), bb2 returns
        // After threading: bb0 → Goto(bb2)
        let locals = vec![make_local(None)];
        let blocks = vec![
            make_block(0, vec![], Terminator::Goto(BlockId(1))),
            make_block(1, vec![], Terminator::Goto(BlockId(2))),
            make_block(
                2,
                vec![],
                Terminator::Return(Some(Operand::Constant(Constant::Int(0)))),
            ),
        ];
        let mut func = make_function(locals, blocks);

        jump_thread(&mut func);

        // bb0 should now go directly to bb2
        if let Some(Terminator::Goto(bid)) = &func.blocks[0].terminator {
            assert_eq!(bid.0, 2);
        } else {
            panic!("Expected jump-threaded Goto(bb2)");
        }
    }

    // ---- Peephole ----

    #[test]
    fn test_peephole_nop_removal() {
        let locals = vec![make_local(None)];
        let stmts = vec![
            nop(),
            assign(0, Rvalue::Use(Operand::Constant(Constant::Int(1)))),
            nop(),
        ];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(0)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        peephole(&mut func, &mut stats);
        assert_eq!(stats.nops_removed, 2);
        assert_eq!(func.blocks[0].stmts.len(), 1);
    }

    #[test]
    fn test_peephole_identity_add_zero() {
        let locals = vec![make_local(None), make_local(None)];
        let stmts = vec![assign(
            1,
            Rvalue::BinaryOp(
                BinOp::Add,
                Operand::Copy(Local(0)),
                Operand::Constant(Constant::Int(0)),
            ),
        )];
        // Use a non-temp return so return-temp peephole doesn't also fire
        let block = make_block(
            0,
            stmts,
            Terminator::Return(Some(Operand::Constant(Constant::Int(99)))),
        );
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        peephole(&mut func, &mut stats);
        assert_eq!(stats.peepholes_applied, 1);

        if let StatementKind::Assign(_, Rvalue::Use(Operand::Copy(Local(0)))) =
            &func.blocks[0].stmts[0].kind
        {
            // good
        } else {
            panic!("Expected identity fold x + 0 → x");
        }
    }

    #[test]
    fn test_peephole_identity_mul_one() {
        let locals = vec![make_local(None), make_local(None)];
        let stmts = vec![assign(
            1,
            Rvalue::BinaryOp(
                BinOp::Mul,
                Operand::Constant(Constant::Int(1)),
                Operand::Copy(Local(0)),
            ),
        )];
        let block = make_block(
            0,
            stmts,
            Terminator::Return(Some(Operand::Constant(Constant::Int(99)))),
        );
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        peephole(&mut func, &mut stats);
        assert_eq!(stats.peepholes_applied, 1);

        if let StatementKind::Assign(_, Rvalue::Use(Operand::Copy(Local(0)))) =
            &func.blocks[0].stmts[0].kind
        {
            // good
        } else {
            panic!("Expected identity fold 1 * x → x");
        }
    }

    #[test]
    fn test_peephole_mul_zero() {
        let locals = vec![make_local(None), make_local(None)];
        let stmts = vec![assign(
            1,
            Rvalue::BinaryOp(
                BinOp::Mul,
                Operand::Copy(Local(0)),
                Operand::Constant(Constant::Int(0)),
            ),
        )];
        let block = make_block(
            0,
            stmts,
            Terminator::Return(Some(Operand::Constant(Constant::Int(99)))),
        );
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        peephole(&mut func, &mut stats);
        assert_eq!(stats.peepholes_applied, 1);

        if let StatementKind::Assign(_, Rvalue::Use(Operand::Constant(Constant::Int(0)))) =
            &func.blocks[0].stmts[0].kind
        {
            // good
        } else {
            panic!("Expected x * 0 → 0");
        }
    }

    #[test]
    fn test_peephole_return_temp() {
        // Assign(_1, Use(Constant(42))); Return(Copy(_1)) → Return(Constant(42))
        let locals = vec![make_local(None), make_local(None)];
        let stmts = vec![assign(1, Rvalue::Use(Operand::Constant(Constant::Int(42))))];
        let block = make_block(0, stmts, Terminator::Return(Some(Operand::Copy(Local(1)))));
        let mut func = make_function(locals, vec![block]);

        let mut stats = OptStats::default();
        peephole(&mut func, &mut stats);
        assert!(stats.peepholes_applied > 0);

        if let Some(Terminator::Return(Some(Operand::Constant(Constant::Int(42))))) =
            &func.blocks[0].terminator
        {
            // good
        } else {
            panic!("Expected return temp elimination");
        }
    }

    // ---- Full pipeline ----

    #[test]
    fn test_optimize_idempotent() {
        let locals = vec![make_local(None)];
        let blocks = vec![make_block(
            0,
            vec![assign(
                0,
                Rvalue::BinaryOp(
                    BinOp::Add,
                    Operand::Constant(Constant::Int(1)),
                    Operand::Constant(Constant::Int(2)),
                ),
            )],
            Terminator::Return(Some(Operand::Copy(Local(0)))),
        )];
        let func = make_function(locals, blocks);

        let mut program = Program::new();
        program.functions.insert("test".to_string(), func);

        // First pass should optimize
        let stats1 = optimize(&mut program);
        assert!(stats1.total() > 0);

        // Second pass should be idempotent
        let stats2 = optimize(&mut program);
        assert_eq!(stats2.total(), 0, "Second optimization should be no-op");
    }

    #[test]
    fn test_validate_mir_valid() {
        let locals = vec![make_local(None)];
        let blocks = vec![make_block(
            0,
            vec![],
            Terminator::Return(Some(Operand::Constant(Constant::Int(0)))),
        )];
        let func = make_function(locals, blocks);

        let mut program = Program::new();
        program.functions.insert("test".to_string(), func);

        let errors = validate_mir(&program);
        assert!(
            errors.is_empty(),
            "Valid MIR should have no errors: {:?}",
            errors
        );
    }

    #[test]
    fn test_validate_mir_bad_entry() {
        let locals = vec![make_local(None)];
        let blocks = vec![make_block(
            0,
            vec![],
            Terminator::Return(Some(Operand::Constant(Constant::Int(0)))),
        )];
        let mut func = make_function(locals, blocks);
        func.entry_block = BlockId(99);

        let mut program = Program::new();
        program.functions.insert("test".to_string(), func);

        let errors = validate_mir(&program);
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_validate_mir_bad_target() {
        let locals = vec![make_local(None)];
        let blocks = vec![make_block(0, vec![], Terminator::Goto(BlockId(99)))];
        let func = make_function(locals, blocks);

        let mut program = Program::new();
        program.functions.insert("test".to_string(), func);

        let errors = validate_mir(&program);
        assert!(!errors.is_empty());
    }
}
