//! Code formatter for FORMA.
//!
//! This module provides a pretty-printer that formats FORMA source code
//! with consistent indentation and spacing.

use crate::parser::*;

/// A code formatter for FORMA source files.
pub struct Formatter {
    output: String,
    indent: usize,
    indent_size: usize,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            indent_size: 4,
        }
    }

    /// Format a source file and return the formatted output.
    pub fn format(&mut self, source: &SourceFile) -> String {
        self.output.clear();
        self.indent = 0;

        for (i, item) in source.items.iter().enumerate() {
            if i > 0 {
                self.output.push('\n');
            }
            self.format_item(item);
        }

        self.output.clone()
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..(self.indent * self.indent_size) {
            self.output.push(' ');
        }
    }

    fn format_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => self.format_function(f),
            ItemKind::Struct(s) => self.format_struct(s),
            ItemKind::Enum(e) => self.format_enum(e),
            ItemKind::Trait(t) => {
                self.write_indent();
                self.write("t ");
                self.write(&t.name.name);
                self.newline();
            }
            ItemKind::Impl(i) => {
                self.write_indent();
                self.write("impl ");
                self.format_type(&i.self_type);
                self.newline();
            }
            ItemKind::TypeAlias(t) => {
                self.write_indent();
                self.write("type ");
                self.write(&t.name.name);
                self.newline();
            }
            ItemKind::Use(_) => {
                self.write_indent();
                self.write("us ...");
                self.newline();
            }
            ItemKind::Module(m) => {
                self.write_indent();
                self.write("md ");
                self.write(&m.name.name);
                self.newline();
            }
            ItemKind::Const(c) => {
                self.write_indent();
                self.write(&c.name.name);
                self.write(" :: ");
                self.format_expr(&c.value);
                self.newline();
            }
        }
    }

    fn format_function(&mut self, f: &Function) {
        self.write_indent();

        if f.visibility == Visibility::Public {
            self.write("pub ");
        }

        if f.is_async {
            self.write("as ");
        }

        self.write("f ");
        self.write(&f.name.name);
        self.write("(");

        for (i, param) in f.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name.name);
            self.write(": ");
            self.format_type(&param.ty);
        }

        self.write(")");

        if let Some(ref ret) = f.return_type {
            self.write(" -> ");
            self.format_type(ret);
        }

        match &f.body {
            Some(FnBody::Expr(e)) => {
                self.write(" = ");
                self.format_expr(e);
                self.newline();
            }
            Some(FnBody::Block(b)) => {
                self.newline();
                self.indent += 1;
                for stmt in &b.stmts {
                    self.format_stmt(stmt);
                }
                self.indent -= 1;
            }
            None => {
                self.newline();
            }
        }
    }

    fn format_struct(&mut self, s: &Struct) {
        self.write_indent();
        if s.visibility == Visibility::Public {
            self.write("pub ");
        }
        self.write("s ");
        self.write(&s.name.name);

        match &s.kind {
            StructKind::Named(fields) => {
                self.newline();
                self.indent += 1;
                for field in fields {
                    self.write_indent();
                    self.write(&field.name.name);
                    self.write(": ");
                    self.format_type(&field.ty);
                    self.newline();
                }
                self.indent -= 1;
            }
            StructKind::Tuple(types) => {
                self.write("(");
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(t);
                }
                self.write(")");
                self.newline();
            }
            StructKind::Unit => {
                self.newline();
            }
        }
    }

    fn format_enum(&mut self, e: &Enum) {
        self.write_indent();
        if e.visibility == Visibility::Public {
            self.write("pub ");
        }
        self.write("e ");
        self.write(&e.name.name);
        self.newline();
        self.indent += 1;

        for variant in &e.variants {
            self.write_indent();
            self.write(&variant.name.name);
            self.newline();
        }

        self.indent -= 1;
    }

    fn format_type(&mut self, ty: &Type) {
        match &ty.kind {
            TypeKind::Path(p) => {
                for (i, seg) in p.segments.iter().enumerate() {
                    if i > 0 {
                        self.write("::");
                    }
                    self.write(&seg.name.name);
                }
            }
            TypeKind::Tuple(types) => {
                self.write("(");
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(t);
                }
                self.write(")");
            }
            TypeKind::List(elem) => {
                self.write("[");
                self.format_type(elem);
                self.write("]");
            }
            TypeKind::Option(inner) => {
                self.format_type(inner);
                self.write("?");
            }
            TypeKind::Result(ok, _) => {
                self.format_type(ok);
                self.write("!");
            }
            TypeKind::Ref(inner, is_mut) => {
                self.write("&");
                if *is_mut {
                    self.write("mut ");
                }
                self.format_type(inner);
            }
            TypeKind::Infer => self.write("_"),
            TypeKind::Never => self.write("!"),
            _ => self.write("..."),
        }
    }

    fn format_stmt(&mut self, stmt: &Stmt) {
        self.write_indent();
        match &stmt.kind {
            StmtKind::Let(let_stmt) => {
                self.format_pattern(&let_stmt.pattern);
                self.write(" := ");
                self.format_expr(&let_stmt.init);
            }
            StmtKind::Expr(expr) => {
                self.format_expr(expr);
            }
            StmtKind::Item(item) => {
                self.format_item(item);
                return;
            }
            StmtKind::Empty => return,
        }
        self.newline();
    }

    fn format_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(lit) => self.format_literal(lit),
            ExprKind::Ident(id) => self.write(&id.name),
            ExprKind::Binary(left, op, right) => {
                self.format_expr(left);
                self.write(" ");
                self.format_binop(op);
                self.write(" ");
                self.format_expr(right);
            }
            ExprKind::Unary(op, e) => {
                self.format_unaryop(op);
                self.format_expr(e);
            }
            ExprKind::Call(func, args) => {
                self.format_expr(func);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(&arg.value);
                }
                self.write(")");
            }
            ExprKind::If(if_expr) => {
                self.write("if ");
                self.format_expr(&if_expr.condition);
                match &if_expr.then_branch {
                    IfBranch::Expr(e) => {
                        self.write(" then ");
                        self.format_expr(e);
                    }
                    IfBranch::Block(b) => {
                        self.newline();
                        self.indent += 1;
                        for stmt in &b.stmts {
                            self.format_stmt(stmt);
                        }
                        self.indent -= 1;
                    }
                }
            }
            ExprKind::Range(start, end, inclusive) => {
                if let Some(s) = start {
                    self.format_expr(s);
                }
                if *inclusive {
                    self.write("..=");
                } else {
                    self.write("..");
                }
                if let Some(e) = end {
                    self.format_expr(e);
                }
            }
            ExprKind::Return(e) => {
                self.write("ret");
                if let Some(val) = e {
                    self.write(" ");
                    self.format_expr(val);
                }
            }
            ExprKind::Tuple(exprs) => {
                self.write("(");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(e);
                }
                self.write(")");
            }
            ExprKind::Array(exprs) => {
                self.write("[");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(e);
                }
                self.write("]");
            }
            ExprKind::Paren(e) => {
                self.write("(");
                self.format_expr(e);
                self.write(")");
            }
            ExprKind::Assign(target, value, is_init) => {
                self.format_expr(target);
                if *is_init {
                    self.write(" := ");
                } else {
                    self.write(" = ");
                }
                self.format_expr(value);
            }
            _ => self.write("..."),
        }
    }

    fn format_literal(&mut self, lit: &Literal) {
        match &lit.kind {
            LiteralKind::Int(n) => self.write(&n.to_string()),
            LiteralKind::Float(f) => self.write(&f.to_string()),
            LiteralKind::String(s) => {
                self.write("\"");
                self.write(s);
                self.write("\"");
            }
            LiteralKind::Char(c) => {
                self.write("'");
                self.output.push(*c);
                self.write("'");
            }
            LiteralKind::Bool(b) => self.write(if *b { "true" } else { "false" }),
            LiteralKind::None => self.write("none"),
        }
    }

    fn format_binop(&mut self, op: &BinOp) {
        let s = match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            _ => "?",
        };
        self.write(s);
    }

    fn format_unaryop(&mut self, op: &UnaryOp) {
        let s = match op {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::Ref => "&",
            UnaryOp::RefMut => "&mut ",
            UnaryOp::Deref => "*",
        };
        self.write(s);
    }

    fn format_pattern(&mut self, pat: &Pattern) {
        match &pat.kind {
            PatternKind::Wildcard => self.write("_"),
            PatternKind::Ident(id, is_mut, _) => {
                if *is_mut {
                    self.write("mut ");
                }
                self.write(&id.name);
            }
            PatternKind::Literal(lit) => self.format_literal(lit),
            PatternKind::Tuple(pats) => {
                self.write("(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_pattern(p);
                }
                self.write(")");
            }
            _ => self.write("..."),
        }
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}
