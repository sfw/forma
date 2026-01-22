//! Simple interpreter for MIR.
//!
//! This module provides an interpreter that can execute MIR programs
//! for testing and validation purposes.

use std::collections::HashMap;

use super::mir::{
    BinOp, BlockId, Constant, Function, Local, Operand, Program, Rvalue,
    StatementKind, Terminator, UnOp,
};

/// Runtime value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Struct(String, HashMap<String, Value>),
    Ref(Box<Value>),
}

impl Value {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(n) => Some(*n),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Char(c) => write!(f, "'{}'", c),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Tuple(vals) => {
                write!(f, "(")?;
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Array(vals) => {
                write!(f, "[")?;
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Struct(name, fields) => {
                write!(f, "{} {{ ", name)?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, " }}")
            }
            Value::Ref(inner) => write!(f, "&{}", inner),
        }
    }
}

/// Interpreter error.
#[derive(Debug, Clone)]
pub struct InterpError {
    pub message: String,
}

impl std::fmt::Display for InterpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "interpreter error: {}", self.message)
    }
}

impl std::error::Error for InterpError {}

/// Stack frame for function calls.
#[derive(Debug)]
struct Frame {
    function: String,
    locals: HashMap<Local, Value>,
    current_block: BlockId,
}

impl Frame {
    fn new(function: String, entry: BlockId) -> Self {
        Self {
            function,
            locals: HashMap::new(),
            current_block: entry,
        }
    }
}

/// MIR interpreter.
pub struct Interpreter {
    program: Program,
    call_stack: Vec<Frame>,
    max_steps: usize,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            call_stack: Vec::new(),
            max_steps: 1_000_000,
        }
    }

    /// Set the maximum number of steps to prevent infinite loops.
    pub fn with_max_steps(mut self, max: usize) -> Self {
        self.max_steps = max;
        self
    }

    /// Run the program starting from the given function.
    pub fn run(&mut self, fn_name: &str, args: &[Value]) -> Result<Value, InterpError> {
        let func = self
            .program
            .functions
            .get(fn_name)
            .ok_or_else(|| InterpError {
                message: format!("function '{}' not found", fn_name),
            })?
            .clone();

        // Create initial frame
        let mut frame = Frame::new(fn_name.to_string(), func.entry_block);

        // Initialize parameters
        for ((local, _ty), value) in func.params.iter().zip(args.iter()) {
            frame.locals.insert(*local, value.clone());
        }

        self.call_stack.push(frame);

        let result = self.execute(&func)?;

        self.call_stack.pop();

        Ok(result)
    }

    fn execute(&mut self, func: &Function) -> Result<Value, InterpError> {
        let mut steps = 0;

        loop {
            steps += 1;
            if steps > self.max_steps {
                return Err(InterpError {
                    message: "maximum steps exceeded (possible infinite loop)".to_string(),
                });
            }

            let frame = self.call_stack.last_mut().unwrap();
            let block = &func.blocks[frame.current_block.0 as usize];

            // Execute statements
            for stmt in &block.stmts {
                match &stmt.kind {
                    StatementKind::Assign(local, rvalue) => {
                        let value = self.eval_rvalue(rvalue, func)?;
                        let frame = self.call_stack.last_mut().unwrap();
                        frame.locals.insert(*local, value);
                    }
                    StatementKind::Nop => {}
                }
            }

            // Execute terminator
            let terminator = block
                .terminator
                .as_ref()
                .ok_or_else(|| InterpError {
                    message: format!("block {} has no terminator", block.id),
                })?
                .clone();

            match terminator {
                Terminator::Return(op) => {
                    let value = match op {
                        Some(operand) => self.eval_operand(&operand)?,
                        None => Value::Unit,
                    };
                    return Ok(value);
                }

                Terminator::Goto(target) => {
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.current_block = target;
                }

                Terminator::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let cond_val = self.eval_operand(&cond)?;
                    let frame = self.call_stack.last_mut().unwrap();

                    let branch = match cond_val {
                        Value::Bool(true) => then_block,
                        Value::Bool(false) => else_block,
                        _ => {
                            return Err(InterpError {
                                message: "if condition must be boolean".to_string(),
                            })
                        }
                    };
                    frame.current_block = branch;
                }

                Terminator::Switch {
                    operand,
                    targets,
                    default,
                } => {
                    let val = self.eval_operand(&operand)?;
                    let frame = self.call_stack.last_mut().unwrap();

                    let target = match val {
                        Value::Int(n) => {
                            targets
                                .iter()
                                .find(|(v, _)| *v == n)
                                .map(|(_, b)| *b)
                                .unwrap_or(default)
                        }
                        _ => default,
                    };
                    frame.current_block = target;
                }

                Terminator::Call {
                    func: fn_name,
                    args,
                    dest,
                    next,
                } => {
                    // Evaluate arguments
                    let arg_vals: Vec<Value> = args
                        .iter()
                        .map(|a| self.eval_operand(a))
                        .collect::<Result<_, _>>()?;

                    // Handle built-in functions
                    let result = if fn_name == "print" {
                        for (i, val) in arg_vals.iter().enumerate() {
                            if i > 0 {
                                print!(" ");
                            }
                            print!("{}", val);
                        }
                        println!();
                        Value::Unit
                    } else if let Some(callee) = self.program.functions.get(&fn_name).cloned() {
                        // Regular function call
                        let mut callee_frame =
                            Frame::new(fn_name.clone(), callee.entry_block);

                        for ((local, _ty), value) in callee.params.iter().zip(arg_vals.iter()) {
                            callee_frame.locals.insert(*local, value.clone());
                        }

                        self.call_stack.push(callee_frame);
                        let result = self.execute(&callee)?;
                        self.call_stack.pop();
                        result
                    } else {
                        return Err(InterpError {
                            message: format!("undefined function: {}", fn_name),
                        });
                    };

                    // Store result and continue
                    let frame = self.call_stack.last_mut().unwrap();
                    if let Some(d) = dest {
                        frame.locals.insert(d, result);
                    }
                    frame.current_block = next;
                }

                Terminator::Unreachable => {
                    return Err(InterpError {
                        message: "reached unreachable code".to_string(),
                    });
                }
            }
        }
    }

    fn eval_rvalue(&mut self, rvalue: &Rvalue, _func: &Function) -> Result<Value, InterpError> {
        match rvalue {
            Rvalue::Use(op) => self.eval_operand(op),

            Rvalue::BinaryOp(op, left, right) => {
                let l = self.eval_operand(left)?;
                let r = self.eval_operand(right)?;
                self.eval_binop(*op, l, r)
            }

            Rvalue::UnaryOp(op, operand) => {
                let val = self.eval_operand(operand)?;
                self.eval_unop(*op, val)
            }

            Rvalue::Ref(local, _mutability) => {
                let frame = self.call_stack.last().unwrap();
                let val = frame
                    .locals
                    .get(local)
                    .cloned()
                    .unwrap_or(Value::Unit);
                Ok(Value::Ref(Box::new(val)))
            }

            Rvalue::Deref(op) => {
                let val = self.eval_operand(op)?;
                match val {
                    Value::Ref(inner) => Ok(*inner),
                    _ => Err(InterpError {
                        message: "cannot dereference non-reference".to_string(),
                    }),
                }
            }

            Rvalue::Tuple(ops) => {
                let vals: Vec<Value> = ops
                    .iter()
                    .map(|o| self.eval_operand(o))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Tuple(vals))
            }

            Rvalue::Array(ops) => {
                let vals: Vec<Value> = ops
                    .iter()
                    .map(|o| self.eval_operand(o))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Array(vals))
            }

            Rvalue::Struct(name, fields) => {
                let mut map = HashMap::new();
                for (field_name, op) in fields {
                    let val = self.eval_operand(op)?;
                    map.insert(field_name.clone(), val);
                }
                Ok(Value::Struct(name.clone(), map))
            }

            Rvalue::Field(op, field_name) => {
                let val = self.eval_operand(op)?;
                match val {
                    Value::Struct(_, fields) => fields.get(field_name).cloned().ok_or_else(|| {
                        InterpError {
                            message: format!("field '{}' not found", field_name),
                        }
                    }),
                    _ => Err(InterpError {
                        message: "field access on non-struct".to_string(),
                    }),
                }
            }

            Rvalue::TupleField(op, idx) => {
                let val = self.eval_operand(op)?;
                match val {
                    Value::Tuple(vals) => vals.get(*idx).cloned().ok_or_else(|| InterpError {
                        message: format!("tuple index {} out of bounds", idx),
                    }),
                    _ => Err(InterpError {
                        message: "tuple field access on non-tuple".to_string(),
                    }),
                }
            }

            Rvalue::Index(base, idx) => {
                let base_val = self.eval_operand(base)?;
                let idx_val = self.eval_operand(idx)?;

                let index = match idx_val {
                    Value::Int(n) => n as usize,
                    _ => {
                        return Err(InterpError {
                            message: "index must be integer".to_string(),
                        })
                    }
                };

                match base_val {
                    Value::Array(vals) => vals.get(index).cloned().ok_or_else(|| InterpError {
                        message: format!("array index {} out of bounds", index),
                    }),
                    Value::Str(s) => s.chars().nth(index).map(Value::Char).ok_or_else(|| {
                        InterpError {
                            message: format!("string index {} out of bounds", index),
                        }
                    }),
                    _ => Err(InterpError {
                        message: "index access on non-indexable".to_string(),
                    }),
                }
            }

            Rvalue::Cast(op, _ty) => {
                // For now, just return the value unchanged
                self.eval_operand(op)
            }
        }
    }

    fn eval_operand(&self, op: &Operand) -> Result<Value, InterpError> {
        match op {
            Operand::Constant(c) => Ok(self.const_to_value(c)),

            Operand::Local(local) | Operand::Copy(local) | Operand::Move(local) => {
                let frame = self.call_stack.last().unwrap();
                frame
                    .locals
                    .get(local)
                    .cloned()
                    .ok_or_else(|| InterpError {
                        message: format!("undefined local: {}", local),
                    })
            }
        }
    }

    fn const_to_value(&self, c: &Constant) -> Value {
        match c {
            Constant::Unit => Value::Unit,
            Constant::Bool(b) => Value::Bool(*b),
            Constant::Int(n) => Value::Int(*n),
            Constant::Float(n) => Value::Float(*n),
            Constant::Char(c) => Value::Char(*c),
            Constant::Str(s) => Value::Str(s.clone()),
        }
    }

    fn eval_binop(&self, op: BinOp, left: Value, right: Value) -> Result<Value, InterpError> {
        match (op, &left, &right) {
            // Integer arithmetic
            (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (BinOp::Div, Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(InterpError {
                        message: "division by zero".to_string(),
                    })
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (BinOp::Rem, Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(InterpError {
                        message: "remainder by zero".to_string(),
                    })
                } else {
                    Ok(Value::Int(a % b))
                }
            }

            // Float arithmetic
            (BinOp::Add, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (BinOp::Sub, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (BinOp::Mul, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (BinOp::Div, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),

            // Integer comparison
            (BinOp::Eq, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (BinOp::Ne, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a != b)),
            (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (BinOp::Le, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (BinOp::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (BinOp::Ge, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),

            // Boolean comparison
            (BinOp::Eq, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (BinOp::Ne, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),

            // String comparison
            (BinOp::Eq, Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a == b)),
            (BinOp::Ne, Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a != b)),

            // Logical
            (BinOp::And, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
            (BinOp::Or, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a || *b)),

            // Bitwise
            (BinOp::BitAnd, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
            (BinOp::BitOr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
            (BinOp::BitXor, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
            (BinOp::Shl, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
            (BinOp::Shr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),

            // String concatenation
            (BinOp::Add, Value::Str(a), Value::Str(b)) => {
                Ok(Value::Str(format!("{}{}", a, b)))
            }

            _ => Err(InterpError {
                message: format!(
                    "unsupported binary operation: {:?} on {:?} and {:?}",
                    op, left, right
                ),
            }),
        }
    }

    fn eval_unop(&self, op: UnOp, val: Value) -> Result<Value, InterpError> {
        match (op, &val) {
            (UnOp::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnOp::Neg, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (UnOp::BitNot, Value::Int(n)) => Ok(Value::Int(!n)),
            _ => Err(InterpError {
                message: format!("unsupported unary operation: {:?} on {:?}", op, val),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mir::lower::Lowerer;
    use crate::{Parser, Scanner};

    fn run_source(source: &str) -> Result<Value, String> {
        let scanner = Scanner::new(source);
        let (tokens, _) = scanner.scan_all();
        let parser = Parser::new(&tokens);
        let ast = parser.parse().map_err(|e| format!("parse error: {:?}", e))?;

        let program = Lowerer::new()
            .lower(&ast)
            .map_err(|e| format!("lower error: {:?}", e))?;

        let mut interp = Interpreter::new(program);
        interp
            .run("main", &[])
            .map_err(|e| format!("runtime error: {}", e))
    }

    #[test]
    fn test_simple_return() {
        let result = run_source("f main() -> Int = 42").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_arithmetic() {
        let result = run_source("f main() -> Int = 2 + 3 * 4").unwrap();
        assert_eq!(result, Value::Int(14));
    }

    #[test]
    fn test_comparison() {
        let result = run_source("f main() -> Bool = 5 > 3").unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_if_expression() {
        let result = run_source("f main() -> Int = if 5 > 3 then 1 else 0").unwrap();
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn test_let_binding() {
        let result = run_source(
            r#"f main() -> Int
    x = 10
    y = 20
    x + y"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(30));
    }

    #[test]
    fn test_function_call() {
        let result = run_source(
            r#"
f add(a: Int, b: Int) -> Int = a + b

f main() -> Int = add(10, 20)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(30));
    }

    #[test]
    fn test_recursive_function() {
        let result = run_source(
            r#"
f factorial(n: Int) -> Int
    if n <= 1 then 1 else n * factorial(n - 1)

f main() -> Int = factorial(5)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(120));
    }

    #[test]
    fn test_while_loop() {
        let result = run_source(
            r#"f main() -> Int
    x := 5
    result := 1
    wh x > 0
        result = result * x
        x = x - 1
    result"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(120));
    }

    #[test]
    fn test_unary_neg() {
        let result = run_source("f main() -> Int = -42").unwrap();
        assert_eq!(result, Value::Int(-42));
    }

    #[test]
    fn test_unary_not() {
        let result = run_source("f main() -> Bool = !true").unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_nested_if() {
        let result = run_source(
            r#"f main() -> Int
    x = 10
    if x > 5 then (if x > 8 then 100 else 50) else 0"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(100));
    }

    #[test]
    fn test_multiple_functions() {
        let result = run_source(
            r#"
f double(x: Int) -> Int = x * 2

f square(x: Int) -> Int = x * x

f main() -> Int = square(double(3))
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(36));
    }

    #[test]
    fn test_logical_operators() {
        let result = run_source("f main() -> Bool = true && false").unwrap();
        assert_eq!(result, Value::Bool(false));

        let result = run_source("f main() -> Bool = true || false").unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_comparison_chain() {
        let result = run_source(
            r#"f main() -> Bool
    x = 5
    x >= 1 && x <= 10"#,
        )
        .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_string_literal() {
        let result = run_source(r#"f main() -> Str = "hello""#).unwrap();
        assert_eq!(result, Value::Str("hello".to_string()));
    }

    #[test]
    fn test_modulo() {
        let result = run_source("f main() -> Int = 17 % 5").unwrap();
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn test_nested_arithmetic() {
        let result = run_source("f main() -> Int = (2 + 3) * (4 - 1)").unwrap();
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn test_countdown_while() {
        let result = run_source(
            r#"f main() -> Int
    count := 10
    sum := 0
    wh count > 0
        sum = sum + count
        count = count - 1
    sum"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(55)); // 10 + 9 + 8 + ... + 1
    }

    #[test]
    fn test_fibonacci_iterative() {
        // Simpler Fibonacci that avoids potential parsing issues
        let result = run_source(
            r#"
f fib(n: Int) -> Int
    if n <= 1 then n else fib(n - 1) + fib(n - 2)

f main() -> Int = fib(10)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(55)); // 10th Fibonacci number
    }

    #[test]
    fn test_gcd() {
        let result = run_source(
            r#"
f gcd(a: Int, b: Int) -> Int
    if b == 0 then a else gcd(b, a % b)

f main() -> Int = gcd(48, 18)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(6));
    }

    #[test]
    fn test_mutual_recursion() {
        let result = run_source(
            r#"
f is_even(n: Int) -> Bool
    if n == 0 then true else is_odd(n - 1)

f is_odd(n: Int) -> Bool
    if n == 0 then false else is_even(n - 1)

f main() -> Bool = is_even(10)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    // Note: ARIA uses | for pipeline, so bitwise operators are not directly
    // available in expression syntax. Skipping bitwise tests for now.
}
