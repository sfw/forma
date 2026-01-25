//! Simple interpreter for MIR.
//!
//! This module provides an interpreter that can execute MIR programs
//! for testing and validation purposes.

use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH, Duration};
use std::thread;
use rand::Rng;
use serde_json;
use chrono::{DateTime, Utc, TimeZone, Datelike, Timelike, Weekday};
use base64::{Engine as _, engine::general_purpose::STANDARD as BASE64};
use sha2::{Sha256, Digest};
use uuid::Uuid;
use regex::Regex;

use super::mir::{
    BinOp, BlockId, Constant, Function, Local, Operand, Program, Rvalue,
    StatementKind, Terminator, UnOp,
};
use crate::types::Ty;

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
    /// Enum variant value at runtime.
    ///
    /// Represents an instantiated enum variant with its payload.
    /// - `type_name`: The enum type (e.g., "Option", "Result")
    /// - `variant`: The active variant (e.g., "Some", "None", "Ok", "Err")
    /// - `fields`: The values contained in the variant (empty for unit variants)
    ///
    /// # Examples
    /// ```text
    /// None    -> Enum { type_name: "Option", variant: "None", fields: [] }
    /// Some(5) -> Enum { type_name: "Option", variant: "Some", fields: [Int(5)] }
    /// Ok(42)  -> Enum { type_name: "Result", variant: "Ok", fields: [Int(42)] }
    /// ```
    Enum {
        type_name: String,
        variant: String,
        fields: Vec<Value>,
    },
    Ref(Box<Value>),
    /// HashMap for key-value storage
    Map(HashMap<String, Value>),
    /// Closure value - a function with captured environment
    ///
    /// - `func_name`: The lifted function that implements the closure
    /// - `captures`: Values captured from the enclosing scope
    Closure {
        func_name: String,
        captures: Vec<Value>,
    },
    /// JSON value for dynamic JSON operations
    Json(serde_json::Value),
    /// Task - a handle to a spawned async computation
    /// Contains the result once the task completes (simplified sync implementation)
    Task(Box<Value>),
    /// Future - result of an async block (simplified sync implementation)
    Future(Box<Value>),
    /// Channel - bounded channel with sender/receiver pair
    /// In this simplified implementation, uses a channel ID to reference shared state
    Channel(u64),
    /// Sender half of a channel
    Sender(u64),
    /// Receiver half of a channel
    Receiver(u64),
    /// Mutex - synchronization primitive
    /// In this simplified implementation, just wraps a value
    Mutex(u64),
    /// MutexGuard - holds a lock on a mutex
    MutexGuard(u64),
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
            Value::Enum { type_name, variant, fields } => {
                write!(f, "{}::{}", type_name, variant)?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, v) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", v)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Value::Ref(inner) => write!(f, "&{}", inner),
            Value::Map(map) => {
                write!(f, "{{")?;
                for (i, (k, v)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Closure { func_name, captures } => {
                write!(f, "<closure {} [", func_name)?;
                for (i, v) in captures.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]>")
            }
            Value::Json(json) => write!(f, "{}", json),
            Value::Task(inner) => write!(f, "Task({})", inner),
            Value::Future(inner) => write!(f, "Future({})", inner),
            Value::Channel(id) => write!(f, "Channel({})", id),
            Value::Sender(id) => write!(f, "Sender({})", id),
            Value::Receiver(id) => write!(f, "Receiver({})", id),
            Value::Mutex(id) => write!(f, "Mutex({})", id),
            Value::MutexGuard(id) => write!(f, "MutexGuard({})", id),
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
    /// Function name (useful for stack traces in debugging)
    #[allow(dead_code)]
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
    /// Channel state: maps channel ID to (queue, capacity, closed)
    channels: std::collections::HashMap<u64, (Vec<Value>, usize, bool)>,
    /// Next channel ID
    next_channel_id: u64,
    /// Mutex state: maps mutex ID to (value, locked)
    mutexes: std::collections::HashMap<u64, (Value, bool)>,
    /// Next mutex ID
    next_mutex_id: u64,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            call_stack: Vec::new(),
            max_steps: 1_000_000,
            channels: std::collections::HashMap::new(),
            next_channel_id: 0,
            mutexes: std::collections::HashMap::new(),
            next_mutex_id: 0,
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
                    let result = if let Some(builtin_result) = self.call_builtin(&fn_name, &arg_vals)? {
                        builtin_result
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

                Terminator::CallIndirect {
                    callee,
                    args,
                    dest,
                    next,
                } => {
                    // Evaluate the callee (should be a closure or function reference)
                    let callee_val = self.eval_operand(&callee)?;

                    // Evaluate arguments
                    let arg_vals: Vec<Value> = args
                        .iter()
                        .map(|a| self.eval_operand(a))
                        .collect::<Result<_, _>>()?;

                    let result = match callee_val {
                        Value::Closure { func_name, captures } => {
                            // Get the closure's implementation function
                            if let Some(callee_fn) = self.program.functions.get(&func_name).cloned() {
                                let mut callee_frame = Frame::new(func_name.clone(), callee_fn.entry_block);

                                // First bind captured values, then regular arguments
                                // The lifted function has signature: fn(captures..., args...)
                                let all_args: Vec<Value> = captures.into_iter().chain(arg_vals.into_iter()).collect();

                                for ((local, _ty), value) in callee_fn.params.iter().zip(all_args.iter()) {
                                    callee_frame.locals.insert(*local, value.clone());
                                }

                                self.call_stack.push(callee_frame);
                                let result = self.execute(&callee_fn)?;
                                self.call_stack.pop();
                                result
                            } else {
                                return Err(InterpError {
                                    message: format!("undefined closure function: {}", func_name),
                                });
                            }
                        }
                        _ => {
                            return Err(InterpError {
                                message: format!("cannot call non-closure value: {:?}", callee_val),
                            });
                        }
                    };

                    // Store result and continue
                    let frame = self.call_stack.last_mut().unwrap();
                    if let Some(d) = dest {
                        frame.locals.insert(d, result);
                    }
                    frame.current_block = next;
                }

                Terminator::Spawn {
                    expr,
                    dest,
                    next,
                } => {
                    // Spawn: evaluate the expression and wrap it in a Task
                    // In this simplified synchronous implementation, we execute immediately
                    let value = self.eval_operand(&expr)?;
                    let task_value = match value {
                        Value::Future(inner) => Value::Task(inner),
                        other => Value::Task(Box::new(other)),
                    };

                    let frame = self.call_stack.last_mut().unwrap();
                    if let Some(d) = dest {
                        frame.locals.insert(d, task_value);
                    }
                    frame.current_block = next;
                }

                Terminator::Await {
                    task,
                    dest,
                    next,
                } => {
                    // Await: extract the value from a Task or Future
                    let value = self.eval_operand(&task)?;
                    let result = match value {
                        Value::Task(inner) => *inner,
                        Value::Future(inner) => *inner,
                        other => other, // For backwards compatibility
                    };

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

    /// Handle built-in functions. Returns Some(result) if the function is a built-in,
    /// None if it should be handled as a regular function call.
    fn call_builtin(&mut self, fn_name: &str, args: &[Value]) -> Result<Option<Value>, InterpError> {
        match fn_name {
            // ===== I/O =====
            "print" => {
                for (i, val) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}", val);
                }
                println!();
                Ok(Some(Value::Unit))
            }

            // str(value) -> Str - convert any value to a string
            "str" => {
                if args.is_empty() {
                    return Err(InterpError { message: "str: expected 1 argument".to_string() });
                }
                // Special case: strings should not get extra quotes
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Bool(b) => format!("{}", b),
                    Value::Int(n) => format!("{}", n),
                    Value::Float(f) => format!("{}", f),
                    Value::Char(c) => format!("{}", c),
                    other => format!("{}", other), // fallback to Display
                };
                Ok(Some(Value::Str(s)))
            }

            // ===== Vec operations =====
            "vec_new" => {
                Ok(Some(Value::Array(vec![])))
            }
            "vec_len" => {
                match &args[0] {
                    Value::Array(arr) => Ok(Some(Value::Int(arr.len() as i64))),
                    Value::Str(s) => Ok(Some(Value::Int(s.len() as i64))),  // Support .len() on strings
                    Value::Ref(inner) => {
                        match inner.as_ref() {
                            Value::Array(arr) => Ok(Some(Value::Int(arr.len() as i64))),
                            Value::Str(s) => Ok(Some(Value::Int(s.len() as i64))),
                            _ => Err(InterpError { message: "len: expected array or string".to_string() })
                        }
                    }
                    _ => Err(InterpError { message: "len: expected array or string".to_string() })
                }
            }
            "vec_get" => {
                let arr = match &args[0] {
                    Value::Array(arr) => arr,
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr
                        } else {
                            return Err(InterpError { message: "vec_get: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_get: expected array".to_string() })
                };
                let idx = args[1].as_int().ok_or_else(|| InterpError {
                    message: "vec_get: index must be Int".to_string()
                })?;
                if idx < 0 || idx as usize >= arr.len() {
                    // Return None for out of bounds
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![arr[idx as usize].clone()],
                    }))
                }
            }
            "vec_first" => {
                let arr = match &args[0] {
                    Value::Array(arr) => arr,
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr
                        } else {
                            return Err(InterpError { message: "vec_first: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_first: expected array".to_string() })
                };
                if arr.is_empty() {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![arr[0].clone()],
                    }))
                }
            }
            "vec_last" => {
                let arr = match &args[0] {
                    Value::Array(arr) => arr,
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr
                        } else {
                            return Err(InterpError { message: "vec_last: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_last: expected array".to_string() })
                };
                if arr.is_empty() {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![arr[arr.len() - 1].clone()],
                    }))
                }
            }

            // ===== String operations =====
            "str_len" => {
                let s = match &args[0] {
                    Value::Str(s) => s,
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s
                        } else {
                            return Err(InterpError { message: "str_len: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_len: expected string".to_string() })
                };
                Ok(Some(Value::Int(s.len() as i64)))
            }
            "str_char_at" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_char_at: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_char_at: expected string".to_string() })
                };
                let idx = args[1].as_int().ok_or_else(|| InterpError {
                    message: "str_char_at: index must be Int".to_string()
                })?;
                match s.chars().nth(idx as usize) {
                    Some(c) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Char(c)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }
            "str_slice" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_slice: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_slice: expected string".to_string() })
                };
                let start = args[1].as_int().ok_or_else(|| InterpError {
                    message: "str_slice: start must be Int".to_string()
                })? as usize;
                let end = args[2].as_int().ok_or_else(|| InterpError {
                    message: "str_slice: end must be Int".to_string()
                })? as usize;
                let chars: Vec<char> = s.chars().collect();
                let start = start.min(chars.len());
                let end = end.min(chars.len());
                let result: String = chars[start..end].iter().collect();
                Ok(Some(Value::Str(result)))
            }
            "str_contains" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_contains: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_contains: expected string".to_string() })
                };
                let sub = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_contains: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_contains: expected string".to_string() })
                };
                Ok(Some(Value::Bool(s.contains(&sub))))
            }
            "str_starts_with" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_starts_with: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_starts_with: expected string".to_string() })
                };
                let prefix = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_starts_with: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_starts_with: expected string".to_string() })
                };
                Ok(Some(Value::Bool(s.starts_with(&prefix))))
            }
            "str_ends_with" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_ends_with: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_ends_with: expected string".to_string() })
                };
                let suffix = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_ends_with: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_ends_with: expected string".to_string() })
                };
                Ok(Some(Value::Bool(s.ends_with(&suffix))))
            }
            "str_split" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_split: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_split: expected string".to_string() })
                };
                let delim = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_split: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_split: expected string".to_string() })
                };
                let parts: Vec<Value> = s.split(&delim).map(|p| Value::Str(p.to_string())).collect();
                Ok(Some(Value::Array(parts)))
            }
            "str_trim" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_trim: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_trim: expected string".to_string() })
                };
                Ok(Some(Value::Str(s.trim().to_string())))
            }
            "str_to_int" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_to_int: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_to_int: expected string".to_string() })
                };
                match s.trim().parse::<i64>() {
                    Ok(n) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(n)],
                    })),
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }
            "int_to_str" => {
                let n = args[0].as_int().ok_or_else(|| InterpError {
                    message: "int_to_str: expected Int".to_string()
                })?;
                Ok(Some(Value::Str(n.to_string())))
            }
            "str_to_int_radix" => {
                // str_to_int_radix(s, radix) -> Option[Int]
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_to_int_radix: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_to_int_radix: expected string".to_string() })
                };
                let radix = args[1].as_int().ok_or_else(|| InterpError {
                    message: "str_to_int_radix: expected Int radix".to_string()
                })? as u32;
                match i64::from_str_radix(s.trim(), radix) {
                    Ok(n) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(n)],
                    })),
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }
            "str_replace_all" => {
                // str_replace_all(s, pattern, replacement) -> Str
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_replace_all: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_replace_all: expected string".to_string() })
                };
                let pattern = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_replace_all: expected string pattern".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_replace_all: expected string pattern".to_string() })
                };
                let replacement = match &args[2] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_replace_all: expected string replacement".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_replace_all: expected string replacement".to_string() })
                };
                Ok(Some(Value::Str(s.replace(&pattern, &replacement))))
            }
            "str_concat" => {
                let s1 = match &args[0] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_concat: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_concat: expected string".to_string() })
                };
                let s2 = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "str_concat: expected string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "str_concat: expected string".to_string() })
                };
                Ok(Some(Value::Str(format!("{}{}", s1, s2))))
            }

            // ===== Char operations =====
            "char_is_digit" => {
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_is_digit: expected Char".to_string() })
                };
                Ok(Some(Value::Bool(c.is_ascii_digit())))
            }
            "char_is_alpha" => {
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_is_alpha: expected Char".to_string() })
                };
                Ok(Some(Value::Bool(c.is_alphabetic())))
            }
            "char_is_alphanumeric" => {
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_is_alphanumeric: expected Char".to_string() })
                };
                Ok(Some(Value::Bool(c.is_alphanumeric())))
            }
            "char_is_whitespace" => {
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_is_whitespace: expected Char".to_string() })
                };
                Ok(Some(Value::Bool(c.is_whitespace())))
            }
            "char_to_int" => {
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_to_int: expected Char".to_string() })
                };
                Ok(Some(Value::Int(c as i64)))
            }
            "int_to_char" => {
                let n = args[0].as_int().ok_or_else(|| InterpError {
                    message: "int_to_char: expected Int".to_string()
                })?;
                if n >= 0 && n <= 0x10FFFF {
                    if let Some(c) = char::from_u32(n as u32) {
                        return Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "Some".to_string(),
                            fields: vec![Value::Char(c)],
                        }));
                    }
                }
                Ok(Some(Value::Enum {
                    type_name: "Option".to_string(),
                    variant: "None".to_string(),
                    fields: vec![],
                }))
            }
            "char_to_str" => {
                // Convert a Char to a single-character Str
                let c = match &args[0] {
                    Value::Char(c) => *c,
                    _ => return Err(InterpError { message: "char_to_str: expected Char".to_string() })
                };
                Ok(Some(Value::Str(c.to_string())))
            }

            // ===== HashMap operations =====
            "map_new" => {
                Ok(Some(Value::Map(HashMap::new())))
            }
            "map_len" => {
                let map = match &args[0] {
                    Value::Map(m) => m,
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m
                        } else {
                            return Err(InterpError { message: "map_len: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_len: expected map".to_string() })
                };
                Ok(Some(Value::Int(map.len() as i64)))
            }
            "map_get" => {
                let map = match &args[0] {
                    Value::Map(m) => m,
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m
                        } else {
                            return Err(InterpError { message: "map_get: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_get: expected map".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "map_get: key must be string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_get: key must be string".to_string() })
                };
                match map.get(&key) {
                    Some(v) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![v.clone()],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }
            "map_contains" => {
                let map = match &args[0] {
                    Value::Map(m) => m,
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m
                        } else {
                            return Err(InterpError { message: "map_contains: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_contains: expected map".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "map_contains: key must be string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_contains: key must be string".to_string() })
                };
                Ok(Some(Value::Bool(map.contains_key(&key))))
            }
            "map_keys" => {
                let map = match &args[0] {
                    Value::Map(m) => m,
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m
                        } else {
                            return Err(InterpError { message: "map_keys: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_keys: expected map".to_string() })
                };
                let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                Ok(Some(Value::Array(keys)))
            }

            // ===== Functional mutating operations (return new collections) =====
            "vec_push" => {
                // vec_push(arr, elem) -> new array with elem appended
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_push: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_push: expected array".to_string() })
                };
                let mut new_arr = arr;
                new_arr.push(args[1].clone());
                Ok(Some(Value::Array(new_arr)))
            }
            "vec_pop" => {
                // vec_pop(arr) -> (new_array, Option<elem>)
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_pop: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_pop: expected array".to_string() })
                };
                let mut new_arr = arr;
                let popped = new_arr.pop();
                let opt = match popped {
                    Some(v) => Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![v],
                    },
                    None => Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    },
                };
                Ok(Some(Value::Tuple(vec![Value::Array(new_arr), opt])))
            }
            "vec_set" => {
                // vec_set(arr, index, value) -> new array with element at index replaced
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_set: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_set: expected array".to_string() })
                };
                let idx = args[1].as_int().ok_or_else(|| InterpError {
                    message: "vec_set: index must be Int".to_string()
                })?;
                if idx < 0 || idx as usize >= arr.len() {
                    return Err(InterpError { message: format!("vec_set: index {} out of bounds", idx) });
                }
                let mut new_arr = arr;
                new_arr[idx as usize] = args[2].clone();
                Ok(Some(Value::Array(new_arr)))
            }
            "vec_concat" => {
                // vec_concat(arr1, arr2) -> combined array
                let arr1 = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_concat: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_concat: expected array".to_string() })
                };
                let arr2 = match &args[1] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_concat: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_concat: expected array".to_string() })
                };
                let mut result = arr1;
                result.extend(arr2);
                Ok(Some(Value::Array(result)))
            }
            "vec_slice" => {
                // vec_slice(arr, start, end) -> new array containing arr[start..end]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_slice: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_slice: expected array".to_string() })
                };
                let start = args[1].as_int().ok_or_else(|| InterpError {
                    message: "vec_slice: start must be Int".to_string()
                })? as usize;
                let end = args[2].as_int().ok_or_else(|| InterpError {
                    message: "vec_slice: end must be Int".to_string()
                })? as usize;
                let start = start.min(arr.len());
                let end = end.min(arr.len());
                Ok(Some(Value::Array(arr[start..end].to_vec())))
            }
            "vec_reverse" => {
                // vec_reverse(arr) -> reversed array
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    Value::Ref(inner) => {
                        if let Value::Array(arr) = inner.as_ref() {
                            arr.clone()
                        } else {
                            return Err(InterpError { message: "vec_reverse: expected array".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "vec_reverse: expected array".to_string() })
                };
                let mut result = arr;
                result.reverse();
                Ok(Some(Value::Array(result)))
            }

            "map_insert" => {
                // map_insert(map, key, value) -> new map with entry added
                let map = match &args[0] {
                    Value::Map(m) => m.clone(),
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m.clone()
                        } else {
                            return Err(InterpError { message: "map_insert: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_insert: expected map".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "map_insert: key must be string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_insert: key must be string".to_string() })
                };
                let mut new_map = map;
                new_map.insert(key, args[2].clone());
                Ok(Some(Value::Map(new_map)))
            }
            "map_remove" => {
                // map_remove(map, key) -> (new_map, Option<removed_value>)
                let map = match &args[0] {
                    Value::Map(m) => m.clone(),
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m.clone()
                        } else {
                            return Err(InterpError { message: "map_remove: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_remove: expected map".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    Value::Ref(inner) => {
                        if let Value::Str(s) = inner.as_ref() {
                            s.clone()
                        } else {
                            return Err(InterpError { message: "map_remove: key must be string".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_remove: key must be string".to_string() })
                };
                let mut new_map = map;
                let removed = new_map.remove(&key);
                let opt = match removed {
                    Some(v) => Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![v],
                    },
                    None => Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    },
                };
                Ok(Some(Value::Tuple(vec![Value::Map(new_map), opt])))
            }
            "map_values" => {
                // map_values(map) -> array of values
                let map = match &args[0] {
                    Value::Map(m) => m,
                    Value::Ref(inner) => {
                        if let Value::Map(m) = inner.as_ref() {
                            m
                        } else {
                            return Err(InterpError { message: "map_values: expected map".to_string() });
                        }
                    }
                    _ => return Err(InterpError { message: "map_values: expected map".to_string() })
                };
                let values: Vec<Value> = map.values().cloned().collect();
                Ok(Some(Value::Array(values)))
            }

            // ===== Type/Debug operations =====
            "type_of" => {
                let type_name = match &args[0] {
                    Value::Unit => "Unit",
                    Value::Bool(_) => "Bool",
                    Value::Int(_) => "Int",
                    Value::Float(_) => "Float",
                    Value::Char(_) => "Char",
                    Value::Str(_) => "Str",
                    Value::Tuple(_) => "Tuple",
                    Value::Array(_) => "Array",
                    Value::Struct(name, _) => name.as_str(),
                    Value::Enum { type_name, .. } => type_name.as_str(),
                    Value::Ref(_) => "Ref",
                    Value::Map(_) => "Map",
                    Value::Closure { .. } => "Closure",
                    Value::Json(_) => "Json",
                    Value::Task(_) => "Task",
                    Value::Future(_) => "Future",
                    Value::Channel(_) => "Channel",
                    Value::Sender(_) => "Sender",
                    Value::Receiver(_) => "Receiver",
                    Value::Mutex(_) => "Mutex",
                    Value::MutexGuard(_) => "MutexGuard",
                };
                Ok(Some(Value::Str(type_name.to_string())))
            }
            "panic" => {
                let msg = match &args[0] {
                    Value::Str(s) => s.clone(),
                    other => format!("{}", other),
                };
                Err(InterpError { message: format!("panic: {}", msg) })
            }
            "assert" => {
                let cond = args[0].as_bool().ok_or_else(|| InterpError {
                    message: "assert: expected Bool".to_string()
                })?;
                if !cond {
                    let msg = if args.len() > 1 {
                        match &args[1] {
                            Value::Str(s) => s.clone(),
                            other => format!("{}", other),
                        }
                    } else {
                        "assertion failed".to_string()
                    };
                    return Err(InterpError { message: format!("assertion failed: {}", msg) });
                }
                Ok(Some(Value::Unit))
            }

            // ===== Option/Result unwrapping =====
            "unwrap" => {
                // unwrap(opt) - panic if None, return value if Some
                match &args[0] {
                    Value::Enum { variant, fields, .. } => {
                        match variant.as_str() {
                            "Some" | "Ok" => {
                                Ok(Some(fields.get(0).cloned().unwrap_or(Value::Unit)))
                            }
                            "None" => {
                                Err(InterpError { message: "unwrap called on None".to_string() })
                            }
                            "Err" => {
                                let err_val = fields.get(0).map(|v| format!("{}", v)).unwrap_or_default();
                                Err(InterpError { message: format!("unwrap called on Err: {}", err_val) })
                            }
                            _ => Err(InterpError { message: "unwrap: expected Option or Result".to_string() })
                        }
                    }
                    _ => Err(InterpError { message: "unwrap: expected Option or Result".to_string() })
                }
            }
            "expect" => {
                // expect(opt, msg) - like unwrap but with custom error message
                let msg = if args.len() > 1 {
                    match &args[1] {
                        Value::Str(s) => s.clone(),
                        other => format!("{}", other),
                    }
                } else {
                    "expect failed".to_string()
                };

                match &args[0] {
                    Value::Enum { variant, fields, .. } => {
                        match variant.as_str() {
                            "Some" | "Ok" => {
                                Ok(Some(fields.get(0).cloned().unwrap_or(Value::Unit)))
                            }
                            "None" | "Err" => {
                                Err(InterpError { message: msg })
                            }
                            _ => Err(InterpError { message: "expect: expected Option or Result".to_string() })
                        }
                    }
                    _ => Err(InterpError { message: "expect: expected Option or Result".to_string() })
                }
            }
            "unwrap_or" => {
                // unwrap_or(opt, default) - return default if None/Err
                match &args[0] {
                    Value::Enum { variant, fields, .. } => {
                        match variant.as_str() {
                            "Some" | "Ok" => {
                                Ok(Some(fields.get(0).cloned().unwrap_or(Value::Unit)))
                            }
                            "None" | "Err" => {
                                Ok(Some(args.get(1).cloned().unwrap_or(Value::Unit)))
                            }
                            _ => Err(InterpError { message: "unwrap_or: expected Option or Result".to_string() })
                        }
                    }
                    _ => Err(InterpError { message: "unwrap_or: expected Option or Result".to_string() })
                }
            }
            "is_some" => {
                // is_some(opt) - returns true if Some, false if None
                match &args[0] {
                    Value::Enum { variant, .. } => {
                        Ok(Some(Value::Bool(variant == "Some")))
                    }
                    _ => Err(InterpError { message: "is_some: expected Option".to_string() })
                }
            }
            "is_none" => {
                // is_none(opt) - returns true if None, false if Some
                match &args[0] {
                    Value::Enum { variant, .. } => {
                        Ok(Some(Value::Bool(variant == "None")))
                    }
                    _ => Err(InterpError { message: "is_none: expected Option".to_string() })
                }
            }
            "is_ok" => {
                // is_ok(result) - returns true if Ok, false if Err
                match &args[0] {
                    Value::Enum { variant, .. } => {
                        Ok(Some(Value::Bool(variant == "Ok")))
                    }
                    _ => Err(InterpError { message: "is_ok: expected Result".to_string() })
                }
            }
            "is_err" => {
                // is_err(result) - returns true if Err, false if Ok
                match &args[0] {
                    Value::Enum { variant, .. } => {
                        Ok(Some(Value::Bool(variant == "Err")))
                    }
                    _ => Err(InterpError { message: "is_err: expected Result".to_string() })
                }
            }

            // ===== File I/O =====
            "file_read" => {
                // file_read(path: Str) -> Result[Str, Str]
                let path = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_read: expected Str path".to_string() })
                };
                match std::fs::read_to_string(&path) {
                    Ok(content) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Str(content)],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    }))
                }
            }
            "file_write" => {
                // file_write(path: Str, content: Str) -> Result[(), Str]
                let path = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_write: expected Str path".to_string() })
                };
                let content = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_write: expected Str content".to_string() })
                };
                match std::fs::write(&path, &content) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    }))
                }
            }
            "file_exists" => {
                // file_exists(path: Str) -> Bool
                let path = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_exists: expected Str path".to_string() })
                };
                Ok(Some(Value::Bool(std::path::Path::new(&path).exists())))
            }
            "file_append" => {
                // file_append(path: Str, content: Str) -> Result[(), Str]
                use std::io::Write;
                let path = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_append: expected Str path".to_string() })
                };
                let content = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "file_append: expected Str content".to_string() })
                };
                let result = std::fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(&path)
                    .and_then(|mut f| f.write_all(content.as_bytes()));
                match result {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    }))
                }
            }

            // ===== CLI support =====
            "args" => {
                // args() -> [Str] - command line arguments
                let args: Vec<Value> = std::env::args()
                    .map(|s| Value::Str(s))
                    .collect();
                Ok(Some(Value::Array(args)))
            }
            "env_get" => {
                // env_get(name: Str) -> Option[Str]
                let name = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "env_get: expected Str".to_string() })
                };
                match std::env::var(&name) {
                    Ok(val) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(val)],
                    })),
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }
            "exit" => {
                // exit(code: Int) -> !
                let code = match &args[0] {
                    Value::Int(n) => *n as i32,
                    _ => return Err(InterpError { message: "exit: expected Int".to_string() })
                };
                std::process::exit(code);
            }
            "eprintln" => {
                // eprintln(msg: Str) - print to stderr
                for (i, val) in args.iter().enumerate() {
                    if i > 0 {
                        eprint!(" ");
                    }
                    eprint!("{}", val);
                }
                eprintln!();
                Ok(Some(Value::Unit))
            }

            // ===== Random number generation =====
            "random" => {
                // random() -> Float (0.0 to 1.0)
                let mut rng = rand::thread_rng();
                Ok(Some(Value::Float(rng.r#gen::<f64>())))
            }
            "random_int" => {
                // random_int(min: Int, max: Int) -> Int
                let min = match &args[0] {
                    Value::Int(n) => *n,
                    _ => return Err(InterpError { message: "random_int: expected Int for min".to_string() })
                };
                let max = match &args[1] {
                    Value::Int(n) => *n,
                    _ => return Err(InterpError { message: "random_int: expected Int for max".to_string() })
                };
                let mut rng = rand::thread_rng();
                Ok(Some(Value::Int(rng.gen_range(min..=max))))
            }
            "random_bool" => {
                // random_bool() -> Bool
                let mut rng = rand::thread_rng();
                Ok(Some(Value::Bool(rng.r#gen::<bool>())))
            }
            "random_choice" => {
                // random_choice(arr: [T]) -> T
                let arr = match &args[0] {
                    Value::Array(vals) => vals,
                    _ => return Err(InterpError { message: "random_choice: expected array".to_string() })
                };
                if arr.is_empty() {
                    return Err(InterpError { message: "random_choice: array is empty".to_string() });
                }
                let mut rng = rand::thread_rng();
                let idx = rng.gen_range(0..arr.len());
                Ok(Some(arr[idx].clone()))
            }

            // ===== Float math operations =====
            "sqrt" => {
                // sqrt(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "sqrt: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.sqrt())))
            }
            "pow" => {
                // pow(base: Float, exp: Float) -> Float
                let base = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "pow: expected Float for base".to_string() })
                };
                let exp = match &args[1] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "pow: expected Float for exp".to_string() })
                };
                Ok(Some(Value::Float(base.powf(exp))))
            }
            "sin" => {
                // sin(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "sin: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.sin())))
            }
            "cos" => {
                // cos(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "cos: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.cos())))
            }
            "tan" => {
                // tan(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "tan: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.tan())))
            }
            "log" => {
                // log(x: Float) -> Float (natural log)
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "log: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.ln())))
            }
            "log10" => {
                // log10(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "log10: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.log10())))
            }
            "exp" => {
                // exp(x: Float) -> Float (e^x)
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "exp: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.exp())))
            }
            "floor" => {
                // floor(x: Float) -> Int
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "floor: expected Float".to_string() })
                };
                Ok(Some(Value::Int(x.floor() as i64)))
            }
            "ceil" => {
                // ceil(x: Float) -> Int
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "ceil: expected Float".to_string() })
                };
                Ok(Some(Value::Int(x.ceil() as i64)))
            }
            "round" => {
                // round(x: Float) -> Int
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "round: expected Float".to_string() })
                };
                Ok(Some(Value::Int(x.round() as i64)))
            }
            "abs_float" => {
                // abs_float(x: Float) -> Float
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(i) => *i as f64,
                    _ => return Err(InterpError { message: "abs_float: expected Float".to_string() })
                };
                Ok(Some(Value::Float(x.abs())))
            }

            // ===== Time functions =====
            "time_now" => {
                // time_now() -> Int (unix timestamp in seconds)
                let now = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or(Duration::ZERO);
                Ok(Some(Value::Int(now.as_secs() as i64)))
            }
            "time_now_ms" => {
                // time_now_ms() -> Int (unix timestamp in milliseconds)
                let now = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or(Duration::ZERO);
                Ok(Some(Value::Int(now.as_millis() as i64)))
            }
            "time_sleep" => {
                // time_sleep(ms: Int) -> ()
                let ms = match &args[0] {
                    Value::Int(n) => *n as u64,
                    _ => return Err(InterpError { message: "time_sleep: expected Int".to_string() })
                };
                thread::sleep(Duration::from_millis(ms));
                Ok(Some(Value::Unit))
            }

            // ===== Async operations =====
            "sleep_async" => {
                // sleep_async(ms: Int) -> Future[()]
                // In this simplified implementation, we sleep synchronously but wrap in Future
                let ms = match &args[0] {
                    Value::Int(n) => *n as u64,
                    _ => return Err(InterpError { message: "sleep_async: expected Int".to_string() })
                };
                thread::sleep(Duration::from_millis(ms));
                Ok(Some(Value::Future(Box::new(Value::Unit))))
            }

            "timeout" => {
                // timeout(ms: Int, task: Task[T]) -> Result[T, Str]
                // In this simplified implementation, we don't actually timeout
                // since we execute synchronously. Just return the task result.
                let _ms = match &args[0] {
                    Value::Int(n) => *n as u64,
                    _ => return Err(InterpError { message: "timeout: expected Int".to_string() })
                };
                let result = match &args[1] {
                    Value::Task(inner) => (**inner).clone(),
                    Value::Future(inner) => (**inner).clone(),
                    other => other.clone(),
                };
                // Return Ok(result) since we completed without timeout
                Ok(Some(Value::Enum {
                    type_name: "Result".to_string(),
                    variant: "Ok".to_string(),
                    fields: vec![result],
                }))
            }

            "await_all" => {
                // await_all(tasks: [Task[T]]) -> [T]
                // Awaits all tasks and returns their results
                let tasks = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "await_all: expected array of tasks".to_string() })
                };
                let results: Vec<Value> = tasks.into_iter().map(|task| {
                    match task {
                        Value::Task(inner) => *inner,
                        Value::Future(inner) => *inner,
                        other => other,
                    }
                }).collect();
                Ok(Some(Value::Array(results)))
            }

            "await_any" => {
                // await_any(tasks: [Task[T]]) -> T
                // Returns the first completed task's result
                // In synchronous mode, we just return the first task's result
                let tasks = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "await_any: expected array of tasks".to_string() })
                };
                if tasks.is_empty() {
                    return Err(InterpError { message: "await_any: empty task array".to_string() });
                }
                let result = match &tasks[0] {
                    Value::Task(inner) => (**inner).clone(),
                    Value::Future(inner) => (**inner).clone(),
                    other => other.clone(),
                };
                Ok(Some(result))
            }

            // ===== Channel operations =====
            "channel_new" => {
                // channel_new(capacity: Int) -> (Sender[T], Receiver[T])
                let capacity = match &args[0] {
                    Value::Int(n) => *n as usize,
                    _ => return Err(InterpError { message: "channel_new: expected Int capacity".to_string() })
                };
                let id = self.next_channel_id;
                self.next_channel_id += 1;
                self.channels.insert(id, (Vec::new(), capacity, false));
                Ok(Some(Value::Tuple(vec![Value::Sender(id), Value::Receiver(id)])))
            }

            "channel_send" => {
                // channel_send(sender: Sender[T], value: T) -> Result[(), Str]
                let id = match &args[0] {
                    Value::Sender(id) => *id,
                    _ => return Err(InterpError { message: "channel_send: expected Sender".to_string() })
                };
                let value = args[1].clone();

                if let Some((queue, capacity, closed)) = self.channels.get_mut(&id) {
                    if *closed {
                        return Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Err".to_string(),
                            fields: vec![Value::Str("channel closed".to_string())],
                        }));
                    }
                    if queue.len() >= *capacity && *capacity > 0 {
                        return Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Err".to_string(),
                            fields: vec![Value::Str("channel full".to_string())],
                        }));
                    }
                    queue.push(value);
                    Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str("invalid channel".to_string())],
                    }))
                }
            }

            "channel_recv" => {
                // channel_recv(receiver: Receiver[T]) -> Result[T, Str]
                let id = match &args[0] {
                    Value::Receiver(id) => *id,
                    _ => return Err(InterpError { message: "channel_recv: expected Receiver".to_string() })
                };

                if let Some((queue, _, closed)) = self.channels.get_mut(&id) {
                    if queue.is_empty() {
                        if *closed {
                            return Ok(Some(Value::Enum {
                                type_name: "Result".to_string(),
                                variant: "Err".to_string(),
                                fields: vec![Value::Str("channel closed".to_string())],
                            }));
                        }
                        return Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Err".to_string(),
                            fields: vec![Value::Str("channel empty".to_string())],
                        }));
                    }
                    let value = queue.remove(0);
                    Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![value],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str("invalid channel".to_string())],
                    }))
                }
            }

            "channel_try_send" => {
                // channel_try_send(sender: Sender[T], value: T) -> Bool
                let id = match &args[0] {
                    Value::Sender(id) => *id,
                    _ => return Err(InterpError { message: "channel_try_send: expected Sender".to_string() })
                };
                let value = args[1].clone();

                if let Some((queue, capacity, closed)) = self.channels.get_mut(&id) {
                    if *closed || (queue.len() >= *capacity && *capacity > 0) {
                        return Ok(Some(Value::Bool(false)));
                    }
                    queue.push(value);
                    Ok(Some(Value::Bool(true)))
                } else {
                    Ok(Some(Value::Bool(false)))
                }
            }

            "channel_try_recv" => {
                // channel_try_recv(receiver: Receiver[T]) -> T?
                let id = match &args[0] {
                    Value::Receiver(id) => *id,
                    _ => return Err(InterpError { message: "channel_try_recv: expected Receiver".to_string() })
                };

                if let Some((queue, _, _)) = self.channels.get_mut(&id) {
                    if queue.is_empty() {
                        return Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "None".to_string(),
                            fields: vec![],
                        }));
                    }
                    let value = queue.remove(0);
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![value],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }

            "channel_close" => {
                // channel_close(sender: Sender[T]) -> ()
                let id = match &args[0] {
                    Value::Sender(id) => *id,
                    _ => return Err(InterpError { message: "channel_close: expected Sender".to_string() })
                };

                if let Some((_, _, closed)) = self.channels.get_mut(&id) {
                    *closed = true;
                }
                Ok(Some(Value::Unit))
            }

            // ===== Mutex operations =====
            "mutex_new" => {
                // mutex_new(value: T) -> Mutex[T]
                let value = args[0].clone();
                let id = self.next_mutex_id;
                self.next_mutex_id += 1;
                self.mutexes.insert(id, (value, false));
                Ok(Some(Value::Mutex(id)))
            }

            "mutex_lock" => {
                // mutex_lock(m: Mutex[T]) -> MutexGuard[T]
                let id = match &args[0] {
                    Value::Mutex(id) => *id,
                    _ => return Err(InterpError { message: "mutex_lock: expected Mutex".to_string() })
                };

                if let Some((_, locked)) = self.mutexes.get_mut(&id) {
                    if *locked {
                        return Err(InterpError { message: "mutex_lock: mutex already locked (deadlock in sync mode)".to_string() });
                    }
                    *locked = true;
                    Ok(Some(Value::MutexGuard(id)))
                } else {
                    Err(InterpError { message: "mutex_lock: invalid mutex".to_string() })
                }
            }

            "mutex_try_lock" => {
                // mutex_try_lock(m: Mutex[T]) -> MutexGuard[T]?
                let id = match &args[0] {
                    Value::Mutex(id) => *id,
                    _ => return Err(InterpError { message: "mutex_try_lock: expected Mutex".to_string() })
                };

                if let Some((_, locked)) = self.mutexes.get_mut(&id) {
                    if *locked {
                        return Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "None".to_string(),
                            fields: vec![],
                        }));
                    }
                    *locked = true;
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::MutexGuard(id)],
                    }))
                } else {
                    Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    }))
                }
            }

            "mutex_unlock" => {
                // mutex_unlock(guard: MutexGuard[T]) -> ()
                let id = match &args[0] {
                    Value::MutexGuard(id) => *id,
                    _ => return Err(InterpError { message: "mutex_unlock: expected MutexGuard".to_string() })
                };

                if let Some((_, locked)) = self.mutexes.get_mut(&id) {
                    *locked = false;
                }
                Ok(Some(Value::Unit))
            }

            "mutex_get" => {
                // mutex_get(guard: MutexGuard[T]) -> T
                let id = match &args[0] {
                    Value::MutexGuard(id) => *id,
                    _ => return Err(InterpError { message: "mutex_get: expected MutexGuard".to_string() })
                };

                if let Some((value, _)) = self.mutexes.get(&id) {
                    Ok(Some(value.clone()))
                } else {
                    Err(InterpError { message: "mutex_get: invalid mutex".to_string() })
                }
            }

            "mutex_set" => {
                // mutex_set(guard: MutexGuard[T], value: T) -> ()
                let id = match &args[0] {
                    Value::MutexGuard(id) => *id,
                    _ => return Err(InterpError { message: "mutex_set: expected MutexGuard".to_string() })
                };
                let new_value = args[1].clone();

                if let Some((value, _)) = self.mutexes.get_mut(&id) {
                    *value = new_value;
                    Ok(Some(Value::Unit))
                } else {
                    Err(InterpError { message: "mutex_set: invalid mutex".to_string() })
                }
            }

            // ===== DateTime operations (chrono-based) =====
            "time_from_parts" => {
                // time_from_parts(year, month, day, hour, min, sec) -> Int
                let year = match &args[0] { Value::Int(n) => *n as i32, _ => return Err(InterpError { message: "time_from_parts: year must be Int".to_string() }) };
                let month = match &args[1] { Value::Int(n) => *n as u32, _ => return Err(InterpError { message: "time_from_parts: month must be Int".to_string() }) };
                let day = match &args[2] { Value::Int(n) => *n as u32, _ => return Err(InterpError { message: "time_from_parts: day must be Int".to_string() }) };
                let hour = match &args[3] { Value::Int(n) => *n as u32, _ => return Err(InterpError { message: "time_from_parts: hour must be Int".to_string() }) };
                let min = match &args[4] { Value::Int(n) => *n as u32, _ => return Err(InterpError { message: "time_from_parts: min must be Int".to_string() }) };
                let sec = match &args[5] { Value::Int(n) => *n as u32, _ => return Err(InterpError { message: "time_from_parts: sec must be Int".to_string() }) };
                match Utc.with_ymd_and_hms(year, month, day, hour, min, sec) {
                    chrono::LocalResult::Single(dt) => Ok(Some(Value::Int(dt.timestamp()))),
                    _ => Ok(Some(Value::Int(0))) // Invalid date
                }
            }
            "time_format" => {
                // time_format(timestamp, format_str) -> Str
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_format: timestamp must be Int".to_string() }) };
                let fmt = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "time_format: format must be Str".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Str(dt.format(&fmt).to_string())))
            }
            "time_format_iso" => {
                // time_format_iso(timestamp) -> Str
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_format_iso: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Str(dt.to_rfc3339())))
            }
            "time_format_rfc2822" => {
                // time_format_rfc2822(timestamp) -> Str
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_format_rfc2822: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Str(dt.to_rfc2822())))
            }
            "time_parse" => {
                // time_parse(s, format) -> Result[Int, Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "time_parse: input must be Str".to_string() }) };
                let fmt = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "time_parse: format must be Str".to_string() }) };
                match DateTime::parse_from_str(&s, &fmt) {
                    Ok(dt) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Int(dt.timestamp())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "time_parse_iso" => {
                // time_parse_iso(s) -> Result[Int, Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "time_parse_iso: input must be Str".to_string() }) };
                match DateTime::parse_from_rfc3339(&s) {
                    Ok(dt) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Int(dt.timestamp())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "time_year" => {
                // time_year(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_year: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.year() as i64)))
            }
            "time_month" => {
                // time_month(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_month: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.month() as i64)))
            }
            "time_day" => {
                // time_day(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_day: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.day() as i64)))
            }
            "time_hour" => {
                // time_hour(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_hour: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.hour() as i64)))
            }
            "time_minute" => {
                // time_minute(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_minute: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.minute() as i64)))
            }
            "time_second" => {
                // time_second(timestamp) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_second: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                Ok(Some(Value::Int(dt.second() as i64)))
            }
            "time_weekday" => {
                // time_weekday(timestamp) -> Int (0=Sunday, 6=Saturday)
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_weekday: timestamp must be Int".to_string() }) };
                let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
                let weekday = match dt.weekday() {
                    Weekday::Sun => 0,
                    Weekday::Mon => 1,
                    Weekday::Tue => 2,
                    Weekday::Wed => 3,
                    Weekday::Thu => 4,
                    Weekday::Fri => 5,
                    Weekday::Sat => 6,
                };
                Ok(Some(Value::Int(weekday)))
            }
            "duration_seconds" => {
                // duration_seconds(n) -> Int
                let n = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "duration_seconds: expected Int".to_string() }) };
                Ok(Some(Value::Int(n)))
            }
            "duration_minutes" => {
                // duration_minutes(n) -> Int
                let n = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "duration_minutes: expected Int".to_string() }) };
                Ok(Some(Value::Int(n * 60)))
            }
            "duration_hours" => {
                // duration_hours(n) -> Int
                let n = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "duration_hours: expected Int".to_string() }) };
                Ok(Some(Value::Int(n * 3600)))
            }
            "duration_days" => {
                // duration_days(n) -> Int
                let n = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "duration_days: expected Int".to_string() }) };
                Ok(Some(Value::Int(n * 86400)))
            }
            "time_add" => {
                // time_add(timestamp, duration) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_add: timestamp must be Int".to_string() }) };
                let dur = match &args[1] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_add: duration must be Int".to_string() }) };
                Ok(Some(Value::Int(ts + dur)))
            }
            "time_sub" => {
                // time_sub(timestamp, duration) -> Int
                let ts = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_sub: timestamp must be Int".to_string() }) };
                let dur = match &args[1] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_sub: duration must be Int".to_string() }) };
                Ok(Some(Value::Int(ts - dur)))
            }
            "time_diff" => {
                // time_diff(a, b) -> Int (a - b in seconds)
                let a = match &args[0] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_diff: first arg must be Int".to_string() }) };
                let b = match &args[1] { Value::Int(n) => *n, _ => return Err(InterpError { message: "time_diff: second arg must be Int".to_string() }) };
                Ok(Some(Value::Int(a - b)))
            }

            // ===== Encoding operations =====
            "base64_encode" => {
                // base64_encode(s: Str) -> Str
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "base64_encode: expected Str".to_string() }) };
                Ok(Some(Value::Str(BASE64.encode(s.as_bytes()))))
            }
            "base64_decode" => {
                // base64_decode(s: Str) -> Result[Str, Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "base64_decode: expected Str".to_string() }) };
                match BASE64.decode(&s) {
                    Ok(bytes) => match String::from_utf8(bytes) {
                        Ok(decoded) => Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Str(decoded)],
                        })),
                        Err(e) => Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Err".to_string(),
                            fields: vec![Value::Str(e.to_string())],
                        })),
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "base64_encode_bytes" => {
                // base64_encode_bytes(bytes: [Int]) -> Str
                let bytes: Vec<u8> = match &args[0] {
                    Value::Array(arr) => arr.iter().map(|v| match v { Value::Int(n) => *n as u8, _ => 0 }).collect(),
                    _ => return Err(InterpError { message: "base64_encode_bytes: expected [Int]".to_string() })
                };
                Ok(Some(Value::Str(BASE64.encode(&bytes))))
            }
            "base64_decode_bytes" => {
                // base64_decode_bytes(s: Str) -> Result[[Int], Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "base64_decode_bytes: expected Str".to_string() }) };
                match BASE64.decode(&s) {
                    Ok(bytes) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "hex_encode" => {
                // hex_encode(s: Str) -> Str
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "hex_encode: expected Str".to_string() }) };
                Ok(Some(Value::Str(hex::encode(s.as_bytes()))))
            }
            "hex_decode" => {
                // hex_decode(s: Str) -> Result[Str, Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "hex_decode: expected Str".to_string() }) };
                match hex::decode(&s) {
                    Ok(bytes) => match String::from_utf8(bytes) {
                        Ok(decoded) => Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Str(decoded)],
                        })),
                        Err(e) => Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Err".to_string(),
                            fields: vec![Value::Str(e.to_string())],
                        })),
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "hex_encode_bytes" => {
                // hex_encode_bytes(bytes: [Int]) -> Str
                let bytes: Vec<u8> = match &args[0] {
                    Value::Array(arr) => arr.iter().map(|v| match v { Value::Int(n) => *n as u8, _ => 0 }).collect(),
                    _ => return Err(InterpError { message: "hex_encode_bytes: expected [Int]".to_string() })
                };
                Ok(Some(Value::Str(hex::encode(&bytes))))
            }
            "hex_decode_bytes" => {
                // hex_decode_bytes(s: Str) -> Result[[Int], Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "hex_decode_bytes: expected Str".to_string() }) };
                match hex::decode(&s) {
                    Ok(bytes) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }

            // ===== Hashing operations =====
            "sha256" => {
                // sha256(s: Str) -> Str
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "sha256: expected Str".to_string() }) };
                let mut hasher = Sha256::new();
                hasher.update(s.as_bytes());
                let result = hasher.finalize();
                Ok(Some(Value::Str(hex::encode(result))))
            }
            "sha256_bytes" => {
                // sha256_bytes(bytes: [Int]) -> Str
                let bytes: Vec<u8> = match &args[0] {
                    Value::Array(arr) => arr.iter().map(|v| match v { Value::Int(n) => *n as u8, _ => 0 }).collect(),
                    _ => return Err(InterpError { message: "sha256_bytes: expected [Int]".to_string() })
                };
                let mut hasher = Sha256::new();
                hasher.update(&bytes);
                let result = hasher.finalize();
                Ok(Some(Value::Str(hex::encode(result))))
            }
            "hash_string" => {
                // hash_string(s: Str) -> Int (fast non-crypto hash)
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "hash_string: expected Str".to_string() }) };
                use std::hash::{Hash, Hasher};
                use std::collections::hash_map::DefaultHasher;
                let mut hasher = DefaultHasher::new();
                s.hash(&mut hasher);
                Ok(Some(Value::Int(hasher.finish() as i64)))
            }

            // ===== UUID operations =====
            "uuid_v4" => {
                // uuid_v4() -> Str
                Ok(Some(Value::Str(Uuid::new_v4().to_string())))
            }
            "uuid_parse" => {
                // uuid_parse(s: Str) -> Result[Str, Str]
                let s = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "uuid_parse: expected Str".to_string() }) };
                match Uuid::parse_str(&s) {
                    Ok(uuid) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Str(uuid.to_string())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }

            // ===== Regex operations =====
            "regex_match" => {
                // regex_match(pattern, text) -> Bool
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_match: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_match: text must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => Ok(Some(Value::Bool(re.is_match(&text)))),
                    Err(_) => Ok(Some(Value::Bool(false))),
                }
            }
            "regex_find" => {
                // regex_find(pattern, text) -> Str?
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_find: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_find: text must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => match re.find(&text) {
                        Some(m) => Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "Some".to_string(),
                            fields: vec![Value::Str(m.as_str().to_string())],
                        })),
                        None => Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "None".to_string(),
                            fields: vec![],
                        })),
                    },
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "regex_find_all" => {
                // regex_find_all(pattern, text) -> [Str]
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_find_all: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_find_all: text must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => {
                        let matches: Vec<Value> = re.find_iter(&text)
                            .map(|m| Value::Str(m.as_str().to_string()))
                            .collect();
                        Ok(Some(Value::Array(matches)))
                    },
                    Err(_) => Ok(Some(Value::Array(vec![]))),
                }
            }
            "regex_replace" => {
                // regex_replace(pattern, text, replacement) -> Str
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace: text must be Str".to_string() }) };
                let replacement = match &args[2] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace: replacement must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => Ok(Some(Value::Str(re.replace(&text, replacement.as_str()).to_string()))),
                    Err(_) => Ok(Some(Value::Str(text))),
                }
            }
            "regex_replace_all" => {
                // regex_replace_all(pattern, text, replacement) -> Str
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace_all: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace_all: text must be Str".to_string() }) };
                let replacement = match &args[2] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_replace_all: replacement must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => Ok(Some(Value::Str(re.replace_all(&text, replacement.as_str()).to_string()))),
                    Err(_) => Ok(Some(Value::Str(text))),
                }
            }
            "regex_split" => {
                // regex_split(pattern, text) -> [Str]
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_split: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_split: text must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => {
                        let parts: Vec<Value> = re.split(&text)
                            .map(|s| Value::Str(s.to_string()))
                            .collect();
                        Ok(Some(Value::Array(parts)))
                    },
                    Err(_) => Ok(Some(Value::Array(vec![Value::Str(text)]))),
                }
            }
            "regex_captures" => {
                // regex_captures(pattern, text) -> [Str]?
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_captures: pattern must be Str".to_string() }) };
                let text = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_captures: text must be Str".to_string() }) };
                match Regex::new(&pattern) {
                    Ok(re) => match re.captures(&text) {
                        Some(caps) => {
                            let groups: Vec<Value> = caps.iter()
                                .map(|m| match m {
                                    Some(m) => Value::Str(m.as_str().to_string()),
                                    None => Value::Str(String::new()),
                                })
                                .collect();
                            Ok(Some(Value::Enum {
                                type_name: "Option".to_string(),
                                variant: "Some".to_string(),
                                fields: vec![Value::Array(groups)],
                            }))
                        },
                        None => Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "None".to_string(),
                            fields: vec![],
                        })),
                    },
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "regex_is_valid" => {
                // regex_is_valid(pattern) -> Bool
                let pattern = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "regex_is_valid: pattern must be Str".to_string() }) };
                Ok(Some(Value::Bool(Regex::new(&pattern).is_ok())))
            }

            // ===== Process operations =====
            "exec" => {
                // exec(cmd: Str) -> Result[(Str, Str, Int), Str]
                let cmd = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "exec: cmd must be Str".to_string() }) };
                match std::process::Command::new("sh").arg("-c").arg(&cmd).output() {
                    Ok(output) => {
                        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                        let status = output.status.code().unwrap_or(-1) as i64;
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Str(stdout), Value::Str(stderr), Value::Int(status)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "env_set" => {
                // env_set(name: Str, value: Str) -> ()
                let name = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "env_set: name must be Str".to_string() }) };
                let value = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "env_set: value must be Str".to_string() }) };
                // SAFETY: We're setting a single env var in a single-threaded interpreter context
                unsafe { std::env::set_var(&name, &value); }
                Ok(Some(Value::Unit))
            }
            "env_remove" => {
                // env_remove(name: Str) -> ()
                let name = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "env_remove: name must be Str".to_string() }) };
                // SAFETY: We're removing a single env var in a single-threaded interpreter context
                unsafe { std::env::remove_var(&name); }
                Ok(Some(Value::Unit))
            }
            "env_vars" => {
                // env_vars() -> {Str: Str}
                let mut map = HashMap::new();
                for (key, value) in std::env::vars() {
                    map.insert(key, Value::Str(value));
                }
                Ok(Some(Value::Map(map)))
            }
            "pid" => {
                // pid() -> Int
                Ok(Some(Value::Int(std::process::id() as i64)))
            }
            "cwd" => {
                // cwd() -> Str
                match std::env::current_dir() {
                    Ok(path) => Ok(Some(Value::Str(path.to_string_lossy().to_string()))),
                    Err(_) => Ok(Some(Value::Str(String::new()))),
                }
            }
            "chdir" => {
                // chdir(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "chdir: path must be Str".to_string() }) };
                match std::env::set_current_dir(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "home_dir" => {
                // home_dir() -> Str?
                match std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
                    Ok(home) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(home)],
                    })),
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "temp_dir" => {
                // temp_dir() -> Str
                Ok(Some(Value::Str(std::env::temp_dir().to_string_lossy().to_string())))
            }

            // ===== Path operations =====
            "path_join" => {
                // path_join(parts: [Str]) -> Str
                let parts: Vec<String> = match &args[0] {
                    Value::Array(arr) => arr.iter().filter_map(|v| match v {
                        Value::Str(s) => Some(s.clone()),
                        _ => None,
                    }).collect(),
                    _ => return Err(InterpError { message: "path_join: parts must be [Str]".to_string() })
                };
                let path: std::path::PathBuf = parts.iter().collect();
                Ok(Some(Value::Str(path.to_string_lossy().to_string())))
            }
            "path_parent" => {
                // path_parent(path: Str) -> Str?
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_parent: path must be Str".to_string() }) };
                let p = std::path::Path::new(&path);
                match p.parent() {
                    Some(parent) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(parent.to_string_lossy().to_string())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "path_filename" => {
                // path_filename(path: Str) -> Str?
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_filename: path must be Str".to_string() }) };
                let p = std::path::Path::new(&path);
                match p.file_name() {
                    Some(name) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(name.to_string_lossy().to_string())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "path_stem" => {
                // path_stem(path: Str) -> Str?
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_stem: path must be Str".to_string() }) };
                let p = std::path::Path::new(&path);
                match p.file_stem() {
                    Some(stem) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(stem.to_string_lossy().to_string())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "path_extension" => {
                // path_extension(path: Str) -> Str?
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_extension: path must be Str".to_string() }) };
                let p = std::path::Path::new(&path);
                match p.extension() {
                    Some(ext) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(ext.to_string_lossy().to_string())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "path_is_absolute" => {
                // path_is_absolute(path: Str) -> Bool
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_is_absolute: path must be Str".to_string() }) };
                Ok(Some(Value::Bool(std::path::Path::new(&path).is_absolute())))
            }
            "path_is_relative" => {
                // path_is_relative(path: Str) -> Bool
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_is_relative: path must be Str".to_string() }) };
                Ok(Some(Value::Bool(std::path::Path::new(&path).is_relative())))
            }
            "path_absolute" => {
                // path_absolute(path: Str) -> Result[Str, Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "path_absolute: path must be Str".to_string() }) };
                match std::fs::canonicalize(&path) {
                    Ok(abs) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Str(abs.to_string_lossy().to_string())],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "file_is_file" => {
                // file_is_file(path: Str) -> Bool
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_is_file: path must be Str".to_string() }) };
                Ok(Some(Value::Bool(std::path::Path::new(&path).is_file())))
            }
            "file_is_dir" => {
                // file_is_dir(path: Str) -> Bool
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_is_dir: path must be Str".to_string() }) };
                Ok(Some(Value::Bool(std::path::Path::new(&path).is_dir())))
            }
            "file_size" => {
                // file_size(path: Str) -> Result[Int, Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_size: path must be Str".to_string() }) };
                match std::fs::metadata(&path) {
                    Ok(meta) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Int(meta.len() as i64)],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "dir_create" => {
                // dir_create(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "dir_create: path must be Str".to_string() }) };
                match std::fs::create_dir(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "dir_create_all" => {
                // dir_create_all(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "dir_create_all: path must be Str".to_string() }) };
                match std::fs::create_dir_all(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "dir_remove" => {
                // dir_remove(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "dir_remove: path must be Str".to_string() }) };
                match std::fs::remove_dir(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "dir_remove_all" => {
                // dir_remove_all(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "dir_remove_all: path must be Str".to_string() }) };
                match std::fs::remove_dir_all(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "dir_list" => {
                // dir_list(path: Str) -> Result[[Str], Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "dir_list: path must be Str".to_string() }) };
                match std::fs::read_dir(&path) {
                    Ok(entries) => {
                        let files: Vec<Value> = entries
                            .filter_map(|e| e.ok())
                            .map(|e| Value::Str(e.path().to_string_lossy().to_string()))
                            .collect();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Array(files)],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "file_copy" => {
                // file_copy(from: Str, to: Str) -> Result[(), Str]
                let from = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_copy: from must be Str".to_string() }) };
                let to = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_copy: to must be Str".to_string() }) };
                match std::fs::copy(&from, &to) {
                    Ok(_) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "file_move" => {
                // file_move(from: Str, to: Str) -> Result[(), Str]
                let from = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_move: from must be Str".to_string() }) };
                let to = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_move: to must be Str".to_string() }) };
                match std::fs::rename(&from, &to) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "file_remove" => {
                // file_remove(path: Str) -> Result[(), Str]
                let path = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "file_remove: path must be Str".to_string() }) };
                match std::fs::remove_file(&path) {
                    Ok(()) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Unit],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }

            // ===== HTTP operations =====
            "http_get" => {
                // http_get(url: Str) -> Result[(Int, Str, {Str: Str}), Str]
                let url = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_get: url must be Str".to_string() }) };
                match reqwest::blocking::get(&url) {
                    Ok(resp) => {
                        let status = resp.status().as_u16() as i64;
                        let headers: HashMap<String, Value> = resp.headers()
                            .iter()
                            .filter_map(|(k, v)| v.to_str().ok().map(|v| (k.to_string(), Value::Str(v.to_string()))))
                            .collect();
                        let body = resp.text().unwrap_or_default();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Int(status), Value::Str(body), Value::Map(headers)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "http_post" => {
                // http_post(url: Str, body: Str) -> Result[(Int, Str, {Str: Str}), Str]
                let url = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_post: url must be Str".to_string() }) };
                let body = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_post: body must be Str".to_string() }) };
                let client = reqwest::blocking::Client::new();
                match client.post(&url).body(body).send() {
                    Ok(resp) => {
                        let status = resp.status().as_u16() as i64;
                        let headers: HashMap<String, Value> = resp.headers()
                            .iter()
                            .filter_map(|(k, v)| v.to_str().ok().map(|v| (k.to_string(), Value::Str(v.to_string()))))
                            .collect();
                        let body = resp.text().unwrap_or_default();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Int(status), Value::Str(body), Value::Map(headers)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "http_post_json" => {
                // http_post_json(url: Str, json: Json) -> Result[(Int, Str, {Str: Str}), Str]
                let url = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_post_json: url must be Str".to_string() }) };
                let json = match &args[1] { Value::Json(j) => j.clone(), _ => return Err(InterpError { message: "http_post_json: body must be Json".to_string() }) };
                let client = reqwest::blocking::Client::new();
                match client.post(&url).json(&json).send() {
                    Ok(resp) => {
                        let status = resp.status().as_u16() as i64;
                        let headers: HashMap<String, Value> = resp.headers()
                            .iter()
                            .filter_map(|(k, v)| v.to_str().ok().map(|v| (k.to_string(), Value::Str(v.to_string()))))
                            .collect();
                        let body = resp.text().unwrap_or_default();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Int(status), Value::Str(body), Value::Map(headers)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "http_put" => {
                // http_put(url: Str, body: Str) -> Result[(Int, Str, {Str: Str}), Str]
                let url = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_put: url must be Str".to_string() }) };
                let body = match &args[1] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_put: body must be Str".to_string() }) };
                let client = reqwest::blocking::Client::new();
                match client.put(&url).body(body).send() {
                    Ok(resp) => {
                        let status = resp.status().as_u16() as i64;
                        let headers: HashMap<String, Value> = resp.headers()
                            .iter()
                            .filter_map(|(k, v)| v.to_str().ok().map(|v| (k.to_string(), Value::Str(v.to_string()))))
                            .collect();
                        let body = resp.text().unwrap_or_default();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Int(status), Value::Str(body), Value::Map(headers)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "http_delete" => {
                // http_delete(url: Str) -> Result[(Int, Str, {Str: Str}), Str]
                let url = match &args[0] { Value::Str(s) => s.clone(), _ => return Err(InterpError { message: "http_delete: url must be Str".to_string() }) };
                let client = reqwest::blocking::Client::new();
                match client.delete(&url).send() {
                    Ok(resp) => {
                        let status = resp.status().as_u16() as i64;
                        let headers: HashMap<String, Value> = resp.headers()
                            .iter()
                            .filter_map(|(k, v)| v.to_str().ok().map(|v| (k.to_string(), Value::Str(v.to_string()))))
                            .collect();
                        let body = resp.text().unwrap_or_default();
                        Ok(Some(Value::Enum {
                            type_name: "Result".to_string(),
                            variant: "Ok".to_string(),
                            fields: vec![Value::Tuple(vec![Value::Int(status), Value::Str(body), Value::Map(headers)])],
                        }))
                    },
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }

            // ===== JSON operations =====
            "json_parse" => {
                // json_parse(s: Str) -> Result[Json, Str]
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_parse: expected Str".to_string() })
                };
                match serde_json::from_str::<serde_json::Value>(&s) {
                    Ok(json) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Ok".to_string(),
                        fields: vec![Value::Json(json)],
                    })),
                    Err(e) => Ok(Some(Value::Enum {
                        type_name: "Result".to_string(),
                        variant: "Err".to_string(),
                        fields: vec![Value::Str(e.to_string())],
                    })),
                }
            }
            "json_stringify" => {
                // json_stringify(json: Json) -> Str
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_stringify: expected Json".to_string() })
                };
                Ok(Some(Value::Str(json.to_string())))
            }
            "json_stringify_pretty" => {
                // json_stringify_pretty(json: Json) -> Str
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_stringify_pretty: expected Json".to_string() })
                };
                Ok(Some(Value::Str(serde_json::to_string_pretty(&json).unwrap_or_default())))
            }
            "json_get" => {
                // json_get(json: Json, key: Str) -> Json?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get: key must be Str".to_string() })
                };
                match json.get(&key) {
                    Some(v) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Json(v.clone())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_get_str" => {
                // json_get_str(json: Json, key: Str) -> Str?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get_str: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get_str: key must be Str".to_string() })
                };
                match json.get(&key).and_then(|v| v.as_str()) {
                    Some(s) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Str(s.to_string())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_get_int" => {
                // json_get_int(json: Json, key: Str) -> Int?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get_int: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get_int: key must be Str".to_string() })
                };
                match json.get(&key).and_then(|v| v.as_i64()) {
                    Some(n) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(n)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_get_float" => {
                // json_get_float(json: Json, key: Str) -> Float?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get_float: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get_float: key must be Str".to_string() })
                };
                match json.get(&key).and_then(|v| v.as_f64()) {
                    Some(n) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Float(n)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_get_bool" => {
                // json_get_bool(json: Json, key: Str) -> Bool?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get_bool: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get_bool: key must be Str".to_string() })
                };
                match json.get(&key).and_then(|v| v.as_bool()) {
                    Some(b) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Bool(b)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_get_array" => {
                // json_get_array(json: Json, key: Str) -> [Json]?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_get_array: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_get_array: key must be Str".to_string() })
                };
                match json.get(&key).and_then(|v| v.as_array()) {
                    Some(arr) => {
                        let vals: Vec<Value> = arr.iter().map(|v| Value::Json(v.clone())).collect();
                        Ok(Some(Value::Enum {
                            type_name: "Option".to_string(),
                            variant: "Some".to_string(),
                            fields: vec![Value::Array(vals)],
                        }))
                    }
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_array_get" => {
                // json_array_get(json: Json, idx: Int) -> Json?
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_array_get: expected Json".to_string() })
                };
                let idx = match &args[1] {
                    Value::Int(n) => *n as usize,
                    _ => return Err(InterpError { message: "json_array_get: index must be Int".to_string() })
                };
                match json.as_array().and_then(|arr| arr.get(idx)) {
                    Some(v) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Json(v.clone())],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "json_array_len" => {
                // json_array_len(json: Json) -> Int
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_array_len: expected Json".to_string() })
                };
                let len = json.as_array().map(|arr| arr.len()).unwrap_or(0);
                Ok(Some(Value::Int(len as i64)))
            }
            "json_keys" => {
                // json_keys(json: Json) -> [Str]
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_keys: expected Json".to_string() })
                };
                let keys: Vec<Value> = json.as_object()
                    .map(|obj| obj.keys().map(|k| Value::Str(k.clone())).collect())
                    .unwrap_or_default();
                Ok(Some(Value::Array(keys)))
            }
            "json_values" => {
                // json_values(json: Json) -> [Json]
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_values: expected Json".to_string() })
                };
                let vals: Vec<Value> = json.as_object()
                    .map(|obj| obj.values().map(|v| Value::Json(v.clone())).collect())
                    .unwrap_or_default();
                Ok(Some(Value::Array(vals)))
            }
            "json_type" => {
                // json_type(json: Json) -> Str
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_type: expected Json".to_string() })
                };
                let type_name = match &json {
                    serde_json::Value::Null => "null",
                    serde_json::Value::Bool(_) => "bool",
                    serde_json::Value::Number(_) => "number",
                    serde_json::Value::String(_) => "string",
                    serde_json::Value::Array(_) => "array",
                    serde_json::Value::Object(_) => "object",
                };
                Ok(Some(Value::Str(type_name.to_string())))
            }
            "json_is_null" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_null: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_null())))
            }
            "json_is_bool" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_bool: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_boolean())))
            }
            "json_is_number" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_number: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_number())))
            }
            "json_is_string" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_string: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_string())))
            }
            "json_is_array" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_array: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_array())))
            }
            "json_is_object" => {
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_is_object: expected Json".to_string() })
                };
                Ok(Some(Value::Bool(json.is_object())))
            }
            "json_from_str" => {
                let s = match &args[0] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_from_str: expected Str".to_string() })
                };
                Ok(Some(Value::Json(serde_json::Value::String(s))))
            }
            "json_from_int" => {
                let n = match &args[0] {
                    Value::Int(n) => *n,
                    _ => return Err(InterpError { message: "json_from_int: expected Int".to_string() })
                };
                Ok(Some(Value::Json(serde_json::Value::Number(n.into()))))
            }
            "json_from_float" => {
                let n = match &args[0] {
                    Value::Float(n) => *n,
                    _ => return Err(InterpError { message: "json_from_float: expected Float".to_string() })
                };
                let num = serde_json::Number::from_f64(n).unwrap_or_else(|| serde_json::Number::from(0));
                Ok(Some(Value::Json(serde_json::Value::Number(num))))
            }
            "json_from_bool" => {
                let b = match &args[0] {
                    Value::Bool(b) => *b,
                    _ => return Err(InterpError { message: "json_from_bool: expected Bool".to_string() })
                };
                Ok(Some(Value::Json(serde_json::Value::Bool(b))))
            }
            "json_null" => {
                Ok(Some(Value::Json(serde_json::Value::Null)))
            }
            "json_object" => {
                // json_object() -> Json (empty object)
                Ok(Some(Value::Json(serde_json::Value::Object(serde_json::Map::new()))))
            }
            "json_array" => {
                // json_array() -> Json (empty array)
                Ok(Some(Value::Json(serde_json::Value::Array(vec![]))))
            }
            "json_set" => {
                // json_set(json: Json, key: Str, value: Json) -> Json
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_set: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_set: key must be Str".to_string() })
                };
                let value = match &args[2] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_set: value must be Json".to_string() })
                };
                let mut obj = json.as_object().cloned().unwrap_or_else(serde_json::Map::new);
                obj.insert(key, value);
                Ok(Some(Value::Json(serde_json::Value::Object(obj))))
            }
            "json_has" => {
                // json_has(json: Json, key: Str) -> Bool
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_has: expected Json".to_string() })
                };
                let key = match &args[1] {
                    Value::Str(s) => s.clone(),
                    _ => return Err(InterpError { message: "json_has: key must be Str".to_string() })
                };
                let has = json.as_object().map(|obj| obj.contains_key(&key)).unwrap_or(false);
                Ok(Some(Value::Bool(has)))
            }
            "json_to_value" => {
                // json_to_value(json: Json) -> native FORMA value
                // Converts JSON to native FORMA types where possible
                let json = match &args[0] {
                    Value::Json(j) => j.clone(),
                    _ => return Err(InterpError { message: "json_to_value: expected Json".to_string() })
                };
                fn json_to_forma_value(j: &serde_json::Value) -> Value {
                    match j {
                        serde_json::Value::Null => Value::Unit,
                        serde_json::Value::Bool(b) => Value::Bool(*b),
                        serde_json::Value::Number(n) => {
                            if let Some(i) = n.as_i64() {
                                Value::Int(i)
                            } else if let Some(f) = n.as_f64() {
                                Value::Float(f)
                            } else {
                                Value::Int(0)
                            }
                        }
                        serde_json::Value::String(s) => Value::Str(s.clone()),
                        serde_json::Value::Array(arr) => {
                            Value::Array(arr.iter().map(json_to_forma_value).collect())
                        }
                        serde_json::Value::Object(obj) => {
                            let map: HashMap<String, Value> = obj.iter()
                                .map(|(k, v)| (k.clone(), json_to_forma_value(v)))
                                .collect();
                            Value::Map(map)
                        }
                    }
                }
                Ok(Some(json_to_forma_value(&json)))
            }

            // ===== Sorting operations =====
            "sort_ints" => {
                // sort_ints(arr: [Int]) -> [Int]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sort_ints: expected array".to_string() })
                };
                let mut ints: Vec<i64> = arr.iter()
                    .filter_map(|v| v.as_int())
                    .collect();
                ints.sort();
                Ok(Some(Value::Array(ints.into_iter().map(Value::Int).collect())))
            }
            "sort_ints_desc" => {
                // sort_ints_desc(arr: [Int]) -> [Int]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sort_ints_desc: expected array".to_string() })
                };
                let mut ints: Vec<i64> = arr.iter()
                    .filter_map(|v| v.as_int())
                    .collect();
                ints.sort_by(|a, b| b.cmp(a));
                Ok(Some(Value::Array(ints.into_iter().map(Value::Int).collect())))
            }
            "sort_floats" => {
                // sort_floats(arr: [Float]) -> [Float]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sort_floats: expected array".to_string() })
                };
                let mut floats: Vec<f64> = arr.iter()
                    .filter_map(|v| v.as_float())
                    .collect();
                floats.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
                Ok(Some(Value::Array(floats.into_iter().map(Value::Float).collect())))
            }
            "sort_strings" => {
                // sort_strings(arr: [Str]) -> [Str]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sort_strings: expected array".to_string() })
                };
                let mut strs: Vec<String> = arr.iter()
                    .filter_map(|v| match v {
                        Value::Str(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect();
                strs.sort();
                Ok(Some(Value::Array(strs.into_iter().map(Value::Str).collect())))
            }
            "sort_strings_desc" => {
                // sort_strings_desc(arr: [Str]) -> [Str]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sort_strings_desc: expected array".to_string() })
                };
                let mut strs: Vec<String> = arr.iter()
                    .filter_map(|v| match v {
                        Value::Str(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect();
                strs.sort_by(|a, b| b.cmp(a));
                Ok(Some(Value::Array(strs.into_iter().map(Value::Str).collect())))
            }
            "reverse" => {
                // reverse(arr: [T]) -> [T]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "reverse: expected array".to_string() })
                };
                let mut result = arr;
                result.reverse();
                Ok(Some(Value::Array(result)))
            }
            "shuffle" => {
                // shuffle(arr: [T]) -> [T]
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "shuffle: expected array".to_string() })
                };
                let mut result = arr;
                let mut rng = rand::thread_rng();
                for i in (1..result.len()).rev() {
                    let j = rng.gen_range(0..=i);
                    result.swap(i, j);
                }
                Ok(Some(Value::Array(result)))
            }
            "min_of" => {
                // min_of(arr: [Int]) -> Int?
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "min_of: expected array".to_string() })
                };
                let ints: Vec<i64> = arr.iter().filter_map(|v| v.as_int()).collect();
                match ints.iter().min() {
                    Some(&min) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(min)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "max_of" => {
                // max_of(arr: [Int]) -> Int?
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "max_of: expected array".to_string() })
                };
                let ints: Vec<i64> = arr.iter().filter_map(|v| v.as_int()).collect();
                match ints.iter().max() {
                    Some(&max) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(max)],
                    })),
                    None => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }
            "sum_of" => {
                // sum_of(arr: [Int]) -> Int
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "sum_of: expected array".to_string() })
                };
                let sum: i64 = arr.iter().filter_map(|v| v.as_int()).sum();
                Ok(Some(Value::Int(sum)))
            }
            "binary_search" => {
                // binary_search(arr: [Int], target: Int) -> Int?
                // Returns index if found
                let arr = match &args[0] {
                    Value::Array(arr) => arr.clone(),
                    _ => return Err(InterpError { message: "binary_search: expected array".to_string() })
                };
                let target = match &args[1] {
                    Value::Int(n) => *n,
                    _ => return Err(InterpError { message: "binary_search: target must be Int".to_string() })
                };
                let ints: Vec<i64> = arr.iter().filter_map(|v| v.as_int()).collect();
                match ints.binary_search(&target) {
                    Ok(idx) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "Some".to_string(),
                        fields: vec![Value::Int(idx as i64)],
                    })),
                    Err(_) => Ok(Some(Value::Enum {
                        type_name: "Option".to_string(),
                        variant: "None".to_string(),
                        fields: vec![],
                    })),
                }
            }

            // Not a built-in function
            _ => Ok(None)
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

            Rvalue::Enum { type_name, variant, fields } => {
                let field_vals: Vec<Value> = fields
                    .iter()
                    .map(|f| self.eval_operand(f))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Enum {
                    type_name: type_name.clone(),
                    variant: variant.clone(),
                    fields: field_vals,
                })
            }

            Rvalue::Discriminant(local) => {
                let frame = self.call_stack.last().unwrap();
                let val = frame.locals.get(local).cloned().unwrap_or(Value::Unit);
                match val {
                    Value::Enum { variant, .. } => {
                        let disc = match variant.as_str() {
                            "None" => 0,
                            "Some" => 1,
                            "Ok" => 0,
                            "Err" => 1,
                            // For user-defined enums, use a simple hash-based discriminant
                            _ => {
                                // Simple hash: sum of character codes
                                variant.bytes().fold(0i64, |acc, b| acc + b as i64)
                            }
                        };
                        Ok(Value::Int(disc))
                    }
                    _ => Err(InterpError {
                        message: "discriminant of non-enum".to_string(),
                    }),
                }
            }

            Rvalue::EnumField(local, idx) => {
                let frame = self.call_stack.last().unwrap();
                let val = frame.locals.get(local).cloned().unwrap_or(Value::Unit);
                match val {
                    Value::Enum { fields, .. } => {
                        fields.get(*idx).cloned().ok_or_else(|| InterpError {
                            message: format!("enum field {} out of bounds", idx),
                        })
                    }
                    _ => Err(InterpError {
                        message: "field access on non-enum".to_string(),
                    }),
                }
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

            Rvalue::Cast(op, target_ty) => {
                let value = self.eval_operand(op)?;
                self.cast_value(value, target_ty)
            }

            Rvalue::Closure { func_name, captures } => {
                let capture_vals: Vec<Value> = captures
                    .iter()
                    .map(|c| self.eval_operand(c))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Closure {
                    func_name: func_name.clone(),
                    captures: capture_vals,
                })
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

    /// Cast a value to a target type.
    /// The interpreter uses i64 for all integers and f64 for all floats,
    /// but casts apply truncation/wrapping to simulate the target type.
    fn cast_value(&self, value: Value, target: &Ty) -> Result<Value, InterpError> {
        match (&value, target) {
            // Int to signed integers (apply truncation)
            (Value::Int(n), Ty::I8) => Ok(Value::Int((*n as i8) as i64)),
            (Value::Int(n), Ty::I16) => Ok(Value::Int((*n as i16) as i64)),
            (Value::Int(n), Ty::I32) => Ok(Value::Int((*n as i32) as i64)),
            (Value::Int(n), Ty::I64 | Ty::Int | Ty::Isize) => Ok(Value::Int(*n)),

            // Int to unsigned integers (apply wrapping)
            (Value::Int(n), Ty::U8) => Ok(Value::Int((*n as u8) as i64)),
            (Value::Int(n), Ty::U16) => Ok(Value::Int((*n as u16) as i64)),
            (Value::Int(n), Ty::U32) => Ok(Value::Int((*n as u32) as i64)),
            (Value::Int(n), Ty::U64 | Ty::UInt | Ty::Usize) => Ok(Value::Int((*n as u64) as i64)),

            // Int to float
            (Value::Int(n), Ty::F32) => Ok(Value::Float((*n as f32) as f64)),
            (Value::Int(n), Ty::F64 | Ty::Float) => Ok(Value::Float(*n as f64)),

            // Float to int (truncate)
            (Value::Float(f), Ty::I8) => Ok(Value::Int((*f as i8) as i64)),
            (Value::Float(f), Ty::I16) => Ok(Value::Int((*f as i16) as i64)),
            (Value::Float(f), Ty::I32) => Ok(Value::Int((*f as i32) as i64)),
            (Value::Float(f), Ty::I64 | Ty::Int | Ty::Isize) => Ok(Value::Int(*f as i64)),
            (Value::Float(f), Ty::U8) => Ok(Value::Int((*f as u8) as i64)),
            (Value::Float(f), Ty::U16) => Ok(Value::Int((*f as u16) as i64)),
            (Value::Float(f), Ty::U32) => Ok(Value::Int((*f as u32) as i64)),
            (Value::Float(f), Ty::U64 | Ty::UInt | Ty::Usize) => Ok(Value::Int((*f as u64) as i64)),

            // Float to float
            (Value::Float(f), Ty::F32) => Ok(Value::Float((*f as f32) as f64)),
            (Value::Float(f), Ty::F64 | Ty::Float) => Ok(Value::Float(*f)),

            // Bool to int
            (Value::Bool(b), ty) if ty.is_integer() => Ok(Value::Int(if *b { 1 } else { 0 })),

            // Char to int
            (Value::Char(c), ty) if ty.is_integer() => Ok(Value::Int(*c as i64)),

            // Int to char
            (Value::Int(n), Ty::Char) => Ok(Value::Char((*n as u8) as char)),

            // Same type or unhandled - pass through
            _ => Ok(value),
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

            // Float comparison
            (BinOp::Eq, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (BinOp::Ne, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a != b)),
            (BinOp::Lt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (BinOp::Le, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (BinOp::Gt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (BinOp::Ge, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),

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

            // Char comparison
            (BinOp::Eq, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a == b)),
            (BinOp::Ne, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a != b)),
            (BinOp::Lt, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a < b)),
            (BinOp::Le, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a <= b)),
            (BinOp::Gt, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a > b)),
            (BinOp::Ge, Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a >= b)),

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

    // Note: FORMA uses | for pipeline, so bitwise operators are not directly
    // available in expression syntax. Skipping bitwise tests for now.

    // ========================================================================
    // Enum tests
    // ========================================================================

    #[test]
    fn test_option_some() {
        let result = run_source(
            r#"
f wrap(x: Int) -> Int?
    Some(x)

f main() -> Int? = wrap(42)
"#,
        )
        .unwrap();
        assert_eq!(
            result,
            Value::Enum {
                type_name: "Option".to_string(),
                variant: "Some".to_string(),
                fields: vec![Value::Int(42)],
            }
        );
    }

    #[test]
    fn test_option_none() {
        let result = run_source(
            r#"
f nothing() -> Int?
    None

f main() -> Int? = nothing()
"#,
        )
        .unwrap();
        assert_eq!(
            result,
            Value::Enum {
                type_name: "Option".to_string(),
                variant: "None".to_string(),
                fields: vec![],
            }
        );
    }

    #[test]
    fn test_option_pattern_matching_some() {
        let result = run_source(
            r#"
f unwrap_or(opt: Int?, default: Int) -> Int
    m opt
        Some(x) -> x
        None -> default

f main() -> Int = unwrap_or(Some(42), 0)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_option_pattern_matching_none() {
        let result = run_source(
            r#"
f unwrap_or(opt: Int?, default: Int) -> Int
    m opt
        Some(x) -> x
        None -> default

f main() -> Int = unwrap_or(None, 99)
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(99));
    }

    #[test]
    fn test_result_ok() {
        let result = run_source(
            r#"
f success(x: Int) -> Int!Str
    Ok(x)

f main() -> Int!Str = success(100)
"#,
        )
        .unwrap();
        assert_eq!(
            result,
            Value::Enum {
                type_name: "Result".to_string(),
                variant: "Ok".to_string(),
                fields: vec![Value::Int(100)],
            }
        );
    }

    #[test]
    fn test_result_err() {
        let result = run_source(
            r#"
f fail(msg: Str) -> Int!Str
    Err(msg)

f main() -> Int!Str = fail("error")
"#,
        )
        .unwrap();
        assert_eq!(
            result,
            Value::Enum {
                type_name: "Result".to_string(),
                variant: "Err".to_string(),
                fields: vec![Value::Str("error".to_string())],
            }
        );
    }

    #[test]
    fn test_result_pattern_matching() {
        let result = run_source(
            r#"
f get_value(res: Int!Str) -> Int
    m res
        Ok(x) -> x
        Err(e) -> 0

f main() -> Int = get_value(Ok(42))
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_nested_option() {
        let result = run_source(
            r#"
f double_if_some(opt: Int?) -> Int
    m opt
        Some(x) -> x * 2
        None -> 0

f main() -> Int = double_if_some(Some(21))
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_enum_in_function_composition() {
        let result = run_source(
            r#"
f add_one(x: Int) -> Int = x + 1

f safe_add_one(opt: Int?) -> Int?
    m opt
        Some(x) -> Some(add_one(x))
        None -> None

f unwrap(opt: Int?) -> Int
    m opt
        Some(x) -> x
        None -> 0

f main() -> Int = unwrap(safe_add_one(Some(41)))
"#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(42));
    }
}
