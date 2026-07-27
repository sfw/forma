//! Authoritative semantic metadata for compiler-provided builtins.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
pub enum Effect {
    ReadFile,
    WriteFile,
    Network,
    Process,
    Environment,
    Unsafe,
    Console,
    Clock,
    Random,
    Concurrency,
    Panic,
    Database,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
pub enum Capability {
    Read,
    Write,
    Network,
    Exec,
    Env,
    Unsafe,
}

impl Capability {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Read => "read",
            Self::Write => "write",
            Self::Network => "network",
            Self::Exec => "exec",
            Self::Env => "env",
            Self::Unsafe => "unsafe",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum OwnershipMode {
    Owned,
    Shared,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum Support {
    Supported,
    Unsupported,
    Experimental,
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuiltinSpec {
    pub name: &'static str,
    pub type_parameters: &'static [&'static str],
    pub parameter_modes: &'static [OwnershipMode],
    pub effects: &'static [Effect],
    pub capability: Option<Capability>,
    pub pure: bool,
    pub interpreter: Support,
    pub native: Support,
    pub verification: Support,
    pub documentation: &'static str,
}

#[derive(Debug, Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuiltinMetadata {
    pub name: &'static str,
    pub signature: String,
    pub type_parameters: Vec<String>,
    pub parameter_modes: Vec<OwnershipMode>,
    pub effects: &'static [Effect],
    pub capability: Option<Capability>,
    pub pure: bool,
    pub interpreter: Support,
    pub native: Support,
    pub verification: Support,
    pub documentation: &'static str,
}

macro_rules! gated {
    ($name:literal, $capability:ident, $effect:ident) => {
        BuiltinSpec {
            name: $name,
            type_parameters: &[],
            parameter_modes: &[],
            effects: &[Effect::$effect],
            capability: Some(Capability::$capability),
            pure: false,
            interpreter: Support::Supported,
            native: Support::Unsupported,
            verification: Support::Unsupported,
            documentation: concat!("Capability-gated builtin `", $name, "`."),
        }
    };
}

macro_rules! pure {
    ($name:literal) => {
        BuiltinSpec {
            name: $name,
            type_parameters: &[],
            parameter_modes: &[],
            effects: &[],
            capability: None,
            pure: true,
            interpreter: Support::Supported,
            native: Support::Experimental,
            verification: Support::Experimental,
            documentation: concat!("Compiler-provided builtin `", $name, "`."),
        }
    };
}

macro_rules! effectful {
    ($name:literal, $effect:ident) => {
        BuiltinSpec {
            name: $name,
            type_parameters: &[],
            parameter_modes: &[],
            effects: &[Effect::$effect],
            capability: None,
            pure: false,
            interpreter: Support::Supported,
            native: Support::Experimental,
            verification: Support::Unsupported,
            documentation: concat!("Effectful compiler-provided builtin `", $name, "`."),
        }
    };
}

pub const BUILTINS: &[BuiltinSpec] = &[
    gated!("file_is_dir", Read, ReadFile),
    gated!("file_is_file", Read, ReadFile),
    gated!("file_size", Read, ReadFile),
    gated!("dns_lookup", Network, Network),
    gated!("dns_reverse_lookup", Network, Network),
    gated!("tcp_accept", Network, Network),
    gated!("tcp_close", Network, Network),
    gated!("tcp_listener_close", Network, Network),
    gated!("tcp_local_addr", Network, Network),
    gated!("tcp_peer_addr", Network, Network),
    gated!("tcp_read", Network, Network),
    gated!("tcp_read_exact", Network, Network),
    gated!("tcp_read_line", Network, Network),
    gated!("tcp_set_timeout", Network, Network),
    gated!("tcp_write", Network, Network),
    gated!("tcp_write_all", Network, Network),
    gated!("udp_close", Network, Network),
    gated!("udp_connect", Network, Network),
    gated!("udp_recv", Network, Network),
    gated!("udp_recv_from", Network, Network),
    gated!("udp_send", Network, Network),
    gated!("udp_send_to", Network, Network),
    gated!("tls_close", Network, Network),
    gated!("tls_read", Network, Network),
    gated!("tls_write", Network, Network),
    gated!("db_close", Write, Database),
    gated!("db_execute", Write, Database),
    gated!("db_execute_prepared", Write, Database),
    gated!("db_open_memory", Write, Database),
    gated!("db_prepare", Write, Database),
    gated!("db_query", Write, Database),
    gated!("db_query_one", Write, Database),
    gated!("db_query_prepared", Write, Database),
    gated!("args", Env, Environment),
    gated!("cwd", Env, Environment),
    gated!("home_dir", Env, Environment),
    gated!("temp_dir", Env, Environment),
    gated!("pid", Env, Environment),
    gated!("file_read", Read, ReadFile),
    gated!("file_read_bytes", Read, ReadFile),
    gated!("file_exists", Read, ReadFile),
    gated!("dir_list", Read, ReadFile),
    gated!("path_resolve_within", Read, ReadFile),
    gated!("http_file_response", Read, ReadFile),
    gated!("file_write", Write, WriteFile),
    gated!("file_write_bytes", Write, WriteFile),
    gated!("file_append", Write, WriteFile),
    gated!("file_copy", Write, WriteFile),
    gated!("file_move", Write, WriteFile),
    gated!("file_remove", Write, WriteFile),
    gated!("dir_create", Write, WriteFile),
    gated!("dir_create_all", Write, WriteFile),
    gated!("dir_remove", Write, WriteFile),
    gated!("dir_remove_all", Write, WriteFile),
    gated!("chdir", Write, WriteFile),
    gated!("db_open", Write, Database),
    gated!("db_connect_postgres", Network, Database),
    gated!("http_get", Network, Network),
    gated!("http_post", Network, Network),
    gated!("http_post_json", Network, Network),
    gated!("http_request", Network, Network),
    gated!("http_request_json", Network, Network),
    gated!("http_put", Network, Network),
    gated!("http_delete", Network, Network),
    gated!("http_serve", Network, Network),
    gated!("tcp_connect", Network, Network),
    gated!("tcp_listen", Network, Network),
    gated!("udp_bind", Network, Network),
    gated!("tls_connect", Network, Network),
    gated!("exec", Exec, Process),
    gated!("process_run", Exec, Process),
    gated!("exit", Exec, Process),
    gated!("env_get", Env, Environment),
    gated!("env_set", Env, Environment),
    gated!("env_remove", Env, Environment),
    gated!("env_vars", Env, Environment),
    gated!("ptr_null", Unsafe, Unsafe),
    gated!("ptr_is_null", Unsafe, Unsafe),
    gated!("ptr_offset", Unsafe, Unsafe),
    gated!("ptr_addr", Unsafe, Unsafe),
    gated!("ptr_from_addr", Unsafe, Unsafe),
    gated!("str_to_cstr", Unsafe, Unsafe),
    gated!("cstr_to_str", Unsafe, Unsafe),
    gated!("cstr_to_str_len", Unsafe, Unsafe),
    gated!("cstr_free", Unsafe, Unsafe),
    gated!("alloc", Unsafe, Unsafe),
    gated!("alloc_zeroed", Unsafe, Unsafe),
    gated!("dealloc", Unsafe, Unsafe),
    gated!("mem_copy", Unsafe, Unsafe),
    gated!("mem_set", Unsafe, Unsafe),
];

/// Remaining interpreter builtins. Keeping this inventory beside the gated
/// registry gives every implementation backend/tooling metadata even while
/// detailed generic signatures are migrated incrementally.
pub const EFFECT_BUILTINS: &[BuiltinSpec] = &[
    effectful!("print", Console),
    effectful!("eprintln", Console),
    effectful!("debug", Console),
    effectful!("error", Console),
    effectful!("info", Console),
    effectful!("log_debug", Console),
    effectful!("log_error", Console),
    effectful!("log_info", Console),
    effectful!("log_warn", Console),
    effectful!("log_set_format", Console),
    effectful!("log_set_level", Console),
    effectful!("time_now", Clock),
    effectful!("time_now_ms", Clock),
    effectful!("time_sleep", Clock),
    effectful!("random", Random),
    effectful!("random_int", Random),
    effectful!("random_bool", Random),
    effectful!("random_choice", Random),
    effectful!("random_shuffle", Random),
    effectful!("shuffle", Random),
    effectful!("uuid_v4", Random),
    effectful!("await_all", Concurrency),
    effectful!("await_any", Concurrency),
    effectful!("channel_close", Concurrency),
    effectful!("channel_new", Concurrency),
    effectful!("channel_recv", Concurrency),
    effectful!("channel_send", Concurrency),
    effectful!("channel_try_recv", Concurrency),
    effectful!("channel_try_send", Concurrency),
    effectful!("mutex_get", Concurrency),
    effectful!("mutex_lock", Concurrency),
    effectful!("mutex_new", Concurrency),
    effectful!("mutex_set", Concurrency),
    effectful!("mutex_try_lock", Concurrency),
    effectful!("mutex_unlock", Concurrency),
    effectful!("sleep_async", Concurrency),
    effectful!("timeout", Concurrency),
    effectful!("panic", Panic),
    effectful!("assert", Panic),
];

pub const PURE_BUILTINS: &[BuiltinSpec] = &[
    pure!("abs"),
    pure!("abs_float"),
    pure!("acos"),
    pure!("all"),
    pure!("and_then"),
    pure!("any"),
    pure!("asin"),
    pure!("atan2"),
    pure!("base64_decode"),
    pure!("base64_decode_bytes"),
    pure!("base64_encode"),
    pure!("base64_encode_bytes"),
    pure!("binary_search"),
    pure!("ceil"),
    pure!("char_is_alpha"),
    pure!("char_is_alphanumeric"),
    pure!("char_is_digit"),
    pure!("char_is_whitespace"),
    pure!("char_to_int"),
    pure!("char_to_str"),
    pure!("cos"),
    pure!("duration_days"),
    pure!("duration_hours"),
    pure!("duration_minutes"),
    pure!("duration_seconds"),
    pure!("exp"),
    pure!("expect"),
    pure!("filter"),
    pure!("flatten"),
    pure!("floor"),
    pure!("from_cdouble"),
    pure!("from_cfloat"),
    pure!("from_cint"),
    pure!("from_clong"),
    pure!("from_csize"),
    pure!("from_cuint"),
    pure!("from_culong"),
    pure!("gzip_compress"),
    pure!("gzip_decompress"),
    pure!("hash_string"),
    pure!("hex_decode"),
    pure!("hex_decode_bytes"),
    pure!("hex_encode"),
    pure!("hex_encode_bytes"),
    pure!("http_json_response"),
    pure!("http_redirect"),
    pure!("http_req_form"),
    pure!("http_req_header"),
    pure!("http_req_json"),
    pure!("http_req_param"),
    pure!("http_request_new"),
    pure!("http_response"),
    pure!("http_response_with_headers"),
    pure!("i32"),
    pure!("i64"),
    pure!("int_to_char"),
    pure!("int_to_str"),
    pure!("is_err"),
    pure!("is_none"),
    pure!("is_ok"),
    pure!("is_some"),
    pure!("json_array"),
    pure!("json_array_get"),
    pure!("json_array_len"),
    pure!("json_from_bool"),
    pure!("json_from_float"),
    pure!("json_from_int"),
    pure!("json_from_str"),
    pure!("json_get"),
    pure!("json_get_array"),
    pure!("json_get_bool"),
    pure!("json_get_float"),
    pure!("json_get_int"),
    pure!("json_get_str"),
    pure!("json_has"),
    pure!("json_is_array"),
    pure!("json_is_bool"),
    pure!("json_is_null"),
    pure!("json_is_number"),
    pure!("json_is_object"),
    pure!("json_is_string"),
    pure!("json_keys"),
    pure!("json_null"),
    pure!("json_object"),
    pure!("json_parse"),
    pure!("toml_parse"),
    pure!("toml_stringify"),
    pure!("json_set"),
    pure!("json_stringify"),
    pure!("json_stringify_pretty"),
    pure!("json_to_value"),
    pure!("json_type"),
    pure!("json_values"),
    pure!("len"),
    pure!("log"),
    pure!("log10"),
    pure!("log2"),
    pure!("map"),
    pure!("map_contains"),
    pure!("map_get"),
    pure!("map_insert"),
    pure!("map_keys"),
    pure!("map_len"),
    pure!("map_new"),
    pure!("map_opt"),
    pure!("map_remove"),
    pure!("map_values"),
    pure!("max_of"),
    pure!("min_of"),
    pure!("path_absolute"),
    pure!("path_extension"),
    pure!("path_filename"),
    pure!("path_is_absolute"),
    pure!("path_is_relative"),
    pure!("path_join"),
    pure!("path_parent"),
    pure!("path_stem"),
    pure!("pow"),
    pure!("reduce"),
    pure!("regex_captures"),
    pure!("regex_find"),
    pure!("regex_find_all"),
    pure!("regex_is_valid"),
    pure!("regex_match"),
    pure!("regex_replace"),
    pure!("regex_replace_all"),
    pure!("regex_split"),
    pure!("reverse"),
    pure!("round"),
    pure!("row_get"),
    pure!("row_get_bool"),
    pure!("row_get_float"),
    pure!("row_get_int"),
    pure!("row_get_str"),
    pure!("row_is_null"),
    pure!("row_len"),
    pure!("sha256"),
    pure!("sha256_bytes"),
    pure!("sin"),
    pure!("sizeof"),
    pure!("sort_floats"),
    pure!("sort_ints"),
    pure!("sort_ints_desc"),
    pure!("sort_strings"),
    pure!("sort_strings_desc"),
    pure!("sqrt"),
    pure!("str"),
    pure!("str_char_at"),
    pure!("str_concat"),
    pure!("str_contains"),
    pure!("str_ends_with"),
    pure!("str_len"),
    pure!("str_replace"),
    pure!("str_replace_all"),
    pure!("str_slice"),
    pure!("str_split"),
    pure!("str_starts_with"),
    pure!("str_to_float"),
    pure!("str_to_int"),
    pure!("str_to_int_radix"),
    pure!("str_trim"),
    pure!("sum_of"),
    pure!("tan"),
    pure!("time_add"),
    pure!("time_day"),
    pure!("time_diff"),
    pure!("time_format"),
    pure!("time_format_iso"),
    pure!("time_format_rfc2822"),
    pure!("time_from_parts"),
    pure!("time_hour"),
    pure!("time_minute"),
    pure!("time_month"),
    pure!("time_parse"),
    pure!("time_parse_iso"),
    pure!("time_second"),
    pure!("time_sub"),
    pure!("time_weekday"),
    pure!("time_year"),
    pure!("to_cdouble"),
    pure!("to_cfloat"),
    pure!("to_cint"),
    pure!("to_clong"),
    pure!("to_csize"),
    pure!("to_cuint"),
    pure!("to_culong"),
    pure!("type_of"),
    pure!("unwrap"),
    pure!("unwrap_or"),
    pure!("uuid_parse"),
    pure!("vec_concat"),
    pure!("vec_first"),
    pure!("vec_get"),
    pure!("vec_index_of"),
    pure!("vec_last"),
    pure!("vec_len"),
    pure!("vec_new"),
    pure!("vec_pop"),
    pure!("vec_push"),
    pure!("vec_reverse"),
    pure!("vec_set"),
    pure!("vec_slice"),
    pure!("vec_sort"),
    pure!("zlib_compress"),
    pure!("zlib_decompress"),
];

pub fn get(name: &str) -> Option<&'static BuiltinSpec> {
    BUILTINS
        .iter()
        .chain(EFFECT_BUILTINS)
        .chain(PURE_BUILTINS)
        .find(|builtin| builtin.name == name)
}

pub fn all() -> impl Iterator<Item = &'static BuiltinSpec> {
    BUILTINS.iter().chain(EFFECT_BUILTINS).chain(PURE_BUILTINS)
}

/// The call convention of compiler-provided functions.
///
/// Builtins observe their inputs by default. Operations that transfer an
/// affine value into a new owner are listed explicitly. Keeping this policy in
/// the registry prevents the lowerer and tooling from independently guessing
/// whether a call copies, borrows, or moves an argument.
pub fn ownership_modes(name: &str, arity: usize) -> Vec<OwnershipMode> {
    use OwnershipMode::{Owned, Shared};

    let explicit: &[OwnershipMode] = match name {
        // Persistent collection updates consume the old collection and the
        // inserted value, returning the replacement owner.
        "vec_push" | "vec_insert" => &[Owned, Owned],
        "vec_set" => &[Owned, Shared, Owned],
        "map_set" | "map_insert" => &[Owned, Owned, Owned],
        "map_remove" => &[Owned, Shared],
        "set_add" | "set_insert" | "set_remove" => &[Owned, Owned],

        // Task/future joins discharge the handles they receive.
        "await_all" | "await_any" => &[Owned],
        "timeout" => &[Shared, Owned],

        // Closing or freeing a resource ends that ownership obligation.
        "tcp_close" | "tcp_listener_close" | "udp_close" | "tls_close" | "db_close"
        | "cstr_free" | "dealloc" => &[Owned],

        // Sending and storing transfer the payload while observing the shared
        // synchronization handle.
        "channel_send" | "channel_try_send" | "mutex_set" => &[Shared, Owned],
        _ => &[],
    };

    if explicit.is_empty() {
        vec![Shared; arity]
    } else {
        explicit
            .iter()
            .copied()
            .chain(std::iter::repeat(Shared))
            .take(arity)
            .collect()
    }
}

pub fn metadata() -> Vec<BuiltinMetadata> {
    let environment = crate::types::inference::TypeEnv::with_builtins();
    all()
        .map(|spec| {
            let scheme = environment
                .get(spec.name)
                .expect("registry/type-environment parity is tested");
            let parameter_modes = if spec.parameter_modes.is_empty() {
                match &scheme.ty {
                    crate::types::Ty::Fn(parameters, _) => {
                        ownership_modes(spec.name, parameters.len())
                    }
                    _ => Vec::new(),
                }
            } else {
                spec.parameter_modes.to_vec()
            };
            let type_parameters = if spec.type_parameters.is_empty() {
                (0..scheme.vars.len())
                    .map(|index| format!("T{index}"))
                    .collect()
            } else {
                spec.type_parameters
                    .iter()
                    .map(|name| (*name).into())
                    .collect()
            };
            BuiltinMetadata {
                name: spec.name,
                signature: scheme.to_string(),
                type_parameters,
                parameter_modes,
                effects: spec.effects,
                capability: spec.capability,
                pure: spec.pure,
                interpreter: spec.interpreter,
                native: spec.native,
                verification: spec.verification,
                documentation: spec.documentation,
            }
        })
        .collect()
}

pub type EffectSet = std::collections::HashSet<Effect>;
pub type EffectReport = std::collections::HashMap<String, EffectSet>;

/// Infer transitive function effects from resolved direct calls in MIR.
pub fn infer_effects(program: &crate::mir::Program) -> EffectReport {
    use crate::mir::Terminator;
    let mut effects: EffectReport = program
        .functions
        .keys()
        .map(|name| (name.clone(), EffectSet::new()))
        .collect();
    let mut callees: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    for (name, function) in &program.functions {
        for block in &function.blocks {
            if let Some(Terminator::Call { func, .. }) = &block.terminator {
                if let Some(builtin) = get(func) {
                    effects
                        .entry(name.clone())
                        .or_default()
                        .extend(builtin.effects.iter().copied());
                } else if program.functions.contains_key(func) {
                    callees.entry(name.clone()).or_default().push(func.clone());
                }
            }
        }
    }

    loop {
        let previous = effects.clone();
        let mut changed = false;
        for (caller, targets) in &callees {
            let inherited: EffectSet = targets
                .iter()
                .filter_map(|target| previous.get(target))
                .flat_map(|set| set.iter().copied())
                .collect();
            let caller_effects = effects.entry(caller.clone()).or_default();
            let old_len = caller_effects.len();
            caller_effects.extend(inherited);
            changed |= caller_effects.len() != old_len;
        }
        if !changed {
            break;
        }
    }
    effects
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn effectful_builtins_have_capabilities_and_are_not_pure() {
        for builtin in BUILTINS {
            assert!(
                !builtin.effects.is_empty(),
                "{} has no effect",
                builtin.name
            );
            assert!(builtin.capability.is_some(), "{} has no gate", builtin.name);
            assert!(!builtin.pure, "{} cannot be pure", builtin.name);
        }
    }

    #[test]
    fn names_are_unique() {
        let mut names = std::collections::HashSet::new();
        for builtin in all() {
            assert!(
                names.insert(builtin.name),
                "duplicate builtin {}",
                builtin.name
            );
        }
    }

    #[test]
    fn ungated_effects_are_explicitly_impure_and_not_verifiable() {
        for builtin in EFFECT_BUILTINS {
            assert!(!builtin.effects.is_empty(), "{}", builtin.name);
            assert!(!builtin.pure, "{}", builtin.name);
            assert_eq!(builtin.capability, None, "{}", builtin.name);
            assert_eq!(
                builtin.verification,
                Support::Unsupported,
                "{}",
                builtin.name
            );
        }
    }

    #[test]
    fn every_interpreter_dispatch_arm_has_registry_metadata() {
        let source = include_str!("mir/interp.rs");
        let dispatch = source
            .split("match fn_name {")
            .nth(1)
            .and_then(|tail| tail.split("fn eval_rvalue").next())
            .expect("call_builtin dispatch source");
        let arm = regex::Regex::new(r#"(?m)^\s*\"([A-Za-z_][A-Za-z0-9_]*)\"\s*=>"#).unwrap();
        for captures in arm.captures_iter(dispatch) {
            let name = &captures[1];
            if matches!(name, "Err" | "None") {
                continue;
            }
            assert!(
                get(name).is_some(),
                "builtin `{name}` has no registry entry"
            );
        }
    }

    #[test]
    fn every_registered_builtin_has_interpreter_dispatch() {
        let source = include_str!("mir/interp.rs");
        let dispatch = source
            .split("match fn_name {")
            .nth(1)
            .and_then(|tail| tail.split("fn eval_rvalue").next())
            .expect("call_builtin dispatch source");
        let quoted_name = regex::Regex::new(r#"\"([A-Za-z_][A-Za-z0-9_]*)\""#).unwrap();
        let dispatched: std::collections::HashSet<_> = quoted_name
            .captures_iter(dispatch)
            .map(|captures| captures[1].to_string())
            .collect();
        let missing: Vec<_> = all()
            .filter(|builtin| !dispatched.contains(builtin.name))
            .map(|builtin| builtin.name)
            .collect();
        assert!(
            missing.is_empty(),
            "registered builtins missing interpreter dispatch: {missing:?}"
        );
    }

    #[test]
    fn registry_and_type_environment_cover_the_same_callable_surface() {
        let environment = crate::types::inference::TypeEnv::with_builtins();
        let missing: Vec<_> = all()
            .filter(|builtin| environment.get(builtin.name).is_none())
            .map(|builtin| builtin.name)
            .collect();
        assert!(
            missing.is_empty(),
            "builtins missing type metadata: {missing:?}"
        );
    }

    #[test]
    fn generated_metadata_contains_signatures_and_ownership_modes() {
        let environment = crate::types::inference::TypeEnv::with_builtins();
        for builtin in metadata() {
            assert!(!builtin.signature.is_empty(), "{}", builtin.name);
            if let crate::types::Ty::Fn(parameters, _) = &environment
                .get(builtin.name)
                .expect("metadata requires a type")
                .ty
            {
                assert_eq!(
                    builtin.parameter_modes.len(),
                    parameters.len(),
                    "{}",
                    builtin.name
                );
            }
        }
    }

    #[test]
    fn ownership_policy_distinguishes_observation_from_transfer() {
        assert_eq!(ownership_modes("print", 1), vec![OwnershipMode::Shared]);
        assert_eq!(
            ownership_modes("vec_push", 2),
            vec![OwnershipMode::Owned, OwnershipMode::Owned]
        );
        assert_eq!(ownership_modes("await_all", 1), vec![OwnershipMode::Owned]);
        assert_eq!(ownership_modes("db_close", 1), vec![OwnershipMode::Owned]);
    }

    #[test]
    fn effects_flow_through_user_function_calls() {
        use crate::mir::{BasicBlock, BlockId, Function, Program, Terminator};
        use crate::types::Ty;

        let mut read = Function::new("read".into(), vec![], Ty::Unit);
        read.blocks.push(BasicBlock {
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
        read.blocks.push(BasicBlock {
            id: BlockId(1),
            stmts: vec![],
            terminator: Some(Terminator::Return(None)),
        });
        let mut main = Function::new("main".into(), vec![], Ty::Unit);
        main.blocks.push(BasicBlock {
            id: BlockId(0),
            stmts: vec![],
            terminator: Some(Terminator::Call {
                func: "read".into(),
                args: vec![],
                arg_pass_modes: vec![],
                dest: None,
                next: BlockId(1),
            }),
        });
        main.blocks.push(BasicBlock {
            id: BlockId(1),
            stmts: vec![],
            terminator: Some(Terminator::Return(None)),
        });
        let mut program = Program::new();
        program.functions.insert("read".into(), read);
        program.functions.insert("main".into(), main);

        let report = infer_effects(&program);
        assert!(report["read"].contains(&Effect::ReadFile));
        assert!(report["main"].contains(&Effect::ReadFile));
    }
}
