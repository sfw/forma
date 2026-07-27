# Forma 0.2 Builtin Reference

This file is generated from `docs/builtins.json`. Do not edit it by hand.
The JSON registry remains authoritative for tooling.

The current compiler registry contains **334 builtins**.

| Builtin | Signature | Parameter modes | Effects | Capability | Interpreter | Native | Verification |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `abs` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `abs_float` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `acos` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `all` | `∀?76. ([?76], (?76) -> Bool) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `alloc` | `(Int) -> *Void` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `alloc_zeroed` | `(Int) -> *Void` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `and_then` | `∀?80, ?81. (?80?, (?80) -> ?81?) -> ?81?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `any` | `∀?75. ([?75], (?75) -> Bool) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `args` | `() -> [Str]` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `asin` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `assert` | `(Bool) -> ()` | Shared | Panic | — | Supported | Experimental | Unsupported |
| `atan2` | `(Float, Float) -> Float` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `await_all` | `∀?46. ([Task[?46]]) -> [?46]` | Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `await_any` | `∀?47. ([Task[?47]]) -> ?47` | Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `base64_decode` | `(Str) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `base64_decode_bytes` | `(Str) -> [Int]!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `base64_encode` | `(Str) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `base64_encode_bytes` | `([Int]) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `binary_search` | `([Int], Int) -> Int?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `ceil` | `(Float) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `channel_close` | `∀?53. (Sender[?53]) -> ()` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `channel_new` | `∀?48. (Int) -> (Sender[?48], Receiver[?48])` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `channel_recv` | `∀?50. (Receiver[?50]) -> ?50!Str` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `channel_send` | `∀?49. (Sender[?49], ?49) -> ()!Str` | Shared, Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `channel_try_recv` | `∀?52. (Receiver[?52]) -> ?52?` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `channel_try_send` | `∀?51. (Sender[?51], ?51) -> Bool` | Shared, Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `char_is_alpha` | `(Char) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `char_is_alphanumeric` | `(Char) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `char_is_digit` | `(Char) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `char_is_whitespace` | `(Char) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `char_to_int` | `(Char) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `char_to_str` | `(Char) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `chdir` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `cos` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `cstr_free` | `(*Void) -> ()` | Owned | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `cstr_to_str` | `(*Void) -> Str` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `cstr_to_str_len` | `(*Void, Int) -> Str` | Shared, Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `cwd` | `() -> Str` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `db_close` | `(Database) -> ()` | Owned | Database | Write | Supported | Unsupported | Unsupported |
| `db_connect_postgres` | `(Str) -> Database!Str` | Shared | Database | Network | Supported | Unsupported | Unsupported |
| `db_execute` | `(Database, Str) -> Int!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_execute_prepared` | `∀?64. (Statement, [?64]) -> Int!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_open` | `(Str) -> Database!Str` | Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_open_memory` | `() -> Database!Str` | — | Database | Write | Supported | Unsupported | Unsupported |
| `db_prepare` | `(Database, Str) -> Statement!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_query` | `(Database, Str) -> [Row]!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_query_one` | `(Database, Str) -> Row?!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `db_query_prepared` | `∀?65. (Statement, [?65]) -> [Row]!Str` | Shared, Shared | Database | Write | Supported | Unsupported | Unsupported |
| `dealloc` | `(*Void, Int) -> ()` | Owned, Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `debug` | `∀?7. (?7) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `dir_create` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `dir_create_all` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `dir_list` | `(Str) -> [Str]!Str` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `dir_remove` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `dir_remove_all` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `dns_lookup` | `(Str) -> [Str]!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `dns_reverse_lookup` | `(Str) -> Str!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `duration_days` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `duration_hours` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `duration_minutes` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `duration_seconds` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `env_get` | `(Str) -> Option[Str]` | Shared | Environment | Env | Supported | Unsupported | Unsupported |
| `env_remove` | `(Str) -> ()` | Shared | Environment | Env | Supported | Unsupported | Unsupported |
| `env_set` | `(Str, Str) -> ()` | Shared, Shared | Environment | Env | Supported | Unsupported | Unsupported |
| `env_vars` | `() -> {Str:Str}` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `eprintln` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `error` | `∀?9. (?9) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `exec` | `(Str) -> (Str, Str, Int)!Str` | Shared | Process | Exec | Supported | Unsupported | Unsupported |
| `exit` | `(Int) -> !` | Shared | Process | Exec | Supported | Unsupported | Unsupported |
| `exp` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `expect` | `∀?36. (Option[?36], Str) -> ?36` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `file_append` | `(Str, Str) -> Result[(), Str]` | Shared, Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `file_copy` | `(Str, Str) -> ()!Str` | Shared, Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `file_exists` | `(Str) -> Bool` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_is_dir` | `(Str) -> Bool` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_is_file` | `(Str) -> Bool` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_move` | `(Str, Str) -> ()!Str` | Shared, Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `file_read` | `(Str) -> Result[Str, Str]` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_read_bytes` | `(Str) -> [Int]!Str` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_remove` | `(Str) -> ()!Str` | Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `file_size` | `(Str) -> Int!Str` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `file_write` | `(Str, Str) -> Result[(), Str]` | Shared, Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `file_write_bytes` | `(Str, [Int]) -> ()!Str` | Shared, Shared | WriteFile | Write | Supported | Unsupported | Unsupported |
| `filter` | `∀?72. ([?72], (?72) -> Bool) -> [?72]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `flatten` | `∀?79. (?79??) -> ?79?` | Shared | — | — | Supported | Experimental | Experimental |
| `floor` | `(Float) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `from_cdouble` | `(CDouble) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `from_cfloat` | `(CFloat) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `from_cint` | `(CInt) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `from_clong` | `(CLong) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `from_csize` | `(CSize) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `from_cuint` | `(CUInt) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `from_culong` | `(CULong) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `gzip_compress` | `(Str) -> [Int]` | Shared | — | — | Supported | Experimental | Experimental |
| `gzip_decompress` | `([Int]) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `hash_string` | `(Str) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `hex_decode` | `(Str) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `hex_decode_bytes` | `(Str) -> [Int]!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `hex_encode` | `(Str) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `hex_encode_bytes` | `([Int]) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `home_dir` | `() -> Str?` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `http_delete` | `(Str) -> (Int, Str, {Str:Str})!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_file_response` | `(Str) -> HttpResponse!Str` | Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `http_get` | `(Str) -> (Int, Str, {Str:Str})!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_json_response` | `(Int, Json) -> HttpResponse` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_post` | `(Str, Str) -> (Int, Str, {Str:Str})!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_post_json` | `(Str, Json) -> (Int, Str, {Str:Str})!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_put` | `(Str, Str) -> (Int, Str, {Str:Str})!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_redirect` | `(Str) -> HttpResponse` | Shared | — | — | Supported | Experimental | Experimental |
| `http_req_form` | `(HttpRequest) -> {Str:Str}` | Shared | — | — | Supported | Experimental | Experimental |
| `http_req_header` | `(HttpRequest, Str) -> Str?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_req_json` | `(HttpRequest) -> Json!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `http_req_param` | `(HttpRequest, Str) -> Str?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_request` | `(Str, Str, Str, Map[Str], Int, Bool) -> (Int, Str, {Str:Str})!Str` | Shared, Shared, Shared, Shared, Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_request_json` | `(Str, Str, Json, Map[Str]) -> (Int, Str, {Str:Str})!Str` | Shared, Shared, Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `http_request_new` | `(Str, Str, Str) -> HttpRequest` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_response` | `(Int, Str) -> HttpResponse` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_response_with_headers` | `(Int, Str, {Str:Str}) -> HttpResponse` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `http_serve` | `∀?63. (Int, (HttpRequest) -> HttpResponse) -> ()!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `i32` | `(Int) -> i32` | Shared | — | — | Supported | Experimental | Experimental |
| `i64` | `(Int) -> i64` | Shared | — | — | Supported | Experimental | Experimental |
| `info` | `∀?8. (?8) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `int_to_char` | `(Int) -> Char?` | Shared | — | — | Supported | Experimental | Experimental |
| `int_to_str` | `(Int) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `is_err` | `∀?42, ?43. (Result[?42, ?43]) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `is_none` | `∀?39. (Option[?39]) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `is_ok` | `∀?40, ?41. (Result[?40, ?41]) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `is_some` | `∀?38. (Option[?38]) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_array` | `() -> Json` | — | — | — | Supported | Experimental | Experimental |
| `json_array_get` | `(Json, Int) -> Json?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_array_len` | `(Json) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `json_from_bool` | `(Bool) -> Json` | Shared | — | — | Supported | Experimental | Experimental |
| `json_from_float` | `(Float) -> Json` | Shared | — | — | Supported | Experimental | Experimental |
| `json_from_int` | `(Int) -> Json` | Shared | — | — | Supported | Experimental | Experimental |
| `json_from_str` | `(Str) -> Json` | Shared | — | — | Supported | Experimental | Experimental |
| `json_get` | `(Json, Str) -> Json?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_get_array` | `(Json, Str) -> [Json]?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_get_bool` | `(Json, Str) -> Bool?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_get_float` | `(Json, Str) -> Float?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_get_int` | `(Json, Str) -> Int?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_get_str` | `(Json, Str) -> Str?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_has` | `(Json, Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_is_array` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_is_bool` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_is_null` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_is_number` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_is_object` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_is_string` | `(Json) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `json_keys` | `(Json) -> [Str]` | Shared | — | — | Supported | Experimental | Experimental |
| `json_null` | `() -> Json` | — | — | — | Supported | Experimental | Experimental |
| `json_object` | `() -> Json` | — | — | — | Supported | Experimental | Experimental |
| `json_parse` | `(Str) -> Json!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `json_set` | `(Json, Str, Json) -> Json` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `json_stringify` | `(Json) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `json_stringify_pretty` | `(Json) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `json_to_value` | `∀?60. (Json) -> ?60` | Shared | — | — | Supported | Experimental | Experimental |
| `json_type` | `(Json) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `json_values` | `(Json) -> [Json]` | Shared | — | — | Supported | Experimental | Experimental |
| `len` | `∀?12. ([?12]) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `log` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `log10` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `log2` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `log_debug` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `log_error` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `log_info` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `log_set_format` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `log_set_level` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `log_warn` | `(Str) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `map` | `∀?70, ?71. ([?70], (?70) -> ?71) -> [?71]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `map_contains` | `∀?27. (Map[?27], Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `map_get` | `∀?25. (Map[?25], Str) -> ?25?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `map_insert` | `∀?26. (Map[?26], Str, ?26) -> Map[?26]` | Owned, Owned, Owned | — | — | Supported | Experimental | Experimental |
| `map_keys` | `∀?29. (Map[?29]) -> [Str]` | Shared | — | — | Supported | Experimental | Experimental |
| `map_len` | `∀?24. (Map[?24]) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `map_new` | `∀?23. () -> Map[?23]` | — | — | — | Supported | Experimental | Experimental |
| `map_opt` | `∀?77, ?78. (?77?, (?77) -> ?78) -> ?78?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `map_remove` | `∀?28. (Map[?28], Str) -> (Map[?28], ?28?)` | Owned, Shared | — | — | Supported | Experimental | Experimental |
| `map_values` | `∀?30. (Map[?30]) -> [?30]` | Shared | — | — | Supported | Experimental | Experimental |
| `max_of` | `([Int]) -> Int?` | Shared | — | — | Supported | Experimental | Experimental |
| `mem_copy` | `(*Void, *Void, Int) -> ()` | Shared, Shared, Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `mem_set` | `(*Void, Int, Int) -> ()` | Shared, Shared, Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `min_of` | `([Int]) -> Int?` | Shared | — | — | Supported | Experimental | Experimental |
| `mutex_get` | `∀?58. (MutexGuard[?58]) -> ?58` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `mutex_lock` | `∀?55. (Mutex[?55]) -> MutexGuard[?55]` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `mutex_new` | `∀?54. (?54) -> Mutex[?54]` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `mutex_set` | `∀?59. (MutexGuard[?59], ?59) -> ()` | Shared, Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `mutex_try_lock` | `∀?56. (Mutex[?56]) -> MutexGuard[?56]?` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `mutex_unlock` | `∀?57. (MutexGuard[?57]) -> ()` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `panic` | `(Str) -> !` | Shared | Panic | — | Supported | Experimental | Unsupported |
| `path_absolute` | `(Str) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `path_extension` | `(Str) -> Str?` | Shared | — | — | Supported | Experimental | Experimental |
| `path_filename` | `(Str) -> Str?` | Shared | — | — | Supported | Experimental | Experimental |
| `path_is_absolute` | `(Str) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `path_is_relative` | `(Str) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `path_join` | `([Str]) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `path_parent` | `(Str) -> Str?` | Shared | — | — | Supported | Experimental | Experimental |
| `path_resolve_within` | `(Str, Str) -> Str!Str` | Shared, Shared | ReadFile | Read | Supported | Unsupported | Unsupported |
| `path_stem` | `(Str) -> Str?` | Shared | — | — | Supported | Experimental | Experimental |
| `pid` | `() -> Int` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `pow` | `(Float, Float) -> Float` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `print` | `∀?6. (?6) -> ()` | Shared | Console | — | Supported | Experimental | Unsupported |
| `process_run` | `(Str, [Str], Str, Map[Str], [Str], Int, Int) -> (Str, Str, Int)!Str` | Shared, Shared, Shared, Shared, Shared, Shared, Shared | Process | Exec | Supported | Unsupported | Unsupported |
| `ptr_addr` | `(*Void) -> Int` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `ptr_from_addr` | `(Int) -> *Void` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `ptr_is_null` | `(*Void) -> Bool` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `ptr_null` | `() -> *Void` | — | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `ptr_offset` | `(*Void, Int) -> *Void` | Shared, Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `random` | `() -> Float` | — | Random | — | Supported | Experimental | Unsupported |
| `random_bool` | `() -> Bool` | — | Random | — | Supported | Experimental | Unsupported |
| `random_choice` | `∀?44. ([?44]) -> ?44` | Shared | Random | — | Supported | Experimental | Unsupported |
| `random_int` | `(Int, Int) -> Int` | Shared, Shared | Random | — | Supported | Experimental | Unsupported |
| `random_shuffle` | `∀?67. ([?67]) -> [?67]` | Shared | Random | — | Supported | Experimental | Unsupported |
| `reduce` | `∀?73, ?74. ([?73], ?74, (?74, ?73) -> ?74) -> ?74` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_captures` | `(Str, Str) -> [Str]?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_find` | `(Str, Str) -> Str?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_find_all` | `(Str, Str) -> [Str]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_is_valid` | `(Str) -> Bool` | Shared | — | — | Supported | Experimental | Experimental |
| `regex_match` | `(Str, Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_replace` | `(Str, Str, Str) -> Str` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_replace_all` | `(Str, Str, Str) -> Str` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `regex_split` | `(Str, Str) -> [Str]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `reverse` | `∀?61. ([?61]) -> [?61]` | Shared | — | — | Supported | Experimental | Experimental |
| `round` | `(Float) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `row_get` | `∀?66. (Row, Int) -> ?66?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_get_bool` | `(Row, Int) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_get_float` | `(Row, Int) -> Float` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_get_int` | `(Row, Int) -> Int` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_get_str` | `(Row, Int) -> Str` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_is_null` | `(Row, Int) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `row_len` | `(Row) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `sha256` | `(Str) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `sha256_bytes` | `([Int]) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `shuffle` | `∀?62. ([?62]) -> [?62]` | Shared | Random | — | Supported | Experimental | Unsupported |
| `sin` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `sizeof` | `(Str) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `sleep_async` | `(Int) -> Future[()]` | Shared | Concurrency | — | Supported | Experimental | Unsupported |
| `sort_floats` | `([Float]) -> [Float]` | Shared | — | — | Supported | Experimental | Experimental |
| `sort_ints` | `([Int]) -> [Int]` | Shared | — | — | Supported | Experimental | Experimental |
| `sort_ints_desc` | `([Int]) -> [Int]` | Shared | — | — | Supported | Experimental | Experimental |
| `sort_strings` | `([Str]) -> [Str]` | Shared | — | — | Supported | Experimental | Experimental |
| `sort_strings_desc` | `([Str]) -> [Str]` | Shared | — | — | Supported | Experimental | Experimental |
| `sqrt` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `str` | `∀?10. (?10) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `str_char_at` | `(Str, Int) -> Char?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_concat` | `(Str, Str) -> Str` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_contains` | `(Str, Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_ends_with` | `(Str, Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_len` | `(Str) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `str_replace` | `(Str, Str, Str) -> Str` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_replace_all` | `(Str, Str, Str) -> Str` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_slice` | `(Str, Int, Int) -> Str` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_split` | `(Str, Str) -> [Str]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_starts_with` | `(Str, Str) -> Bool` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_to_cstr` | `(Str) -> *Void` | Shared | Unsafe | Unsafe | Supported | Unsupported | Unsupported |
| `str_to_float` | `(Str) -> Float?` | Shared | — | — | Supported | Experimental | Experimental |
| `str_to_int` | `(Str) -> Int?` | Shared | — | — | Supported | Experimental | Experimental |
| `str_to_int_radix` | `(Str, Int) -> Int?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `str_trim` | `(Str) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `sum_of` | `([Int]) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `tan` | `(Float) -> Float` | Shared | — | — | Supported | Experimental | Experimental |
| `tcp_accept` | `(TcpListener) -> TcpStream!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_close` | `(TcpStream) -> ()` | Owned | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_connect` | `(Str, Int) -> TcpStream!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_listen` | `(Str, Int) -> TcpListener!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_listener_close` | `(TcpListener) -> ()` | Owned | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_local_addr` | `(TcpStream) -> Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_peer_addr` | `(TcpStream) -> Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_read` | `(TcpStream, Int) -> Str!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_read_exact` | `(TcpStream, Int) -> Str!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_read_line` | `(TcpStream) -> Str!Str` | Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_set_timeout` | `(TcpStream, Int) -> ()` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_write` | `(TcpStream, Str) -> Int!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tcp_write_all` | `(TcpStream, Str) -> ()!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `temp_dir` | `() -> Str` | — | Environment | Env | Supported | Unsupported | Unsupported |
| `time_add` | `(Int, Int) -> Int` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_day` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_diff` | `(Int, Int) -> Int` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_format` | `(Int, Str) -> Str` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_format_iso` | `(Int) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `time_format_rfc2822` | `(Int) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `time_from_parts` | `(Int, Int, Int, Int, Int, Int) -> Int` | Shared, Shared, Shared, Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_hour` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_minute` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_month` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_now` | `() -> Int` | — | Clock | — | Supported | Experimental | Unsupported |
| `time_now_ms` | `() -> Int` | — | Clock | — | Supported | Experimental | Unsupported |
| `time_parse` | `(Str, Str) -> Int!Str` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_parse_iso` | `(Str) -> Int!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `time_second` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_sleep` | `(Int) -> ()` | Shared | Clock | — | Supported | Experimental | Unsupported |
| `time_sub` | `(Int, Int) -> Int` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `time_weekday` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `time_year` | `(Int) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `timeout` | `∀?45. (Int, Future[?45]) -> ?45!Str` | Shared, Owned | Concurrency | — | Supported | Experimental | Unsupported |
| `tls_close` | `(TlsStream) -> ()` | Owned | Network | Network | Supported | Unsupported | Unsupported |
| `tls_connect` | `(Str, Int) -> TlsStream!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tls_read` | `(TlsStream, Int) -> Str!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `tls_write` | `(TlsStream, Str) -> Int!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `to_cdouble` | `(Float) -> CDouble` | Shared | — | — | Supported | Experimental | Experimental |
| `to_cfloat` | `(Float) -> CFloat` | Shared | — | — | Supported | Experimental | Experimental |
| `to_cint` | `(Int) -> CInt` | Shared | — | — | Supported | Experimental | Experimental |
| `to_clong` | `(Int) -> CLong` | Shared | — | — | Supported | Experimental | Experimental |
| `to_csize` | `(Int) -> CSize` | Shared | — | — | Supported | Experimental | Experimental |
| `to_cuint` | `(Int) -> CUInt` | Shared | — | — | Supported | Experimental | Experimental |
| `to_culong` | `(Int) -> CULong` | Shared | — | — | Supported | Experimental | Experimental |
| `toml_parse` | `(Str) -> Json!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `toml_stringify` | `(Json) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `type_of` | `∀?34. (?34) -> Str` | Shared | — | — | Supported | Experimental | Experimental |
| `udp_bind` | `(Str, Int) -> UdpSocket!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `udp_close` | `(UdpSocket) -> ()` | Owned | Network | Network | Supported | Unsupported | Unsupported |
| `udp_connect` | `(UdpSocket, Str, Int) -> ()!Str` | Shared, Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `udp_recv` | `(UdpSocket, Int) -> Str!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `udp_recv_from` | `(UdpSocket, Int) -> (Str, Str, Int)!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `udp_send` | `(UdpSocket, Str) -> Int!Str` | Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `udp_send_to` | `(UdpSocket, Str, Int, Str) -> Int!Str` | Shared, Shared, Shared, Shared | Network | Network | Supported | Unsupported | Unsupported |
| `unwrap` | `∀?35. (Option[?35]) -> ?35` | Shared | — | — | Supported | Experimental | Experimental |
| `unwrap_or` | `∀?37. (Option[?37], ?37) -> ?37` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `uuid_parse` | `(Str) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |
| `uuid_v4` | `() -> Str` | — | Random | — | Supported | Experimental | Unsupported |
| `vec_concat` | `∀?20. ([?20], [?20]) -> [?20]` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `vec_first` | `∀?18. ([?18]) -> ?18?` | Shared | — | — | Supported | Experimental | Experimental |
| `vec_get` | `∀?16. ([?16], Int) -> ?16?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `vec_index_of` | `∀?69. ([?69], ?69) -> Int?` | Shared, Shared | — | — | Supported | Experimental | Experimental |
| `vec_last` | `∀?19. ([?19]) -> ?19?` | Shared | — | — | Supported | Experimental | Experimental |
| `vec_len` | `∀?13. ([?13]) -> Int` | Shared | — | — | Supported | Experimental | Experimental |
| `vec_new` | `∀?11. () -> [?11]` | — | — | — | Supported | Experimental | Experimental |
| `vec_pop` | `∀?15. ([?15]) -> ([?15], ?15?)` | Shared | — | — | Supported | Experimental | Experimental |
| `vec_push` | `∀?14. ([?14], ?14) -> [?14]` | Owned, Owned | — | — | Supported | Experimental | Experimental |
| `vec_reverse` | `∀?22. ([?22]) -> [?22]` | Shared | — | — | Supported | Experimental | Experimental |
| `vec_set` | `∀?17. ([?17], Int, ?17) -> [?17]` | Owned, Shared, Owned | — | — | Supported | Experimental | Experimental |
| `vec_slice` | `∀?21. ([?21], Int, Int) -> [?21]` | Shared, Shared, Shared | — | — | Supported | Experimental | Experimental |
| `vec_sort` | `∀?68. ([?68]) -> [?68]` | Shared | — | — | Supported | Experimental | Experimental |
| `zlib_compress` | `(Str) -> [Int]` | Shared | — | — | Supported | Experimental | Experimental |
| `zlib_decompress` | `([Int]) -> Str!Str` | Shared | — | — | Supported | Experimental | Experimental |

## Reading the table

- Parameter modes are ownership behavior, not merely calling syntax.
- Effects describe possible authority; capabilities grant it to one execution.
- Unsupported verification does not imply failure—it means the operation is outside that verification model.
- Profile support is transitive through calls; consult `docs/profiles.md`.
