# FORMA Standard Library Expansion Plan

**Purpose:** Make FORMA competitive with Go/Rust for real-world applications
**Date:** January 24, 2026
**Status:** Sprints 1-6 Complete (JSON, Sorting, DateTime, Encoding, Regex, Process/Path, HTTP)

---

## Current State

### What We Have (75 built-ins + 115 stdlib functions)

**Built-ins working:**
- Print/IO: `print`, `eprintln`, `exit`
- File I/O: `file_read`, `file_write`, `file_append`, `file_exists`
- Strings: `str_len`, `str_slice`, `str_contains`, `str_split`, `str_trim`, etc.
- Vec: `vec_new`, `vec_push`, `vec_pop`, `vec_get`, `vec_len`, `vec_slice`, etc.
- Map: `map_new`, `map_insert`, `map_get`, `map_remove`, `map_keys`, `map_values`
- Math: `sqrt`, `sin`, `cos`, `pow`, `log`, `floor`, `ceil`, `round`
- Random: `random`, `random_int`, `random_bool`, `random_choice`
- Time: `time_now`, `time_now_ms`, `time_sleep`
- Option/Result: `unwrap`, `expect`, `is_some`, `is_none`, `is_ok`, `is_err`
- CLI: `args`, `env_get`
- Char: `char_is_digit`, `char_is_alpha`, `char_to_int`, etc.

**Stdlib files:**
- core.forma (17 functions) - assertions, math utilities
- iter.forma (31 functions) - Range, iteration
- vec.forma (40 functions) - Vec wrapper
- string.forma (24 functions) - String utilities
- map.forma (3 functions) - Map helpers (minimal)

---

## Expansion Plan

### Phase 1: Essential Data Processing (Priority: Critical)

These are needed for ANY real application.

#### 1.1 JSON Module
**File:** `stdlib/json.forma` + interpreter built-ins

Built-ins to add:
```
json_parse(s: Str) -> Result[JsonValue, Str]
json_stringify(v: JsonValue) -> Str
json_stringify_pretty(v: JsonValue) -> Str
json_get(v: JsonValue, key: Str) -> JsonValue?
json_get_str(v: JsonValue, key: Str) -> Str?
json_get_int(v: JsonValue, key: Str) -> Int?
json_get_float(v: JsonValue, key: Str) -> Float?
json_get_bool(v: JsonValue, key: Str) -> Bool?
json_get_array(v: JsonValue, key: Str) -> [JsonValue]?
json_get_object(v: JsonValue, key: Str) -> JsonValue?
json_is_null(v: JsonValue) -> Bool
json_is_string(v: JsonValue) -> Bool
json_is_number(v: JsonValue) -> Bool
json_is_bool(v: JsonValue) -> Bool
json_is_array(v: JsonValue) -> Bool
json_is_object(v: JsonValue) -> Bool
json_array_len(v: JsonValue) -> Int
json_array_get(v: JsonValue, idx: Int) -> JsonValue?
json_object_keys(v: JsonValue) -> [Str]
json_from_str(s: Str) -> JsonValue
json_from_int(n: Int) -> JsonValue
json_from_float(f: Float) -> JsonValue
json_from_bool(b: Bool) -> JsonValue
json_from_array(arr: [JsonValue]) -> JsonValue
json_null() -> JsonValue
```

Implementation: Use `serde_json` crate in interpreter, create `Value::Json` variant.

#### 1.2 Sorting Module
**File:** `stdlib/sort.forma` + interpreter built-ins

Built-ins to add:
```
sort_ints(arr: [Int]) -> [Int]
sort_ints_desc(arr: [Int]) -> [Int]
sort_floats(arr: [Float]) -> [Float]
sort_strings(arr: [Str]) -> [Str]
sort_by_key(arr: [T], key_fn: Str) -> [T]  # key_fn is field name
reverse(arr: [T]) -> [T]
shuffle(arr: [T]) -> [T]
binary_search(arr: [Int], target: Int) -> Int?
min_of(arr: [Int]) -> Int?
max_of(arr: [Int]) -> Int?
sum_of(arr: [Int]) -> Int
```

#### 1.3 Date/Time Module
**File:** `stdlib/datetime.forma` + interpreter built-ins

Built-ins to add:
```
# Timestamps
time_now() -> Int                    # Already exists
time_now_ms() -> Int                 # Already exists
time_from_parts(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int) -> Int

# Formatting
time_format(timestamp: Int, format: Str) -> Str
time_format_iso(timestamp: Int) -> Str
time_format_rfc2822(timestamp: Int) -> Str

# Parsing
time_parse(s: Str, format: Str) -> Result[Int, Str]
time_parse_iso(s: Str) -> Result[Int, Str]

# Components
time_year(timestamp: Int) -> Int
time_month(timestamp: Int) -> Int
time_day(timestamp: Int) -> Int
time_hour(timestamp: Int) -> Int
time_minute(timestamp: Int) -> Int
time_second(timestamp: Int) -> Int
time_weekday(timestamp: Int) -> Int  # 0=Sunday, 6=Saturday

# Duration
duration_seconds(n: Int) -> Int
duration_minutes(n: Int) -> Int
duration_hours(n: Int) -> Int
duration_days(n: Int) -> Int

# Arithmetic
time_add(timestamp: Int, duration: Int) -> Int
time_sub(timestamp: Int, duration: Int) -> Int
time_diff(a: Int, b: Int) -> Int
```

Implementation: Use `chrono` crate in interpreter.

---

### Phase 2: Encoding & Hashing (Priority: High)

Needed for APIs, security, data exchange.

#### 2.1 Encoding Module
**File:** `stdlib/encoding.forma` + interpreter built-ins

Built-ins to add:
```
# Base64
base64_encode(s: Str) -> Str
base64_decode(s: Str) -> Result[Str, Str]
base64_encode_bytes(bytes: [Int]) -> Str
base64_decode_bytes(s: Str) -> Result[[Int], Str]

# Hex
hex_encode(s: Str) -> Str
hex_decode(s: Str) -> Result[Str, Str]
hex_encode_bytes(bytes: [Int]) -> Str
hex_decode_bytes(s: Str) -> Result[[Int], Str]

# URL
url_encode(s: Str) -> Str
url_decode(s: Str) -> Result[Str, Str]
url_parse(s: Str) -> Result[Url, Str]  # Returns struct with scheme, host, path, query, etc.

# HTML (basic)
html_escape(s: Str) -> Str
html_unescape(s: Str) -> Str
```

Implementation: Use `base64`, `hex`, `urlencoding` crates.

#### 2.2 Hashing Module
**File:** `stdlib/hash.forma` + interpreter built-ins

Built-ins to add:
```
# Cryptographic hashes
sha256(s: Str) -> Str
sha256_bytes(bytes: [Int]) -> Str
sha512(s: Str) -> Str
md5(s: Str) -> Str  # Note: not secure, but needed for compatibility

# HMAC
hmac_sha256(key: Str, message: Str) -> Str

# Non-crypto (fast)
hash_string(s: Str) -> Int
hash_bytes(bytes: [Int]) -> Int

# UUID
uuid_v4() -> Str
uuid_parse(s: Str) -> Result[Str, Str]  # Validates format
```

Implementation: Use `sha2`, `md5`, `hmac`, `uuid` crates.

---

### Phase 3: Text Processing (Priority: High)

#### 3.1 Regex Module
**File:** `stdlib/regex.forma` + interpreter built-ins

Built-ins to add:
```
regex_match(pattern: Str, text: Str) -> Bool
regex_find(pattern: Str, text: Str) -> Str?
regex_find_all(pattern: Str, text: Str) -> [Str]
regex_replace(pattern: Str, text: Str, replacement: Str) -> Str
regex_replace_all(pattern: Str, text: Str, replacement: Str) -> Str
regex_split(pattern: Str, text: Str) -> [Str]
regex_captures(pattern: Str, text: Str) -> [Str]?  # Capture groups
regex_is_valid(pattern: Str) -> Bool
```

Implementation: Use `regex` crate.

#### 3.2 String Enhancements
**Add to existing string built-ins:**

```
str_repeat(s: Str, n: Int) -> Str
str_pad_left(s: Str, len: Int, pad: Str) -> Str
str_pad_right(s: Str, len: Int, pad: Str) -> Str
str_to_upper(s: Str) -> Str
str_to_lower(s: Str) -> Str
str_capitalize(s: Str) -> Str
str_reverse(s: Str) -> Str
str_chars(s: Str) -> [Char]
str_bytes(s: Str) -> [Int]
str_from_bytes(bytes: [Int]) -> Result[Str, Str]
str_lines(s: Str) -> [Str]
str_words(s: Str) -> [Str]
str_is_empty(s: Str) -> Bool
str_is_whitespace(s: Str) -> Bool
str_is_numeric(s: Str) -> Bool
str_is_alpha(s: Str) -> Bool
str_is_alphanumeric(s: Str) -> Bool
str_count(s: Str, sub: Str) -> Int
str_find(s: Str, sub: Str) -> Int?  # Index of first occurrence
str_find_last(s: Str, sub: Str) -> Int?
str_remove_prefix(s: Str, prefix: Str) -> Str
str_remove_suffix(s: Str, suffix: Str) -> Str
```

---

### Phase 4: System Interaction (Priority: High)

#### 4.1 Process Module
**File:** `stdlib/process.forma` + interpreter built-ins

Built-ins to add:
```
# Run commands
exec(cmd: Str) -> Result[ProcessOutput, Str]
exec_with_args(cmd: Str, args: [Str]) -> Result[ProcessOutput, Str]
exec_with_env(cmd: Str, args: [Str], env: {Str: Str}) -> Result[ProcessOutput, Str]
exec_shell(cmd: Str) -> Result[ProcessOutput, Str]

# ProcessOutput fields accessed via:
process_stdout(output: ProcessOutput) -> Str
process_stderr(output: ProcessOutput) -> Str
process_status(output: ProcessOutput) -> Int
process_success(output: ProcessOutput) -> Bool

# Environment
env_get(name: Str) -> Str?           # Already exists
env_set(name: Str, value: Str) -> ()
env_remove(name: Str) -> ()
env_vars() -> {Str: Str}

# Current process
pid() -> Int
cwd() -> Str
chdir(path: Str) -> Result[(), Str]
hostname() -> Str
username() -> Str?
home_dir() -> Str?
temp_dir() -> Str
```

Implementation: Use `std::process`, `std::env`.

#### 4.2 Path/Filesystem Module
**File:** `stdlib/path.forma` + interpreter built-ins

Built-ins to add:
```
# Path manipulation
path_join(parts: [Str]) -> Str
path_parent(path: Str) -> Str?
path_filename(path: Str) -> Str?
path_stem(path: Str) -> Str?         # Filename without extension
path_extension(path: Str) -> Str?
path_is_absolute(path: Str) -> Bool
path_is_relative(path: Str) -> Bool
path_normalize(path: Str) -> Str
path_absolute(path: Str) -> Result[Str, Str]

# Filesystem queries
file_exists(path: Str) -> Bool       # Already exists
file_is_file(path: Str) -> Bool
file_is_dir(path: Str) -> Bool
file_is_symlink(path: Str) -> Bool
file_size(path: Str) -> Result[Int, Str]
file_modified(path: Str) -> Result[Int, Str]  # Timestamp
file_created(path: Str) -> Result[Int, Str]

# Directory operations
dir_create(path: Str) -> Result[(), Str]
dir_create_all(path: Str) -> Result[(), Str]  # mkdir -p
dir_remove(path: Str) -> Result[(), Str]
dir_remove_all(path: Str) -> Result[(), Str]  # rm -rf
dir_list(path: Str) -> Result[[Str], Str]
dir_list_recursive(path: Str) -> Result[[Str], Str]

# File operations
file_copy(from: Str, to: Str) -> Result[(), Str]
file_move(from: Str, to: Str) -> Result[(), Str]
file_remove(path: Str) -> Result[(), Str]
file_read_bytes(path: Str) -> Result[[Int], Str]
file_write_bytes(path: Str, bytes: [Int]) -> Result[(), Str]
```

Implementation: Use `std::fs`, `std::path`.

---

### Phase 5: Networking (Priority: Critical for Web Apps)

#### 5.1 HTTP Client Module
**File:** `stdlib/http.forma` + interpreter built-ins

Built-ins to add:
```
# Simple requests
http_get(url: Str) -> Result[HttpResponse, Str]
http_post(url: Str, body: Str) -> Result[HttpResponse, Str]
http_post_json(url: Str, json: JsonValue) -> Result[HttpResponse, Str]
http_put(url: Str, body: Str) -> Result[HttpResponse, Str]
http_delete(url: Str) -> Result[HttpResponse, Str]

# Request builder
http_request(method: Str, url: Str) -> HttpRequest
http_set_header(req: HttpRequest, name: Str, value: Str) -> HttpRequest
http_set_body(req: HttpRequest, body: Str) -> HttpRequest
http_set_json(req: HttpRequest, json: JsonValue) -> HttpRequest
http_set_timeout(req: HttpRequest, ms: Int) -> HttpRequest
http_send(req: HttpRequest) -> Result[HttpResponse, Str]

# Response accessors
http_status(resp: HttpResponse) -> Int
http_body(resp: HttpResponse) -> Str
http_json(resp: HttpResponse) -> Result[JsonValue, Str]
http_header(resp: HttpResponse, name: Str) -> Str?
http_headers(resp: HttpResponse) -> {Str: Str}
http_ok(resp: HttpResponse) -> Bool  # status 200-299
```

Implementation: Use `reqwest` crate (blocking API for now, async later).

#### 5.2 TCP/UDP Module (Lower Level)
**File:** `stdlib/net.forma` + interpreter built-ins

Built-ins to add:
```
# TCP Client
tcp_connect(host: Str, port: Int) -> Result[TcpStream, Str]
tcp_read(stream: TcpStream, max_bytes: Int) -> Result[Str, Str]
tcp_read_line(stream: TcpStream) -> Result[Str, Str]
tcp_write(stream: TcpStream, data: Str) -> Result[Int, Str]
tcp_close(stream: TcpStream) -> ()
tcp_set_timeout(stream: TcpStream, ms: Int) -> ()

# TCP Server (basic)
tcp_listen(host: Str, port: Int) -> Result[TcpListener, Str]
tcp_accept(listener: TcpListener) -> Result[TcpStream, Str]

# UDP
udp_bind(host: Str, port: Int) -> Result[UdpSocket, Str]
udp_send(socket: UdpSocket, addr: Str, port: Int, data: Str) -> Result[Int, Str]
udp_recv(socket: UdpSocket, max_bytes: Int) -> Result[(Str, Str, Int), Str]  # (data, addr, port)
udp_close(socket: UdpSocket) -> ()

# DNS
dns_lookup(hostname: Str) -> Result[[Str], Str]  # Returns IP addresses
```

Implementation: Use `std::net`.

---

### Phase 6: Async Runtime (Priority: Critical for Scale)

This is the most complex addition. For now, implement basic blocking versions. Full async comes later.

#### 6.1 Channels (Sync for now)
**File:** `stdlib/channel.forma` + interpreter built-ins

```
channel_new() -> (Sender, Receiver)
channel_send(sender: Sender, value: T) -> Result[(), Str]
channel_recv(receiver: Receiver) -> Result[T, Str]
channel_try_recv(receiver: Receiver) -> T?
channel_close(sender: Sender) -> ()
```

#### 6.2 Thread Pool (Future)
```
spawn(fn: () -> T) -> Future[T]
await(future: Future[T]) -> T
sleep(ms: Int) -> ()  # Async-aware sleep
```

---

## Implementation Order

### Sprint 1: Data Essentials (Week 1-2)
1. JSON module (critical for any API work)
2. Sorting module (basic data manipulation)
3. String enhancements (common operations)

### Sprint 2: Encoding & Security (Week 3)
4. Base64/Hex encoding
5. URL encoding
6. Hashing (SHA256, MD5)
7. UUID generation

### Sprint 3: Text & System (Week 4)
8. Regex module
9. Date/Time formatting and parsing
10. Process execution
11. Path/Filesystem operations

### Sprint 4: Networking (Week 5-6)
12. HTTP client (blocking)
13. TCP/UDP sockets
14. DNS lookup

### Sprint 5: Concurrency Foundation (Week 7+)
15. Channels
16. Basic thread pool
17. Async runtime (major undertaking)

---

## Cargo Dependencies to Add

```toml
[dependencies]
# Already have
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
rand = "0.8"

# New for stdlib expansion
chrono = "0.4"           # Date/time
base64 = "0.22"          # Base64 encoding
hex = "0.4"              # Hex encoding
urlencoding = "2.1"      # URL encoding
sha2 = "0.10"            # SHA256/512
md-5 = "0.10"            # MD5
hmac = "0.12"            # HMAC
uuid = { version = "1.0", features = ["v4"] }  # UUID
regex = "1.10"           # Regex
reqwest = { version = "0.12", features = ["blocking", "json"] }  # HTTP client
```

---

## Testing Strategy

For each module:
1. Create `tests/forma/test_<module>.forma` with comprehensive tests
2. Add Rust unit tests in `tests/<module>_tests.rs`
3. Create example in `examples/<module>_example.forma`
4. Update stdlib documentation

---

## Success Criteria

FORMA stdlib will be competitive when:
- [x] Can parse/generate JSON (Sprint 1 - Complete)
- [x] Can make HTTP requests (Sprint 6 - Complete)
- [x] Can work with dates/times (Sprint 2 - Complete)
- [x] Can do regex matching (Sprint 4 - Complete)
- [x] Can encode/decode base64, hex, URL (Sprint 3 - Complete)
- [x] Can hash data (SHA256) (Sprint 3 - Complete)
- [x] Can generate UUIDs (Sprint 3 - Complete)
- [x] Can run external commands (Sprint 5 - Complete)
- [x] Can manipulate filesystem paths (Sprint 5 - Complete)
- [x] Can sort arrays (Sprint 1 - Complete)

---

## Notes

### Sprint 1 Implementation (January 24, 2026)

**JSON Module:**
- Added `Value::Json(serde_json::Value)` variant to interpreter
- Added `Ty::Json` to type system
- Registered 30 JSON builtins in interpreter and type checker
- Created `stdlib/json.forma` with wrapper functions
- Created `tests/forma/test_json.forma` with 14 passing tests

**Sorting Module:**
- Added 12 sorting builtins: `sort_ints`, `sort_ints_desc`, `sort_floats`, `sort_floats_desc`, `sort_strings`, `sort_strings_desc`, `reverse`, `shuffle`, `min_of`, `max_of`, `sum_of`, `binary_search`
- Created `tests/forma/test_sort.forma` with 13 passing tests

### Sprint 2 Implementation (January 24, 2026)

**DateTime Module:**
- Added 19 datetime builtins using chrono crate
- Functions: `time_from_parts`, `time_format`, `time_format_iso`, `time_format_rfc2822`, `time_parse`, `time_parse_iso`, `time_year`, `time_month`, `time_day`, `time_hour`, `time_minute`, `time_second`, `time_weekday`, `duration_seconds`, `duration_minutes`, `duration_hours`, `duration_days`, `time_add`, `time_sub`, `time_diff`
- Created `stdlib/datetime.forma` with helper functions
- Created `tests/forma/test_datetime.forma` with 14 passing tests

### Sprint 3 Implementation (January 24, 2026)

**Encoding Module:**
- Added 12 encoding builtins using base64, hex, sha2, uuid crates
- Base64: `base64_encode`, `base64_decode`, `base64_encode_bytes`, `base64_decode_bytes`
- Hex: `hex_encode`, `hex_decode`, `hex_encode_bytes`, `hex_decode_bytes`
- Hashing: `sha256`, `sha256_bytes`, `hash_string`
- UUID: `uuid_v4`, `uuid_parse`
- Created `tests/forma/test_encoding.forma` with 15 passing tests

### Sprint 4 Implementation (January 24, 2026)

**Regex Module:**
- Added 8 regex builtins using regex crate
- Functions: `regex_match`, `regex_find`, `regex_find_all`, `regex_replace`, `regex_replace_all`, `regex_split`, `regex_captures`, `regex_is_valid`
- Created `tests/forma/test_regex.forma` with 15 passing tests

### Sprint 5 Implementation (January 24, 2026)

**Process Module:**
- Added 9 process builtins: `exec`, `env_set`, `env_remove`, `env_vars`, `pid`, `cwd`, `chdir`, `home_dir`, `temp_dir`

**Path/Filesystem Module:**
- Added 18 path/filesystem builtins
- Path manipulation: `path_join`, `path_parent`, `path_filename`, `path_stem`, `path_extension`, `path_is_absolute`, `path_is_relative`, `path_absolute`
- File queries: `file_is_file`, `file_is_dir`, `file_size`
- Directory operations: `dir_create`, `dir_create_all`, `dir_remove`, `dir_remove_all`, `dir_list`
- File operations: `file_copy`, `file_move`, `file_remove`
- Created `tests/forma/test_process.forma` with 13 passing tests

### Sprint 6 Implementation (January 24, 2026)

**HTTP Module:**
- Added 5 HTTP builtins using reqwest crate (blocking API)
- Functions: `http_get`, `http_post`, `http_post_json`, `http_put`, `http_delete`
- Returns tuple: `(status_code: Int, body: Str, headers: {Str: Str})`

**Files Modified:**
- `src/mir/interp.rs` - Added all builtins (~100+ new builtins)
- `src/types/types.rs` - Added `Ty::Json` variant
- `src/types/inference.rs` - Registered all new builtins in type checker
- `Cargo.toml` - Added chrono, base64, hex, sha2, uuid, regex, reqwest dependencies

**Total New Tests:** 84 passing tests across 6 test files

