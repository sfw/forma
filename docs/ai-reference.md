# Forma 0.2 AI Quick Reference
<!-- Compact operational context for agents. -->
<!-- Compiler output and generated docs/grammar.* / docs/builtins.json remain authoritative. -->

Forma is an evolving 0.x language. Do not invent syntax or builtins. When this
document and the compiler disagree, follow `forma check`, semantic queries, the
generated grammar, and `docs/builtins.json`.

## Syntax Overview

- Indentation-based (like Python), no braces required
- Comments: `# ...`
- Immutable binding: `x = value`
- Mutable binding/update: `x := value`, then `x := replacement`
- Type annotations: `x: Type = value` or `x: Type := value`
- Last expression is return value
- Generics use `[T]` not `<T>`
- F-strings: `f"hello {name}, result is {x + 1}"`

## Keywords

| Short | Long | Purpose |
|-------|------|---------|
| `f` | — | function |
| `s` | — | struct |
| `e` | — | enum |
| `t` | — | trait |
| `i` | — | impl |
| `m` | — | match |
| `wh` | while | while loop |
| `lp` | loop | infinite loop |
| `br` | break | break |
| `ct` | continue | continue |
| `ret` | return | return |
| `as` | async | async modifier |
| `aw` | await | await |
| `sp` | spawn | spawn task |
| `us` | use | import |
| `md` | module | module |
| `pub` | — | public |
| `mut` | — | mutable |
| `ref` | — | reference param |
| `mv` | move | move |
| `un` | unsafe | unsafe |
| `type` | — | type alias |
| `where` | — | trait bounds |
| `linear` | — | linear type |
| `affine` | — | affine type |

Literals: `true`/`T`, `false`/`F`, `none`/`N`, `Some`, `Ok`/`ok`, `Err`/`err`

## Types

### Primitives
`Int`, `Float`, `Bool`, `Char`, `Str`, `()` (Unit), `!` (Never)

`Int` is signed 64-bit. Integer negation and `+ - * / %` trap on overflow or a
zero divisor in Hosted and Native execution; signed division truncates toward
zero.

### Sized integers
`i8` `i16` `i32` `i64` `i128` `u8` `u16` `u32` `u64` `u128` `isize` `usize`

### Collections
| Syntax | Type |
|--------|------|
| `[T]` | List |
| `[T; N]` | Fixed array |
| `{K: V}` | Map |
| `{T}` | Set |
| `(A, B)` | Tuple |

### Special
| Syntax | Type |
|--------|------|
| `T?` | Option[T] |
| `T!E` | Result[T, E] |
| `(A, B) -> C` | Function |
| `&T` | Shared ref |
| `&mut T` | Mutable ref |

### Async/Concurrency
`Task[T]`, `Future[T]`, `Sender[T]`, `Receiver[T]`, `Mutex[T]`

### Networking/DB
`TcpStream`, `TcpListener`, `UdpSocket`, `TlsStream`, `Database`, `Json`

## Operators

```
Arithmetic:   + - * / %
Comparison:   == != < <= > >=
Logical:      && || !
Bitwise:      & | ^ << >>
Bindings:     = (immutable)  := (mutable create/update)
Compound:     += -= *= /= %=
Special:      ? (propagate) ?? (coalesce) -> => .. ..= :: . @
```

## Syntax Forms

### Function
```forma
f name(param: Type, param: Type) -> ReturnType
    body

f name(a: Int, b: Int) -> Int = a + b          # single-expression

as f async_fn() -> Str!Str                      # async
    aw some_future()?

f generic[T](x: T) -> T = x                    # generic

f with_ref(ref data: [Int]) -> Int              # shared ref param
f with_mut(ref mut data: [Int]) -> Unit         # mutable ref param
```

### Struct
```forma
s Name
    field: Type
    field2: Type

s Tuple(Type, Type)                              # tuple struct
s Unit                                           # unit struct
s Generic[T]
    value: T
```

### Enum
```forma
e Name
    Variant
    Variant(Type)
    Variant(Type, Type)

e Simple = A | B | C                             # inline syntax
```

### Trait & Impl
```forma
t TraitName
    f method(&self) -> Type

i Type
    f method(&self) -> Type
        body

i TraitName for Type
    f method(&self) -> Type
        body
```

### Match
```forma
m expr
    Pattern -> expr
    Pattern if guard -> expr
    Variant(x) -> expr
    (a, b) -> expr
    Point { x, y } -> x + y                     # struct destructure
    _ -> expr                                    # wildcard
```

### Control Flow
```forma
if cond then expr else expr                      # expression form
if cond
    block
else if cond
    block
else
    block

wh condition                                     # while
    body

for x in collection                              # for-in
    body

lp                                               # infinite loop
    if done then br
    body

'outer: for x in xs                              # labeled loop
    for y in ys
        if x == y then br 'outer                 # break outer

ret value                                        # early return
br                                               # break
ct                                               # continue
```

### String Interpolation
```forma
name := "world"
greeting := f"hello {name}"                      # "hello world"
result := f"{x} + {y} = {x + y}"                # expressions allowed
```

### Closures
```forma
|x: Int, y: Int| x + y                          # typed params required
|x: Int| -> Int x * 2                           # with return type
```

### Variables
```forma
limit = 42                                       # immutable, inferred Int
title: Str = "Forma"                             # immutable, annotated
count := 0                                       # mutable, inferred Int
count := count + 1                               # update mutable binding
buffer: [Int] := vec_new()                       # mutable, annotated
```

Binding syntax controls mutability, not ownership. Assigning a non-`Copy` value
to another binding moves it unless it is explicitly cloned.

### Modules & Imports
```forma
us module.path
us module.{A, B}
us module.path -> alias
md name
    pub f helper() -> Int = 0
```

### Type Alias
```forma
type Name = ExistingType
type Handler[T] = (T) -> Unit
```

### Contracts
```forma
@inv(balance >= 0, "balance cannot be negative")
s Account
    balance: Int

@pre(n >= 0)
@post(result > 0, "must be positive")
f factorial(n: Int) -> Int
    if n <= 1 then 1 else n * factorial(n - 1)

@post(old(balance) + delta == result)
f deposit(balance: Int, delta: Int) -> Int
    balance + delta

@pre(values.len() > 0)
@post(forall i in 0..result.len()-1: result[i] <= result[i+1])
@post(permutation(values, result))
f sort(values: [Int]) -> [Int]
    sort_ints(values)
```

`@inv` applies only to named structs. Struct fields are directly in scope;
`old(...)` and `result` are not. Invariants run after construction, at function
entry/return, and when a `ref mut` borrow returns. Temporary inconsistency inside
an exclusive mutation is allowed only if validity is restored before return.
Keep decoders fallible: validate untrusted JSON/TOML/database/network data before
constructing an invariant-bearing struct.

Named contract patterns (expand to expressions):
- `@nonempty(x)` (pre-only), `@nonnegative(x)`, `@positive(x)`, `@nonzero(x)`
- `@bounded(x, lo, hi)`, `@sorted(x)`, `@sorted_desc(x)`, `@unique(x)`
- `@same_length(a, b)`, `@permutation(a, b)`
- `@unchanged(x)` (post-only), `@pure` (post-only, no args)

Contract CLI:
```bash
forma explain file.forma --format human|json|markdown --examples=N --seed S
forma verify dir --report --format json --examples N --seed S
```

### Async
```forma
as f work() -> Int
    result := aw some_future()
    result

task := sp work()                                # spawn
value := aw task                                 # await
results := aw await_all(tasks)                   # await multiple
first := aw await_any(tasks)                     # first to complete
```

## Error Handling Patterns

```forma
# Propagate with ? (Result only)
f load(path: Str) -> Str!Str
    content := file_read(path)?
    Ok(content)

# Unwrap with !
db := db_open("app.db")!

# Default with ?? (Option only)
name := env_get("USER") ?? "unknown"
val := str_to_int("abc") ?? 0

# Match on Result
m result
    Ok(v) -> use(v)
    Err(e) -> handle(e)

# Match on Option
m option
    Some(v) -> v
    None -> default

# Match with guard
m str_to_int(s)
    Some(n) if n > 0 -> Ok(n)
    Some(_) -> Err("not positive")
    None -> Err("not a number")
```

### Option/Result Utility Functions

```forma
# Option checks and unwrapping
is_some(Some(42))             # true
is_none(None)                 # true
unwrap(Some(42))              # 42 (panics on None)
unwrap_or(Some(42), 0)       # 42
unwrap_or(None, 0)           # 0
expect(Some(42), "missing")   # 42 (panics with msg on None)

# Result checks
is_ok(Ok(42))                 # true
is_err(Err("bad"))            # true

# Functions returning Option
str_to_int("42")              # Some(42)
str_to_int("abc")             # None
str_to_float("3.14")          # Some(3.14)
vec_get([10, 20], 0)          # Some(10)
vec_get([10, 20], 5)          # None

# Option chaining
map_opt(Some(5), |x: Int| x * 2)   # Some(10)
map_opt(None, |x: Int| x * 2)      # None
flatten(Some(Some(42)))             # Some(42)
and_then(Some(5), |x: Int| if x > 0 then Some(x * 10) else None)  # Some(50)
```

## Functional Operations

```forma
# Higher-order collection operations (closures require typed params)
doubled := map([1, 2, 3], |x: Int| x * 2)           # [2, 4, 6]
evens := filter([1, 2, 3, 4], |x: Int| x % 2 == 0)  # [2, 4]
total := reduce([1, 2, 3], 0, |acc: Int, x: Int| acc + x)  # 6
any([1, 2, 3], |x: Int| x > 2)                       # true
all([1, 2, 3], |x: Int| x > 0)                       # true

# Generic sort and search
vec_sort([3, 1, 2])                 # [1, 2, 3]
vec_index_of([10, 20, 30], 20)     # Some(1)
```

## Reference Parameters

Ordinary non-`Copy` values are affine: they may move or be dropped, but cannot be
used after a move or duplicated implicitly. Owned parameters move their arguments.
Use `clone(value)` for explicit duplication.

```forma
# Read-only reference
f sum(ref arr: [Int]) -> Int
    total := 0
    for x in arr
        total := total + x
    total

# Mutable reference
f sort(ref mut arr: [Int]) -> Unit
    quicksort(ref mut arr, 0, len(arr) - 1)

# Calling
total := sum(ref data)
sort(ref mut data)
```

References are second-class. They cannot be stored in ordinary aggregates,
captured by escaping closures, or sent to another task. A returned reference is
allowed only when derived from a reference parameter. Loan regions are inferred.

## Effects, Capabilities, and Profiles

- Effects are inferred through direct calls and describe possible authority.
- Capabilities grant authority to one execution; effects do not grant it.
- File, network, process, environment, and unsafe/FFI builtins are gated by the
  capability documented in `docs/builtins.json`.
- Core is the portable semantic subset.
- Hosted requires the managed interpreter runtime.
- Native identifies currently runtime-backed native facilities.
- Experimental has weaker compatibility guarantees.
- Profile support is transitive through calls.

Capability flags: `--allow-read`, `--allow-write`, `--allow-network`,
`--allow-exec`, `--allow-env`, `--allow-unsafe`, `--allow-all`.

Prefer least privilege. Capability flags are not complete OS isolation for
hostile code.

## Structured Concurrency Rules

- Task captures move into the child and must satisfy compiler-known `Send`.
- References cannot cross task boundaries.
- Task handles are affine: await, cancel, return, or explicitly detach them.
- Cancellation, deadlines, task limits, and a subset of capabilities propagate.
- Channels and mutexes are Hosted handles; sending moves the value.

## Common Patterns

### Struct with methods
```forma
s Point
    x: Float
    y: Float

i Point
    f distance(&self) -> Float
        sqrt(self.x * self.x + self.y * self.y)
```

### Option/Result matching
```forma
f safe_div(a: Int, b: Int) -> Int?
    if b == 0 then None
    else Some(a / b)

f main()
    m safe_div(10, 3)
        Some(v) -> print(v)
        None -> print("div by zero")
```

### Main function
```forma
f main()
    # program logic
```

> `main` can optionally return `-> Int` for an explicit exit code (e.g., `f main() -> Int`).
> When no return type is specified, the process exits with code 0.

### HTTP server
```forma
f handler(req: HttpRequest) -> HttpResponse
    m req.path
        "/" -> http_response(200, "OK")
        "/json" -> http_json_response(200, json_object())
        _ -> http_response(404, "Not Found")

f main()
    http_serve(8080, handler)!
```

### Database
```forma
f main()
    db := db_open("app.db")!
    db_execute(db, "CREATE TABLE t (id INTEGER, name TEXT)")!
    rows := db_query(db, "SELECT * FROM t")!
    for row in rows
        print(row_get_str(row, 1))
    db_close(db)
```

## Key Builtins (by category)

This is a convenience index, not the registry. For exact signatures, parameter
ownership modes, effects, capability requirements, and backend support, consult
the generated `docs/builtins.json`.

### I/O & Logging
`print(v)` `eprintln(v)` `str(v)` `debug(v)` `info(v)` `warning(v)` `error(v)`
`log_info(msg)` `log_warn(msg)` `log_error(msg)` `log_debug(msg)` `log_set_level(lvl)` `log_set_format(fmt)`

### Math
`abs(n)` `abs_float(x)` `sqrt(x)` `pow(b,e)` `sin(x)` `cos(x)` `tan(x)` `asin(x)` `acos(x)` `atan2(y,x)` `log(x)` `log2(x)` `exp(x)` `floor(x)` `ceil(x)` `round(x)` `min_of(a,b)` `max_of(a,b)` `sum_of(arr)`

### String
`str_len(s)` `str_contains(s,sub)` `str_starts_with(s,p)` `str_ends_with(s,p)` `str_split(s,d)` `str_trim(s)` `str_slice(s,i,j)` `str_char_at(s,i)` `str_concat(a,b)` `str_replace(s,old,new)` `str_replace_all(s,old,new)` `str_to_int(s)` `str_to_int_radix(s,radix)` `str_to_float(s)` `int_to_str(n)` `int_to_char(n)` `char_to_str(c)` `char_to_int(c)`

### Character Classification
`char_is_alpha(c)` `char_is_digit(c)` `char_is_alphanumeric(c)` `char_is_whitespace(c)`

### Collection
`len(c)` `is_empty(c)` `contains(c,v)` `reverse(v)` `vec_new()` `vec_push(v,x)` `vec_pop(v)` `vec_get(v,i)` `vec_set(v,i,x)` `vec_first(v)` `vec_last(v)` `vec_len(v)` `vec_slice(v,i,j)` `vec_concat(a,b)` `vec_reverse(v)` `vec_sort(v)` `vec_index_of(v,x)` `binary_search(v,x)` `shuffle(v)` `sort_ints(v)` `sort_ints_desc(v)` `sort_floats(v)` `sort_strings(v)` `sort_strings_desc(v)` `map_new()` `map_get(m,k)` `map_insert(m,k,v)` `map_remove(m,k)` `map_keys(m)` `map_values(m)` `map_contains(m,k)` `map_len(m)`

### Functional (higher-order)
`map(arr,fn)` `filter(arr,fn)` `reduce(arr,init,fn)` `any(arr,fn)` `all(arr,fn)`

### Option/Result
`unwrap(v)` `unwrap_or(v,d)` `expect(v,msg)` `is_some(v)` `is_none(v)` `is_ok(v)` `is_err(v)` `map_opt(opt,fn)` `flatten(opt)` `and_then(opt,fn)`

### File I/O (needs --allow-read/--allow-write)
`file_read(p)` `file_write(p,c)` `file_exists(p)` `file_append(p,c)` `file_read_bytes(p)` `file_write_bytes(p,b)` `file_copy(src,dst)` `file_move(src,dst)` `file_remove(p)` `file_size(p)` `file_is_file(p)` `file_is_dir(p)` `dir_list(p)` `dir_create(p)` `dir_create_all(p)` `dir_remove(p)` `dir_remove_all(p)`

### Path Operations
`path_join(a,b)` `path_parent(p)` `path_filename(p)` `path_stem(p)` `path_extension(p)` `path_absolute(p)` `path_is_absolute(p)` `path_is_relative(p)` `path_resolve_within(root,relative)`

For scoped agent file tools, resolve every external relative path with
`path_resolve_within` before I/O. It rejects absolute paths, parent traversal,
and canonical symlink escapes.

### Stdlib File I/O (requires `us std.io`)
`file_read_lines(p)` `file_write_lines(p,lines)`

### JSON
`json_parse(s)` `json_stringify(v)` `json_stringify_pretty(v)` `json_get(o,k)` `json_get_str(o,k)` `json_get_int(o,k)` `json_get_float(o,k)` `json_get_bool(o,k)` `json_get_array(o,k)` `json_has(o,k)` `json_set(o,k,v)` `json_keys(o)` `json_values(o)` `json_object()` `json_array()` `json_null()` `json_from_str(s)` `json_from_int(n)` `json_from_float(x)` `json_from_bool(b)` `json_array_get(a,i)` `json_array_len(a)` `json_type(v)` `json_is_object(v)` `json_is_array(v)` `json_is_string(v)` `json_is_number(v)` `json_is_bool(v)` `json_is_null(v)` `json_to_value(j)`

### HTTP (needs --allow-network)
`http_get(url)` `http_post(url,body)` `http_post_json(url,j)` `http_put(url,body)` `http_delete(url)` `http_request_new(method,url)` `http_req_header(req,k,v)` `http_req_param(req,k,v)` `http_req_json(req,j)` `http_req_form(req,data)` `http_serve(port,handler)` `http_response(code,body)` `http_response_with_headers(code,body,hdrs)` `http_json_response(code,json)` `http_file_response(code,path)` `http_redirect(url)`

### TCP (needs --allow-network)
`tcp_connect(host,port)` `tcp_listen(host,port)` `tcp_accept(listener)` `tcp_read(conn)` `tcp_read_exact(conn,n)` `tcp_read_line(conn)` `tcp_write(conn,data)` `tcp_write_all(conn,data)` `tcp_close(conn)` `tcp_listener_close(l)` `tcp_local_addr(conn)` `tcp_peer_addr(conn)` `tcp_set_timeout(conn,ms)`

### UDP (needs --allow-network)
`udp_bind(host,port)` `udp_connect(sock,host,port)` `udp_send(sock,data)` `udp_send_to(sock,data,addr)` `udp_recv(sock)` `udp_recv_from(sock)` `udp_close(sock)`

### TLS (needs --allow-network)
`tls_connect(host,port)` `tls_read(conn)` `tls_write(conn,data)` `tls_close(conn)`

### DNS (needs --allow-network)
`dns_lookup(host)` `dns_reverse_lookup(ip)`

### Database
`db_open(path)` `db_open_memory()` `db_connect_postgres(url)` `db_execute(db,sql)` `db_query(db,sql)` `db_query_one(db,sql)` `db_prepare(db,sql)` `db_execute_prepared(stmt,params)` `db_query_prepared(stmt,params)` `db_close(db)` `row_get(r,i)` `row_get_int(r,i)` `row_get_str(r,i)` `row_get_float(r,i)` `row_get_bool(r,i)` `row_is_null(r,i)` `row_len(r)`

Use SQLite locally. For remote PostgreSQL, obtain the connection URL with
`env_get` and pass it to `db_connect_postgres`; never embed database credentials
in source or settings. Prepared queries use `?` placeholders on both backends;
write `??` for a literal PostgreSQL question-mark operator.

### Structured Processes (needs --allow-exec)
`process_run(program,args,cwd,environment,allowed_programs,timeout_ms,max_output_chars)`
uses direct invocation rather than a shell, exact executable authority, a
cleared environment plus explicit entries, a deadline, process-group
termination, and bounded stdout/stderr. Prefer it over shell-oriented process
helpers for automation and agent harnesses.

### HTTP and configuration
`http_request(method,url,body,headers,timeout_ms,follow_redirects)` supports
authorization headers and arbitrary text request bodies. `toml_parse(source)`
and `toml_stringify(value)` provide human-authored configuration interchange;
keep API payloads and workflow graphs in JSON.

### Async/Channels
`channel_new()` `channel_send(s,v)` `channel_recv(r)` `channel_try_send(s,v)` `channel_try_recv(r)` `channel_close(ch)` `mutex_new(v)` `mutex_lock(m)` `mutex_unlock(m)` `mutex_try_lock(m)` `mutex_get(m)` `mutex_set(m,v)` `sleep_async(s)` `await_all(tasks)` `await_any(tasks)` `timeout(future,ms)`

### Random
`random()` `random_int(min,max)` `random_bool()` `random_choice(list)` `random_shuffle(list)` `shuffle(list)`

### Time
`time_now()` `time_now_ms()` `time_sleep(s)` `time_format(ts,fmt)` `time_format_iso(ts)` `time_parse(s,fmt)` `time_parse_iso(s)` `time_from_parts(y,mo,d,h,mi,s)` `time_add(ts,secs)` `time_sub(ts,secs)` `time_diff(a,b)` `time_year(ts)` `time_month(ts)` `time_day(ts)` `time_hour(ts)` `time_minute(ts)` `time_second(ts)` `time_weekday(ts)` `duration_seconds(s)` `duration_minutes(m)` `duration_hours(h)` `duration_days(d)`

### Regex
`regex_match(pat,s)` `regex_find(pat,s)` `regex_find_all(pat,s)` `regex_captures(pat,s)` `regex_replace(pat,s,r)` `regex_replace_all(pat,s,r)` `regex_split(pat,s)` `regex_is_valid(pat)`

### Compression
`gzip_compress(data)` `gzip_decompress(data)` `zlib_compress(data)` `zlib_decompress(data)`

### Hex Encoding
`hex_encode(s)` `hex_decode(s)` `hex_encode_bytes(b)` `hex_decode_bytes(s)`

### Hashing/UUID
`hash_string(s)` `uuid_parse(s)`

### Type Introspection
`type_of(v)` `sizeof(v)` `int(v)` `float(v)`

### Process/System (needs --allow-exec)
`exec(cmd)` `pid()` `args()` `cwd()` `chdir(p)` `home_dir()` `temp_dir()`

### Environment (needs --allow-env)
`env_get(k)` `env_set(k,v)` `env_remove(k)` `env_vars()`

### Assertions
`assert(cond)` `panic(msg)` `exit(code)`

### Memory/FFI (needs --allow-unsafe)
`alloc(size)` `alloc_zeroed(size)` `dealloc(ptr)` `mem_copy(dst,src,n)` `mem_set(ptr,val,n)` `ptr_null()` `ptr_is_null(p)` `ptr_addr(p)` `ptr_from_addr(n)` `ptr_offset(p,n)` `str_to_cstr(s)` `cstr_to_str(p)` `cstr_to_str_len(p,n)` `cstr_free(p)` `to_cint(n)` `from_cint(n)` `to_clong(n)` `from_clong(n)` `to_cfloat(x)` `from_cfloat(x)` `to_cdouble(x)` `from_cdouble(x)` `to_cuint(n)` `from_cuint(n)` `to_culong(n)` `from_culong(n)` `to_csize(n)` `from_csize(n)`

## Stdlib Modules

Import with `us`. These are FORMA functions that compose builtins.
- `us std.core` → `clamp(v,lo,hi)` `clamp_float(v,lo,hi)` `abs` `min` `max` `gcd` `lcm`
- `us std.io` → `file_read_lines(path)` `file_write_lines(path,lines)` `puts(s)`
- `us std.string` → `str_join(arr,sep)` `str_replace_first(s,old,new)` `str_index_of(s,sub)`
- `us std.vec` → `int_vec_index_of(arr,x)` `int_vec_sum(arr)` `int_vec_max(arr)`
- `us std.iter` → `range(start,end)` `enumerate(arr)`
- `us std.map` → `map_get_or(m,key,default)` `map_update(m,key,fn)`

## CLI

```bash
forma run <file>                        # run program
forma run <file> --allow-all            # run with all capabilities (DO NOT use on untrusted code)
forma run <file> --no-check-contracts   # disable contracts (enabled by default)
forma run <file> --no-optimize          # disable MIR optimization pass
forma check <file>                      # type check only
forma check <file> --error-format json  # JSON errors
forma check <file> --partial            # partial check (contracts only)
forma explain <file> --format json      # contract intent in JSON
forma explain <file> --examples=3 --seed 42
forma verify <path> --report --format human
forma verify <path> --report --format json --examples 20 --seed 42
forma verify <path> --level test --report
forma verify <path> --level exhaustive --max-domain 4096 --report
forma verify <path> --level formal --report
forma verify <path> --level formal --report --solver z3 --require-proved --emit-smt target/formal
forma verify <path> --report --max-steps 10000 --timeout 1000
forma verify <path> --report --allow-side-effects
forma grammar --format ebnf             # export grammar
forma grammar --format json             # export grammar (JSON)
forma build <file>                      # build native binary (LLVM)
forma build <file> --no-optimize        # build without MIR optimization
forma fmt <file>                        # format code
forma repl                              # interactive REPL
forma new <name>                        # create new project (forma.toml + src/main.forma)
forma init                              # initialize project in current directory
forma lsp                               # start LSP server for IDE support
forma typeof <file> --position L:C      # type at position
forma complete <file> --position L:C    # completions
```

Formal verification covers acyclic `Bool`/signed-64-bit-`Int` MIR
path-sensitively and structural tuples/named structs composed from those leaves.
It proves construction, projection, equality, projected field updates, and
struct-invariant establishment/preservation at return boundaries; invariant
parameters are assumed valid at entry. Arithmetic safety is checked and
unsatisfiable preconditions/invariant assumptions are rejected as vacuous. Use
`--fail-on-unknown` for a strict incompleteness gate or `--require-proved` to
require every obligation-bearing function to be `PROVED`. The JSON report
records solver identity, version when available, timeout, proof policy, and
per-struct aggregate proof status. Direct pure source calls are symbolically
inlined. Loops, recursive/indirect or effectful calls, arrays/vectors, indexing,
and reference/dereference reasoning report `UNKNOWN`.

**Security:** `--allow-all` enables file, network, process, env, and unsafe operations. Do not use on untrusted code. Prefer least-privilege: `--allow-read`, `--allow-write`, `--allow-network`, `--allow-exec`, `--allow-env`, `--allow-unsafe`. The `--allow-exec` flag permits shell command execution and should be treated as full shell access.

## Verification Result Rules

| Result | Meaning |
| --- | --- |
| `UNCONTRACTED` | No contract obligation was declared |
| `TESTED` | Reproducible generated examples passed within reported bounds |
| `COUNTEREXAMPLE` | An execution or model violated an obligation |
| `EXHAUSTIVE` | Every tuple in a supported finite domain was checked |
| `PROVED` | The supported SMT obligations were discharged |
| `UNKNOWN` | Unsupported, too large, timed out, or not proved |
| `SKIPPED` | Work was intentionally not attempted |

Never describe `TESTED` as proof. Formal verification is Experimental and covers
only its pure supported subset. Preserve `UNKNOWN`, `SKIPPED`, bounds, seeds, and
unsupported effects in summaries.
