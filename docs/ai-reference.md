# FORMA AI Quick Reference
<!-- Token-efficient reference for LLM system prompts. ~400 lines. -->
<!-- Include this in context windows for correct FORMA code generation. -->

## Syntax Overview

- Indentation-based (like Python), no braces required
- Comments: `# ...`
- Variables: `x := value` (mutable by default)
- Type annotations: `x: Type = value`
- Last expression is return value
- Generics use `[T]` not `<T>`

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
Assignment:   := = += -= *= /= %=
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

ret value                                        # early return
br                                               # break
ct                                               # continue
```

### Closures
```forma
|x: Int, y: Int| x + y                          # typed params required
|x: Int| -> Int x * 2                           # with return type
```

### Variables
```forma
x := 42                                          # infer type
x: Int = 42                                      # explicit type
x := x + 1                                       # reassign
```

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
# Option checks
is_some(Some(42))             # true
is_none(None)                 # true

# Option unwrapping
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
vec_get([10, 20], 0)          # Some(10)
vec_get([10, 20], 5)          # None
```

## Reference Parameters

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

### I/O
`print(v)` `eprintln(v)` `str(v)`

### Math
`abs(n)` `sqrt(x)` `pow(b,e)` `sin(x)` `cos(x)` `tan(x)` `log(x)` `exp(x)` `floor(x)` `ceil(x)` `round(x)`

### String
`str_len(s)` `str_contains(s,sub)` `str_starts_with(s,p)` `str_ends_with(s,p)` `str_split(s,d)` `str_trim(s)` `str_slice(s,i,j)` `str_replace_all(s,old,new)` `str_to_int(s)`

### Collection
`len(c)` `vec_new()` `vec_push(v,x)` `vec_pop(v)` `vec_slice(v,i,j)` `vec_concat(a,b)` `vec_reverse(v)` `sort_ints(v)` `map_new()` `map_get(m,k)` `map_insert(m,k,v)` `map_keys(m)` `map_contains(m,k)`

### Option/Result
`unwrap(v)` `unwrap_or(v,d)` `expect(v,msg)` `is_some(v)` `is_none(v)` `is_ok(v)` `is_err(v)`

### File I/O (needs --allow-read/--allow-write)
`file_read(p)` `file_write(p,c)` `file_exists(p)` `file_append(p,c)` `dir_list(p)` `dir_create(p)` `path_join(a,b)`

### JSON
`json_parse(s)` `json_stringify(v)` `json_get(o,k)` `json_get_str(o,k)` `json_get_int(o,k)`

### Networking (needs --allow-network)
`http_get(url)` `http_post(url,body)` `http_post_json(url,j)` `http_serve(port,handler)` `tcp_connect(h,p)` `tcp_read(c)` `tcp_write(c,d)` `tls_connect(h,p)`

### Process/Env/Unsafe capabilities
- Process builtins require `--allow-exec`
- Environment builtins require `--allow-env`
- Pointer/memory/FFI-style builtins require `--allow-unsafe`

### Database
`db_open(path)` `db_open_memory()` `db_execute(db,sql)` `db_query(db,sql)` `db_close(db)` `row_get_int(r,i)` `row_get_str(r,i)` `row_get_float(r,i)`

### Async/Channels
`channel_new()` `channel_send(s,v)` `channel_recv(r)` `mutex_new(v)` `mutex_lock(m)` `mutex_unlock(m)` `sleep_async(s)`

### Random
`random()` `random_int(min,max)` `random_bool()` `random_choice(list)`

### Time
`time_now()` `time_now_ms()` `time_sleep(s)` `time_format(ts,fmt)`

### Regex
`regex_match(pat,s)` `regex_find(pat,s)` `regex_find_all(pat,s)` `regex_replace_all(pat,s,r)` `regex_split(pat,s)`

### Assertions
`assert(cond)` `panic(msg)` `exit(code)`

## CLI

```bash
forma run <file>                        # run program
forma run <file> --allow-all            # run with all capabilities (DO NOT use on untrusted code)
forma run <file> --no-check-contracts   # disable contracts (enabled by default)
forma check <file>                      # type check only
forma check <file> --error-format json  # JSON errors
forma explain <file> --format json      # contract intent in JSON
forma explain <file> --examples=3 --seed 42
# or: forma explain <file> --max-examples 3 --seed 42
# explain --examples includes valid + invalid (precondition-violating) cases
forma verify <path> --report --format human
forma verify <path> --report --format json --examples 20 --seed 42
forma verify <path> --report --max-steps 10000 --timeout 1000
forma verify <path> --report --allow-side-effects
forma grammar --format ebnf             # export grammar
forma grammar --format json             # export grammar (JSON)
forma fmt <file>                        # format code
forma repl                              # interactive REPL
forma typeof <file> --position L:C      # type at position
forma complete <file> --position L:C    # completions
```

**Security:** `--allow-all` enables file, network, process, env, and unsafe operations. Do not use on untrusted code. Prefer least-privilege: `--allow-read`, `--allow-write`, `--allow-network`, `--allow-exec`, `--allow-env`, `--allow-unsafe`. The `--allow-exec` flag permits shell command execution and should be treated as full shell access.
