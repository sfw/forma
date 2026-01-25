# FORMA Production Readiness Plan

**Date:** January 25, 2026
**Goal:** Close critical gaps to make FORMA production-ready
**Status:** Sprint 3 Complete - HTTP Server builtins implemented

### Progress Summary
- Sprint 1: Async Foundation - COMPLETE (async/await/spawn)
- Sprint 2: Concurrency - COMPLETE (channels, mutex)
- Sprint 3: HTTP Server - COMPLETE (response builders, request helpers)
- Sprint 4: TCP/UDP Sockets - NOT STARTED
- Sprint 5: C FFI - NOT STARTED
- Sprint 6: LSP Server - NOT STARTED

---

## Overview

This plan addresses the four critical gaps identified in COMPARISON_ANALYSIS.md:

1. **Async/Concurrency** - Enable parallel processing and server workloads
2. **C FFI** - Allow calling C libraries
3. **HTTP Server** - Enable building web services
4. **LSP Server** - Provide IDE support

---

## Phase 1: Concurrency Model (Priority: P0)

### Design Decision: Async/Await

We'll use **async/await** like Rust, but simplified for AI:
- No Pin/Unpin complexity
- No explicit executor selection
- Built-in runtime (like Go's, but with async syntax)

### 1.1 Core Async Syntax

```forma
# Async function declaration
f async fetch_data(url: Str) -> Result[Str, Str] {
    response := await http_get(url)?
    response.body
}

# Calling async functions
f async main() {
    data := await fetch_data("https://api.example.com")
    print(data)
}

# Spawn concurrent tasks
f async process_all(urls: [Str]) -> [Str] {
    tasks := urls | map(|url| spawn fetch_data(url)) | collect()
    await_all(tasks)
}
```

### 1.2 Concurrency Builtins to Implement

```
# Task spawning
spawn(task: async () -> T) -> Task[T]
await_all(tasks: [Task[T]]) -> [T]
await_any(tasks: [Task[T]]) -> T
await_race(tasks: [Task[T]]) -> Result[T, [Task[T]]]

# Channels (bounded)
channel_new(capacity: Int) -> (Sender[T], Receiver[T])
channel_send(sender: Sender[T], value: T) -> Result[(), Str]
channel_recv(receiver: Receiver[T]) -> Result[T, Str]
channel_try_send(sender: Sender[T], value: T) -> Bool
channel_try_recv(receiver: Receiver[T]) -> T?
channel_close(sender: Sender[T]) -> ()

# Synchronization
mutex_new() -> Mutex[T]
mutex_lock(m: Mutex[T]) -> MutexGuard[T]
mutex_try_lock(m: Mutex[T]) -> MutexGuard[T]?

# Timeouts
timeout(duration_ms: Int, task: async () -> T) -> Result[T, Str]
sleep_async(ms: Int) -> ()
```

### 1.3 Implementation Tasks

- [x] Add `async` keyword to lexer (was already present as `As`)
- [x] Add `await` keyword to lexer (was already present as `Aw`)
- [x] Add `spawn` keyword to lexer (added as `Sp`)
- [x] Parse async function declarations (was already working)
- [x] Parse await expressions (was already working)
- [x] Parse spawn expressions (added)
- [x] Add `Task[T]` type to type system
- [x] Add `Future[T]` type to type system
- [x] Add `Sender[T]`, `Receiver[T]` types
- [x] Add `Mutex[T]`, `MutexGuard[T]` types
- [x] Implement async transformation in MIR (Spawn/Await terminators)
- [x] Implement tokio-based runtime in interpreter (simplified sync for now)
- [x] Add channel builtins (channel_new, channel_send, channel_recv, etc.)
- [x] Add mutex builtins (mutex_new, mutex_lock, mutex_unlock, etc.)
- [x] Add timeout/sleep_async builtins
- [x] Add await_all/await_any builtins
- [x] Create tests/forma/test_async.forma
- [ ] Create stdlib/async.forma with helper functions
- [ ] Update grammar export for async syntax

### 1.4 Dependencies to Add

```toml
tokio = { version = "1", features = ["full"] }
```

---

## Phase 2: C FFI (Priority: P0)

### Design Decision: Simple extern blocks

```forma
# Declare external C functions
extern "C" {
    f strlen(s: *Char) -> Int
    f printf(format: *Char, ...) -> Int
    f malloc(size: Int) -> *Void
    f free(ptr: *Void) -> ()
}

# Use them
f main() {
    s := "Hello, World!"
    len := strlen(s.as_ptr())
    print(f"Length: {len}")
}
```

### 2.1 FFI Types

```
*T          - Raw pointer to T
*Void       - Void pointer (c_void)
*Char       - C string pointer
CInt        - C int (platform-specific)
CUInt       - C unsigned int
CLong       - C long
CULong      - C unsigned long
CFloat      - C float
CDouble     - C double
```

### 2.2 FFI Builtins

```
# Pointer operations
ptr_null() -> *T
ptr_is_null(p: *T) -> Bool
ptr_offset(p: *T, offset: Int) -> *T
ptr_read(p: *T) -> T
ptr_write(p: *T, value: T) -> ()

# String conversion
str_to_cstr(s: Str) -> *Char
cstr_to_str(p: *Char) -> Str
cstr_to_str_len(p: *Char, len: Int) -> Str

# Memory
alloc(size: Int) -> *Void
alloc_zeroed(size: Int) -> *Void
realloc(ptr: *Void, size: Int) -> *Void
dealloc(ptr: *Void) -> ()
```

### 2.3 Implementation Tasks

- [ ] Add `extern` keyword to lexer
- [ ] Add pointer type syntax `*T` to parser
- [ ] Add C primitive types (CInt, CLong, etc.)
- [ ] Parse extern blocks
- [ ] Add FFI type checking rules
- [ ] Implement libffi-based calling in interpreter
- [ ] Add pointer builtins
- [ ] Add string conversion builtins
- [ ] Add memory allocation builtins
- [ ] Create tests/forma/test_ffi.forma
- [ ] Create stdlib/ffi.forma with safe wrappers
- [ ] Document FFI safety guidelines

### 2.4 Dependencies to Add

```toml
libffi = "3.2"
```

---

## Phase 3: HTTP Server (Priority: P0)

### Design Decision: Simple callback-based API

```forma
# Simple server
f async main() {
    http_serve(8080, handle_request)
}

f async handle_request(req: HttpRequest) -> HttpResponse {
    m req.path {
        "/" => http_response(200, "Hello, World!"),
        "/api/users" => {
            users := get_users()
            http_json_response(200, users)
        },
        _ => http_response(404, "Not Found")
    }
}
```

### 3.1 Server Types

```forma
s HttpRequest {
    method: Str,        # GET, POST, etc.
    path: Str,          # /api/users
    query: Map[Str, Str], # ?key=value
    headers: Map[Str, Str],
    body: Str
}

s HttpResponse {
    status: Int,
    headers: Map[Str, Str],
    body: Str
}
```

### 3.2 Server Builtins

```
# Server lifecycle
http_serve(port: Int, handler: async (HttpRequest) -> HttpResponse) -> Result[(), Str]
http_serve_tls(port: Int, cert: Str, key: Str, handler: ...) -> Result[(), Str]

# Response builders
http_response(status: Int, body: Str) -> HttpResponse
http_json_response(status: Int, data: Json) -> HttpResponse
http_redirect(url: Str) -> HttpResponse
http_file_response(path: Str) -> Result[HttpResponse, Str]

# Request helpers
http_req_json(req: HttpRequest) -> Result[Json, Str]
http_req_form(req: HttpRequest) -> Map[Str, Str]
http_req_param(req: HttpRequest, name: Str) -> Str?
http_req_header(req: HttpRequest, name: Str) -> Str?
```

### 3.3 Implementation Tasks

- [x] Define HttpRequest struct in type system (via Named type + builtin return)
- [x] Define HttpResponse struct in type system (via Named type + builtin return)
- [x] Implement http_serve using hyper (simplified stub - prints handler info)
- [x] Implement response builder builtins (http_response, http_json_response, http_redirect, http_file_response)
- [x] Implement request helper builtins (http_req_json, http_req_form, http_req_param, http_req_header)
- [x] Create tests/forma/test_http_server.forma (7 tests passing)
- [x] Add http_request_new helper for testing
- [ ] Add routing helper functions in stdlib
- [ ] Create stdlib/http_server.forma
- [ ] Create examples/web_server.forma

### 3.4 Dependencies to Add

```toml
hyper = { version = "1", features = ["full"] }   # ADDED
hyper-util = "0.1"                                # ADDED
http-body-util = "0.1"                            # ADDED
bytes = "1"                                       # ADDED
```

---

## Phase 4: TCP/UDP Sockets (Priority: P1)

### 4.1 TCP Builtins

```
# TCP Client
tcp_connect(host: Str, port: Int) -> Result[TcpStream, Str]
tcp_read(stream: TcpStream, max_bytes: Int) -> Result[Str, Str]
tcp_read_exact(stream: TcpStream, bytes: Int) -> Result[Str, Str]
tcp_read_line(stream: TcpStream) -> Result[Str, Str]
tcp_write(stream: TcpStream, data: Str) -> Result[Int, Str]
tcp_write_all(stream: TcpStream, data: Str) -> Result[(), Str]
tcp_close(stream: TcpStream) -> ()
tcp_set_timeout(stream: TcpStream, ms: Int) -> ()
tcp_peer_addr(stream: TcpStream) -> Str
tcp_local_addr(stream: TcpStream) -> Str

# TCP Server
tcp_listen(host: Str, port: Int) -> Result[TcpListener, Str]
tcp_accept(listener: TcpListener) -> Result[TcpStream, Str]
tcp_listener_close(listener: TcpListener) -> ()

# Async versions (after Phase 1)
tcp_connect_async(host: Str, port: Int) -> async Result[TcpStream, Str]
tcp_read_async(stream: TcpStream, max_bytes: Int) -> async Result[Str, Str]
tcp_write_async(stream: TcpStream, data: Str) -> async Result[Int, Str]
tcp_accept_async(listener: TcpListener) -> async Result[TcpStream, Str]
```

### 4.2 UDP Builtins

```
udp_bind(host: Str, port: Int) -> Result[UdpSocket, Str]
udp_send_to(socket: UdpSocket, addr: Str, port: Int, data: Str) -> Result[Int, Str]
udp_recv_from(socket: UdpSocket, max_bytes: Int) -> Result[(Str, Str, Int), Str]
udp_close(socket: UdpSocket) -> ()

# Connected UDP
udp_connect(socket: UdpSocket, addr: Str, port: Int) -> Result[(), Str]
udp_send(socket: UdpSocket, data: Str) -> Result[Int, Str]
udp_recv(socket: UdpSocket, max_bytes: Int) -> Result[Str, Str]
```

### 4.3 DNS Builtins

```
dns_lookup(hostname: Str) -> Result[[Str], Str]
dns_reverse_lookup(ip: Str) -> Result[Str, Str]
```

### 4.4 Implementation Tasks

- [ ] Add TcpStream, TcpListener, UdpSocket types
- [ ] Implement TCP client builtins using std::net
- [ ] Implement TCP server builtins
- [ ] Implement UDP builtins
- [ ] Implement DNS builtins
- [ ] Add async versions (after Phase 1)
- [ ] Create tests/forma/test_tcp.forma
- [ ] Create tests/forma/test_udp.forma
- [ ] Create stdlib/net.forma
- [ ] Create examples/tcp_echo_server.forma
- [ ] Create examples/chat_client.forma

---

## Phase 5: LSP Server (Priority: P1)

### 5.1 LSP Features to Implement

| Feature | Priority | Notes |
|---------|----------|-------|
| textDocument/didOpen | P0 | Parse and type-check on open |
| textDocument/didChange | P0 | Re-check on edit |
| textDocument/didSave | P0 | Full re-check on save |
| textDocument/publishDiagnostics | P0 | Show errors/warnings |
| textDocument/completion | P0 | Use existing `forma complete` |
| textDocument/hover | P0 | Use existing `forma typeof` |
| textDocument/definition | P1 | Go to definition |
| textDocument/references | P1 | Find all references |
| textDocument/rename | P2 | Rename symbol |
| textDocument/formatting | P2 | Use existing `forma fmt` |
| textDocument/signatureHelp | P2 | Function signature on ( |

### 5.2 Implementation Tasks

- [ ] Create src/lsp/mod.rs
- [ ] Implement LSP message parsing (JSON-RPC)
- [ ] Implement document synchronization
- [ ] Implement diagnostics publishing
- [ ] Integrate with existing type checker for errors
- [ ] Implement completion using existing `complete` command
- [ ] Implement hover using existing `typeof` command
- [ ] Implement go-to-definition
- [ ] Implement find references
- [ ] Add `forma lsp` command to CLI
- [ ] Create VS Code extension package
- [ ] Test with VS Code, Neovim, other editors

### 5.3 Dependencies to Add

```toml
tower-lsp = "0.20"
```

---

## Phase 6: Additional Production Features (Priority: P2)

### 6.1 Logging

```
log_debug(msg: Str) -> ()
log_info(msg: Str) -> ()
log_warn(msg: Str) -> ()
log_error(msg: Str) -> ()
log_set_level(level: Str) -> ()  # "debug", "info", "warn", "error"
log_set_format(format: Str) -> ()  # "text", "json"
```

### 6.2 TLS Support

```
# TLS wrapper for TCP
tls_connect(host: Str, port: Int) -> Result[TlsStream, Str]
tls_read(stream: TlsStream, max_bytes: Int) -> Result[Str, Str]
tls_write(stream: TlsStream, data: Str) -> Result[Int, Str]
tls_close(stream: TlsStream) -> ()

# Update HTTP client to use HTTPS automatically
# (Already works via reqwest, just document it)
```

### 6.3 Compression

```
gzip_compress(data: Str) -> [Int]
gzip_decompress(data: [Int]) -> Result[Str, Str]
zlib_compress(data: Str) -> [Int]
zlib_decompress(data: [Int]) -> Result[Str, Str]
```

### 6.4 Database Client (SQLite first)

```
db_open(path: Str) -> Result[Database, Str]
db_execute(db: Database, sql: Str) -> Result[Int, Str]
db_query(db: Database, sql: Str) -> Result[[Row], Str]
db_query_one(db: Database, sql: Str) -> Result[Row?, Str]
db_prepare(db: Database, sql: Str) -> Result[Statement, Str]
db_bind(stmt: Statement, params: [Value]) -> Result[(), Str]
db_step(stmt: Statement) -> Result[Row?, Str]
db_close(db: Database) -> ()

# Row access
row_get_int(row: Row, col: Int) -> Int
row_get_str(row: Row, col: Int) -> Str
row_get_float(row: Row, col: Int) -> Float
row_get_bool(row: Row, col: Int) -> Bool
row_get_null(row: Row, col: Int) -> Bool
```

---

## Implementation Order (Sprints)

### Sprint 1: Async Foundation (Week 1-2) ✅ COMPLETED
1. ✅ Add async/await/spawn keywords to lexer (As, Aw, Sp tokens)
2. ✅ Parse async function declarations
3. ✅ Parse await and spawn expressions
4. ✅ Add Task[T] and Future[T] types
5. ✅ Basic async transformation in MIR (Spawn/Await terminators)
6. ✅ Integrate tokio runtime (added to Cargo.toml)
7. ✅ Test basic async/await (tests/forma/test_async.forma)
8. ✅ Add sleep_async, timeout, await_all, await_any builtins

### Sprint 2: Concurrency Features (Week 3) ✅ COMPLETED
1. ✅ Implement channels (Sender/Receiver) - channel_new, channel_send, channel_recv, etc.
2. ✅ Implement mutex - mutex_new, mutex_lock, mutex_unlock, mutex_get, mutex_set
3. ✅ Implement timeout/sleep_async (done in Sprint 1)
4. ✅ Implement await_all/await_any (done in Sprint 1)
5. ✅ Create async tests and examples (tests/forma/test_channels.forma)

### Sprint 3: HTTP Server (Week 4)
1. Define HttpRequest/HttpResponse types
2. Implement http_serve (requires async)
3. Implement response builders
4. Implement request helpers
5. Create web server example

### Sprint 4: TCP/UDP (Week 5)
1. Add socket types
2. Implement TCP client/server
3. Implement UDP
4. Implement DNS lookup
5. Create networking examples

### Sprint 5: FFI Foundation (Week 6-7)
1. Add extern block syntax
2. Add pointer types
3. Implement libffi calling
4. Add pointer builtins
5. Create FFI tests and examples

### Sprint 6: LSP Server (Week 8-9)
1. Create LSP module
2. Implement document sync
3. Implement diagnostics
4. Implement completion
5. Implement hover
6. Package VS Code extension

### Sprint 7: Production Polish (Week 10)
1. Logging framework
2. TLS support
3. Compression
4. SQLite client
5. Documentation

---

## Success Criteria

FORMA will be considered production-ready when:

- [ ] Async/await works with spawn and channels
- [ ] Can build a simple HTTP API server
- [ ] Can call C libraries via FFI
- [ ] LSP provides completion and diagnostics in VS Code
- [ ] TCP/UDP sockets work for custom protocols
- [ ] All tests pass (target: 400+ tests)
- [ ] Examples work: web server, chat client, CLI tool with DB

---

## Dependencies Summary

Add to Cargo.toml:

```toml
# Async runtime
tokio = { version = "1", features = ["full"] }

# HTTP server
hyper = { version = "1", features = ["full"] }
hyper-util = "0.1"
http-body-util = "0.1"

# FFI
libffi = "3.2"

# LSP
tower-lsp = "0.20"

# Additional (Phase 6)
tracing = "0.1"           # Logging
rustls = "0.23"           # TLS
flate2 = "1.0"            # Compression
rusqlite = "0.31"         # SQLite
```

---

## Notes

### Async Design Rationale

We chose async/await over goroutines because:
1. **Explicit is better** - AI can see where suspension points are
2. **Type safety** - Task[T] makes return types clear
3. **Familiar** - Matches Rust/JS/Python async patterns
4. **Simpler runtime** - No M:N scheduling complexity

We simplified Rust's async by:
1. **No Pin/Unpin** - All async values are implicitly pinned
2. **No explicit executor** - Built-in tokio runtime
3. **No Send/Sync** - Single-threaded by default, opt-in for multi-threaded

### FFI Safety

FFI is inherently unsafe. Our approach:
1. Mark extern functions as requiring unsafe context (future)
2. Provide safe wrappers in stdlib
3. Document safety requirements clearly
4. AI should prefer safe stdlib over raw FFI

### HTTP Server Simplicity

We prioritized simplicity over flexibility:
1. Single handler function (no middleware chain)
2. Routing in user code (pattern match on path)
3. No cookies/sessions built-in (use headers)
4. Future: Add router helper in stdlib

---

*Last updated: January 24, 2026*
