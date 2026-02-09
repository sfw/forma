# Sprint 47: Public Repo Hardening

## Goal

Harden FORMA for public-repo launch by addressing five concrete launch risks:

1. CI supply-chain setup for LLVM dependencies
2. Missing explicit workflow token permissions
3. Mutable GitHub Action version tags
4. Missing explicit untrusted-code warning for `--allow-all` / high-risk capability flags
5. Predictable temp-file usage in contract test script

---

## Scope

### 47.1 CI Supply-Chain Hardening (LLVM apt setup)

**Priority:** P1  
**Files:** `.github/workflows/ci.yml`

### Changes

- Replace deprecated `apt-key` usage with a scoped keyring file under `/usr/share/keyrings`.
- Use `signed-by=` in repo source entry to constrain trust to the LLVM repo.
- Use `https://apt.llvm.org/...` instead of `http://...`.

### Acceptance Criteria

1. No `apt-key add` usage remains in CI workflow.
2. LLVM apt source uses `https` and `signed-by=...`.
3. `llvm-check` job still installs and runs successfully.

---

### 47.2 Principle-of-Least-Privilege Workflow Permissions

**Priority:** P2  
**Files:** `.github/workflows/ci.yml`

### Changes

- Add top-level `permissions:` block with minimal scope:
  - `contents: read`
- If artifact upload requires additional scope in your environment, set minimally at job level (do not widen globally).

### Acceptance Criteria

1. CI workflow declares explicit permissions.
2. All jobs continue to pass with those permissions.

---

### 47.3 Action Pinning Hardening

**Priority:** P2  
**Files:** `.github/workflows/ci.yml`

### Changes

- Pin all third-party actions to immutable SHAs.
- Add inline comments mapping each SHA to the human-readable tag (e.g., `# actions/checkout@v4`).
- Keep pinned versions current and reviewed.

### Acceptance Criteria

1. No `uses: ...@vX` mutable tags remain for third-party actions.
2. Workflow remains functionally unchanged and green.

---

### 47.4 Public Safety Documentation for High-Risk Capabilities

**Priority:** P2  
**Files:** `README.md`, `docs/reference.md`, `docs/ai-reference.md`

### Changes

- Add a **Security Note** near capability examples:
  - `--allow-all` is equivalent to enabling file/network/process/env/unsafe operations.
  - Do not run untrusted FORMA code with `--allow-all`.
  - Prefer least privilege: `--allow-read`, `--allow-write`, `--allow-network`, `--allow-exec`, `--allow-env`, `--allow-unsafe` as needed.
- Mention that `exec` executes through shell command evaluation and should be treated as high-risk.

### Acceptance Criteria

1. All three docs contain a clear untrusted-code warning.
2. `--allow-all` examples are paired with least-privilege guidance.
3. Wording is consistent across docs.

---

### 47.5 Temp-File Safety in Contract Test Script

**Priority:** P3  
**Files:** `scripts/test_contracts.sh`

### Changes

- Replace fixed `/tmp/forma_*.out/json` paths with `mktemp`.
- Add cleanup via `trap` for created temp files.
- Preserve script behavior and output checks.

### Acceptance Criteria

1. Script uses `mktemp` for all temp artifacts.
2. Script cleans up temp files on exit.
3. Script passes locally and in CI.

---

## Verification Plan

Run and include in PR:

```bash
# CI config sanity
rg -n "apt-key|http://apt\\.llvm\\.org|uses: .*@v[0-9]" .github/workflows/ci.yml
rg -n "^permissions:" .github/workflows/ci.yml

# Script safety checks
rg -n "/tmp/forma_|mktemp|trap" scripts/test_contracts.sh
bash scripts/test_contracts.sh

# Full gates
cargo test --all
cargo clippy --all-targets -- -D warnings
cargo fmt --all -- --check
```

Also validate workflow jobs in GitHub Actions after merge:

- `test`
- `clippy`
- `fmt`
- `llvm-check`
- `runtime-test`
- `forma-tests`
- `contract-tests`
- `coverage-audit`
- `showcase`

---

## Out of Scope

1. Runtime sandboxing of FORMA program execution at OS/container level
2. Redesign of `exec` builtin semantics
3. Dependency SBOM/signing infrastructure
4. Secret scanning platform rollout (e.g., GitHub Advanced Security policy)

---

## Definition of Done

1. Findings 1–5 are fully resolved in code and docs.
2. CI workflow hardening is implemented without regressions.
3. Security warning language for untrusted code is explicit and visible.
4. Contract test script is temp-file safe and deterministic.
5. All verification commands pass.
