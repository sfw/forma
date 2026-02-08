#!/usr/bin/env bash
# builtin_coverage.sh — Audit builtin function test coverage
#
# Extracts all builtin names from the interpreter match arms and checks
# which ones have corresponding tests (Rust unit tests + .forma integration tests).
#
# Usage: bash scripts/builtin_coverage.sh [--enforce THRESHOLD]
# Exit: 0 normally, 1 if --enforce threshold is not met

set -euo pipefail

INTERP="src/mir/interp.rs"
ENFORCE_THRESHOLD=""

while [ $# -gt 0 ]; do
    case "$1" in
        --enforce)
            ENFORCE_THRESHOLD="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

if [ ! -f "$INTERP" ]; then
    echo "Error: $INTERP not found. Run from the project root."
    exit 1
fi

# Extract builtin names from match arms: patterns like "builtin_name" =>
BUILTINS=$(grep -E '^\s+"[a-z_]+" *=>' "$INTERP" | sed 's/.*"\([a-z_]*\)".*/\1/' | sort -u)
TOTAL=$(echo "$BUILTINS" | wc -l | tr -d ' ')

# --- Source 1: call_builtin("name") in Rust source and test code ---
TESTED_RUST=$(grep -ro 'call_builtin("[a-z_]*"' src/ tests/ 2>/dev/null \
    | sed 's/.*call_builtin("//;s/"//' | sort -u || true)

# --- Source 2: builtin names referenced in .forma test/fixture/example files ---
# Concatenate all .forma content into a temp file, then grep each builtin against it
FORMA_TMP=$(mktemp)
trap 'rm -f "$FORMA_TMP"' EXIT

for dir in tests/forma tests/fixtures examples/showcase; do
    if [ -d "$dir" ]; then
        cat "$dir"/*.forma >> "$FORMA_TMP" 2>/dev/null || true
    fi
done

TESTED_FORMA=""
if [ -s "$FORMA_TMP" ]; then
    while IFS= read -r builtin; do
        if grep -qw "$builtin" "$FORMA_TMP" 2>/dev/null; then
            TESTED_FORMA="${TESTED_FORMA}${builtin}
"
        fi
    done <<< "$BUILTINS"
fi
TESTED_FORMA=$(echo "$TESTED_FORMA" | sort -u | grep -v '^$' || true)

# Combine all tested builtins
TESTED=$(printf '%s\n%s\n' "$TESTED_RUST" "$TESTED_FORMA" | sort -u | grep -v '^$' || true)
TESTED_COUNT=$(echo "$TESTED" | grep -c . || echo 0)

# Define high-risk builtin categories
RISK_NETWORK="tcp_accept tcp_close tcp_connect tcp_listen tcp_listener_close tcp_local_addr tcp_peer_addr tcp_read tcp_read_exact tcp_read_line tcp_set_timeout tcp_write tcp_write_all udp_bind udp_close udp_connect udp_recv udp_recv_from udp_send udp_send_to tls_close tls_connect tls_read tls_write http_delete http_get http_post http_post_json http_put http_serve http_file_response http_json_response http_redirect http_req_form http_req_header http_req_json http_req_param http_request_new http_response http_response_with_headers dns_lookup dns_reverse_lookup"
RISK_ENV="env_get env_set env_remove env_vars"
RISK_FILE="file_read file_write file_append file_copy file_move file_remove file_exists file_size file_is_file file_is_dir dir_create dir_create_all dir_list dir_remove dir_remove_all chdir"
RISK_UNSAFE="alloc alloc_zeroed dealloc mem_copy mem_set ptr_addr ptr_from_addr ptr_is_null ptr_offset ptr_null"

# Find untested builtins and classify by risk
UNTESTED=""
UNTESTED_COUNT=0
UNTESTED_RISKY=""
UNTESTED_RISKY_COUNT=0

while IFS= read -r builtin; do
    if ! echo "$TESTED" | grep -qx "$builtin"; then
        UNTESTED="${UNTESTED}  ${builtin}
"
        UNTESTED_COUNT=$((UNTESTED_COUNT + 1))
        for risk_builtin in $RISK_NETWORK $RISK_ENV $RISK_FILE $RISK_UNSAFE; do
            if [ "$builtin" = "$risk_builtin" ]; then
                UNTESTED_RISKY="${UNTESTED_RISKY}  ${builtin}
"
                UNTESTED_RISKY_COUNT=$((UNTESTED_RISKY_COUNT + 1))
                break
            fi
        done
    fi
done <<< "$BUILTINS"

# Calculate coverage percentage
if [ "$TOTAL" -gt 0 ]; then
    COVERED=$((TOTAL - UNTESTED_COUNT))
    PCT=$((COVERED * 100 / TOTAL))
else
    COVERED=0
    PCT=0
fi

echo "========================================="
echo "  FORMA Builtin Coverage Report"
echo "========================================="
echo ""
echo "Total builtins:       $TOTAL"
echo "Tested builtins:      $COVERED"
echo "Untested:             $UNTESTED_COUNT"
echo "Coverage:             ${PCT}%"
echo ""
echo "Sources:"
echo "  Rust tests:         $(echo "$TESTED_RUST" | grep -c . || echo 0) builtins"
echo "  .forma tests:       $(echo "$TESTED_FORMA" | grep -c . || echo 0) builtins"
echo ""

if [ "$UNTESTED_RISKY_COUNT" -gt 0 ]; then
    echo "HIGH-RISK untested ($UNTESTED_RISKY_COUNT):"
    echo "$UNTESTED_RISKY"
fi

if [ "$UNTESTED_COUNT" -gt 0 ]; then
    echo "All untested ($UNTESTED_COUNT):"
    echo "$UNTESTED"
fi

echo "========================================="

# Enforcement mode
if [ -n "$ENFORCE_THRESHOLD" ]; then
    if [ "$PCT" -lt "$ENFORCE_THRESHOLD" ]; then
        echo ""
        echo "FAIL: Coverage ${PCT}% is below threshold ${ENFORCE_THRESHOLD}%"
        exit 1
    else
        echo ""
        echo "PASS: Coverage ${PCT}% meets threshold ${ENFORCE_THRESHOLD}%"
    fi
fi

exit 0
