#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

compiler="${1:-}"
if [[ -z "$compiler" ]]; then
    if [[ -x target/debug/forma ]]; then
        compiler="target/debug/forma"
    elif [[ -x target/release/forma ]]; then
        compiler="target/release/forma"
    else
        cargo build --quiet
        compiler="target/debug/forma"
    fi
fi

package_version="$(awk -F '"' '/^version = "/ { print $2; exit }' Cargo.toml)"
reported_version="$($compiler --version | awk '{print $2}')"

if [[ "$reported_version" != "$package_version" ]]; then
    echo "version drift: Cargo.toml=$package_version compiler=$reported_version" >&2
    exit 1
fi

"$compiler" grammar --check
python3 scripts/generate_builtin_reference.py --check
python3 scripts/builtin_coverage.py --enforce 100

python3 - <<'PY'
import json
import re
from pathlib import Path

required = {
    "README.md": ["affine ownership", "TESTED", "Core", "Hosted", "Native", "Experimental"],
    "docs/ai-reference.md": ["Immutable binding", "docs/builtins.json", "COUNTEREXAMPLE", "UNKNOWN"],
    "docs/reference.md": ["## Ownership", "## Effects & Capabilities", "## Feature Profiles"],
    "skills/forma/SKILL.md": ["Affine ownership", "capabilities", "Verification"],
    "skills/forma-codex/SKILL.md": ["Affine Ownership Rules", "Profiles and Verification Confidence"],
}

for filename, needles in required.items():
    text = Path(filename).read_text()
    for needle in needles:
        if needle not in text:
            raise SystemExit(f"{filename}: missing required 0.2 documentation marker {needle!r}")

builtins = json.loads(Path("docs/builtins.json").read_text())
if not isinstance(builtins, list) or not builtins:
    raise SystemExit("docs/builtins.json must contain a non-empty generated list")

names = [item.get("name") for item in builtins]
if len(names) != len(set(names)):
    raise SystemExit("docs/builtins.json contains duplicate builtin names")

required_fields = {
    "name", "signature", "parameterModes", "effects", "capability",
    "interpreter", "native", "verification", "documentation",
}
for index, item in enumerate(builtins):
    missing = required_fields - item.keys()
    if missing:
        raise SystemExit(f"docs/builtins.json item {index} missing {sorted(missing)}")

cargo = Path("Cargo.toml").read_text()
version = re.search(r'^version = "([^"]+)"', cargo, flags=re.M).group(1)
print(f"documentation consistency OK: Forma {version} and {len(builtins)} generated builtins")
PY

if [[ -f ../forma-website/reference-data.js ]]; then
    python3 scripts/generate_website_reference.py --check
fi
