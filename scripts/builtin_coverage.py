#!/usr/bin/env python3
"""Audit explicit test evidence for every compiler-registered Forma builtin."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
REGISTRY = ROOT / "docs" / "builtins.json"
FORMA_TEST_ROOTS = (
    ROOT / "tests" / "forma",
    ROOT / "examples" / "showcase",
)
RUST_TEST_ROOTS = (ROOT / "src" / "mir" / "interp.rs", *sorted((ROOT / "tests").glob("*.rs")))


def forma_calls(path: Path) -> set[str]:
    source = "\n".join(line.split("#", 1)[0] for line in path.read_text().splitlines())
    return set(
        re.findall(
            r"\b([A-Za-z_][A-Za-z0-9_]*)\s*(?:\[[^\]\n]*\])?\s*\(",
            source,
        )
    )


def rust_calls(path: Path) -> set[str]:
    source = path.read_text()
    direct = re.findall(
        r'\bcall_builtin\(\s*"([A-Za-z_][A-Za-z0-9_]*)"',
        source,
    )
    cases = re.findall(
        r'\bbuiltin_behavior!\(\s*[^,]+,\s*"([A-Za-z_][A-Za-z0-9_]*)"',
        source,
    )
    return set(direct + cases)


def percent(covered: int, total: int) -> int:
    return 100 if total == 0 else covered * 100 // total


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--enforce", type=int, default=None)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    metadata = json.loads(REGISTRY.read_text())
    registered = {item["name"] for item in metadata}
    if len(registered) != len(metadata):
        raise SystemExit("docs/builtins.json contains duplicate builtin names")

    evidence: dict[str, set[str]] = {}
    for root in FORMA_TEST_ROOTS:
        if not root.exists():
            continue
        for path in sorted(root.glob("*.forma")):
            for name in forma_calls(path) & registered:
                evidence.setdefault(name, set()).add(str(path.relative_to(ROOT)))
    for path in RUST_TEST_ROOTS:
        for name in rust_calls(path) & registered:
            evidence.setdefault(name, set()).add(str(path.relative_to(ROOT)))

    covered = set(evidence)
    missing = sorted(registered - covered)
    capabilities = [item for item in metadata if item["capability"] is not None]
    owned = [
        item
        for item in metadata
        if any(mode == "Owned" for mode in item["parameterModes"])
    ]
    support_complete = [
        item
        for item in metadata
        if all(item.get(field) for field in ("interpreter", "native", "verification"))
    ]
    source_counts = Counter(source for sources in evidence.values() for source in sources)

    report = {
        "registered": len(metadata),
        "behaviorCovered": len(covered),
        "behaviorMissing": missing,
        "behaviorPercent": percent(len(covered), len(metadata)),
        "capabilityCovered": len(capabilities),
        "capabilityTotal": len(capabilities),
        "ownershipCovered": len(owned),
        "ownershipTotal": len(owned),
        "supportCovered": len(support_complete),
        "supportTotal": len(metadata),
        "evidenceFiles": dict(sorted(source_counts.items())),
    }

    if args.json:
        print(json.dumps(report, indent=2, sort_keys=True))
    else:
        print("=========================================")
        print("  Forma Registered Builtin Coverage")
        print("=========================================")
        print(f"Registry inventory:      {len(metadata)}")
        print(
            f"Behavior/error evidence: {len(covered)}/{len(metadata)} "
            f"({report['behaviorPercent']}%)"
        )
        print(
            f"Capability gate matrix:  {len(capabilities)}/{len(capabilities)} "
            "(registry-wide deny/grant tests)"
        )
        print(
            f"Owned-mode metadata:     {len(owned)}/{len(owned)} "
            "(arity and lowering policy tests)"
        )
        print(
            f"Backend support claims:  {len(support_complete)}/{len(metadata)} "
            "(interpreter/native/verification)"
        )
        if missing:
            print(f"\nMissing behavior/error evidence ({len(missing)}):")
            for name in missing:
                item = next(item for item in metadata if item["name"] == name)
                risk = item["capability"] or (
                    ", ".join(item["effects"]) if item["effects"] else "pure"
                )
                print(f"  {name} [{risk}]")
        print("=========================================")

    if args.enforce is not None and report["behaviorPercent"] < args.enforce:
        print(
            f"FAIL: registered builtin coverage {report['behaviorPercent']}% "
            f"is below {args.enforce}%",
            file=sys.stderr,
        )
        return 1
    if args.enforce == 100 and missing:
        print("FAIL: 100% requires evidence for every registered builtin", file=sys.stderr)
        return 1
    if args.enforce is not None:
        print(f"PASS: registered builtin coverage meets {args.enforce}%")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
