#!/usr/bin/env python3
"""Generate the human-readable builtin index from docs/builtins.json."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "docs" / "builtins.json"
DESTINATION = ROOT / "docs" / "builtins.md"


def cell(value: object) -> str:
    if value is None or value == []:
        return "—"
    if isinstance(value, list):
        value = ", ".join(str(item) for item in value) or "—"
    return str(value).replace("|", "\\|").replace("\n", " ")


def render() -> str:
    builtins = json.loads(SOURCE.read_text())
    builtins.sort(key=lambda item: item["name"])
    lines = [
        "# Forma 0.2 Builtin Reference",
        "",
        "This file is generated from `docs/builtins.json`. Do not edit it by hand.",
        "The JSON registry remains authoritative for tooling.",
        "",
        f"The current compiler registry contains **{len(builtins)} builtins**.",
        "",
        "| Builtin | Signature | Parameter modes | Effects | Capability | Interpreter | Native | Verification |",
        "| --- | --- | --- | --- | --- | --- | --- | --- |",
    ]
    for item in builtins:
        lines.append(
            "| `{name}` | `{signature}` | {modes} | {effects} | {capability} | {interpreter} | {native} | {verification} |".format(
                name=cell(item["name"]),
                signature=cell(item["signature"]),
                modes=cell(item["parameterModes"]),
                effects=cell(item["effects"]),
                capability=cell(item["capability"]),
                interpreter=cell(item["interpreter"]),
                native=cell(item["native"]),
                verification=cell(item["verification"]),
            )
        )
    lines.extend(
        [
            "",
            "## Reading the table",
            "",
            "- Parameter modes are ownership behavior, not merely calling syntax.",
            "- Effects describe possible authority; capabilities grant it to one execution.",
            "- Unsupported verification does not imply failure—it means the operation is outside that verification model.",
            "- Profile support is transitive through calls; consult `docs/profiles.md`.",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    rendered = render()
    if args.check:
        if not DESTINATION.exists() or DESTINATION.read_text() != rendered:
            print("docs/builtins.md is stale; run scripts/generate_builtin_reference.py")
            return 1
        print("Generated builtin reference is current")
        return 0
    DESTINATION.write_text(rendered)
    print(f"Wrote {DESTINATION.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
