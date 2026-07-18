#!/usr/bin/env python3
"""
legacy_command_census.py -- burn-down meter for the legacy KDP command surface.

Parses:
  src/NAMES.f90  -- WCC(n)='WORD' command-word table entries
  src/CMDER.f90  -- IF(WC.EQ.'WORD' ...) dispatch statements (continuations merged)

Reports:
  - total command words registered in NAMES
  - how many of those words are dispatched somewhere in CMDER
  - words with no CMDER dispatch at all (candidates: dead already, or
    dispatched outside CMDER -- informational)
  - number of dispatch statements in CMDER

Modes:
  python3 test/legacy_command_census.py                 # report
  python3 test/legacy_command_census.py --save-baseline # report + write baseline JSON
  python3 test/legacy_command_census.py --diff          # report + diff vs baseline

Baseline lives at test/legacy_command_baseline.json.
"""

import json
import os
import re
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SCRIPT_DIR)
NAMES_FILE = os.path.join(REPO_ROOT, 'src', 'NAMES.f90')
CMDER_FILE = os.path.join(REPO_ROOT, 'src', 'CMDER.f90')
BASELINE_FILE = os.path.join(SCRIPT_DIR, 'legacy_command_baseline.json')

WCC_RE = re.compile(r"WCC\s*\(\s*(\d+)\s*\)\s*=\s*'([^']*)'")
WCEQ_RE = re.compile(r"WC\s*\.EQ\.\s*'([^']*)'")


def strip_comment(line):
    """Remove a trailing ! comment, respecting single-quoted strings."""
    out = []
    in_q = False
    for ch in line:
        if ch == "'":
            in_q = not in_q
        elif ch == '!' and not in_q:
            break
        out.append(ch)
    return ''.join(out)


def code_lines(path):
    """Yield comment-stripped code lines (skips pure-comment lines)."""
    with open(path, 'r', errors='replace') as f:
        for line in f:
            stripped = line.lstrip()
            if stripped.startswith('!'):
                continue
            yield strip_comment(line.rstrip('\n'))


def merge_continuations(lines):
    """Merge Fortran free-form & continuations into single statements."""
    stmt = ''
    for line in lines:
        part = line.strip()
        if part.startswith('&'):
            part = part[1:].lstrip()
        if stmt:
            stmt += ' ' + part
        else:
            stmt = part
        if stmt.endswith('&'):
            stmt = stmt[:-1].rstrip()
            continue
        if stmt:
            yield stmt
        stmt = ''
    if stmt:
        yield stmt


def parse_names():
    """Return dict {word: [slot, ...]} of non-blank WCC entries."""
    words = {}
    for line in code_lines(NAMES_FILE):
        for m in WCC_RE.finditer(line):
            slot = int(m.group(1))
            word = m.group(2).strip()
            if word:
                words.setdefault(word, []).append(slot)
    return words


def parse_cmder():
    """Return (dispatched_words set, dispatch_statement_count)."""
    dispatched = set()
    n_stmts = 0
    for stmt in merge_continuations(code_lines(CMDER_FILE)):
        found = WCEQ_RE.findall(stmt)
        if found:
            n_stmts += 1
            for w in found:
                w = w.strip()
                if w:
                    dispatched.add(w)
    return dispatched, n_stmts


def build_census():
    names = parse_names()
    dispatched, n_stmts = parse_cmder()
    name_words = set(names)
    return {
        'names_total': len(name_words),
        'names_entries': sum(len(v) for v in names.values()),
        'cmder_dispatch_statements': n_stmts,
        'dispatched_in_cmder': sorted(name_words & dispatched),
        'no_cmder_dispatch': sorted(name_words - dispatched),
        'cmder_only': sorted(dispatched - name_words),
        'words': sorted(name_words),
    }


def print_report(c):
    print(f"NAMES command words (unique):      {c['names_total']}")
    print(f"NAMES table entries (incl. dups):  {c['names_entries']}")
    print(f"CMDER dispatch statements:         {c['cmder_dispatch_statements']}")
    print(f"  words dispatched in CMDER:       {len(c['dispatched_in_cmder'])}")
    print(f"  words with NO CMDER dispatch:    {len(c['no_cmder_dispatch'])}")
    print(f"  CMDER words not in NAMES:        {len(c['cmder_only'])}")


def print_diff(base, cur):
    removed = sorted(set(base['words']) - set(cur['words']))
    added = sorted(set(cur['words']) - set(base['words']))
    print()
    print("=== diff vs baseline ===")
    print(f"words:      {base['names_total']} -> {cur['names_total']} "
          f"({len(removed)} removed, {len(added)} added)")
    print(f"dispatch stmts: {base['cmder_dispatch_statements']} -> "
          f"{cur['cmder_dispatch_statements']}")
    if removed:
        print(f"removed ({len(removed)}):")
        for w in removed:
            print(f"  {w}")
    if added:
        print(f"added ({len(added)}):")
        for w in added:
            print(f"  {w}")


def main():
    cur = build_census()
    print_report(cur)

    if '--save-baseline' in sys.argv:
        with open(BASELINE_FILE, 'w') as f:
            json.dump(cur, f, indent=1, sort_keys=True)
            f.write('\n')
        print(f"\nBaseline saved to {os.path.relpath(BASELINE_FILE, REPO_ROOT)}")
    elif '--diff' in sys.argv:
        if not os.path.exists(BASELINE_FILE):
            print("\nNo baseline found; run with --save-baseline first.",
                  file=sys.stderr)
            return 1
        with open(BASELINE_FILE) as f:
            base = json.load(f)
        print_diff(base, cur)
    return 0


if __name__ == '__main__':
    sys.exit(main())
