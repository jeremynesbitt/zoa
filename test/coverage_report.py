#!/usr/bin/env python3
"""
coverage_report.py -- Zoa command coverage checker.

Parses src/codeV-commands.f90 for zoaCmds(N)%cmd registrations,
then scans test/*.zoa scripts to find which registered commands are exercised.

Prints:
  TESTED (n):    sorted list of covered commands
  UNTESTED (n):  sorted list of uncovered registered commands
  USED-BUT-UNREGISTERED (n): commands used in tests but not in the registry
                              (KDP-legacy commands, informational only)

Usage: python3 test/coverage_report.py
"""

import re
import sys
import os
import glob

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SCRIPT_DIR)
COMMANDS_FILE = os.path.join(REPO_ROOT, 'src', 'codeV-commands.f90')
TEST_DIR = SCRIPT_DIR

# ---------------------------------------------------------------------------
# 1. Parse registered commands from src/codeV-commands.f90
# ---------------------------------------------------------------------------
# Match lines like:
#   zoaCmds(NNN)%cmd = "XXX"   or   = 'XXX'
# Skip comment lines (first non-blank char is '!')
# Skip S1..S499 surface aliases (registered in a do-loop, not explicitly)
CMD_RE = re.compile(
    r"""zoaCmds\s*\(\s*\d+\s*\)\s*%cmd\s*=\s*['"]([^'"]+)['"]""",
    re.IGNORECASE
)
SURFACE_ALIAS_RE = re.compile(r'^S\d{1,3}$')  # S1..S499

registered_commands = set()

with open(COMMANDS_FILE, 'r', errors='replace') as f:
    for line in f:
        stripped = line.lstrip()
        if stripped.startswith('!'):
            continue  # comment line
        m = CMD_RE.search(line)
        if m:
            cmd = m.group(1).strip().upper()
            if SURFACE_ALIAS_RE.match(cmd):
                continue  # skip S1..S499 surface aliases
            if cmd:
                registered_commands.add(cmd)

# ---------------------------------------------------------------------------
# 2. Scan test/*.zoa for used commands
# ---------------------------------------------------------------------------
# For each non-comment line, split on ';', take first whitespace token, uppercase.
used_commands = set()

zoa_files = glob.glob(os.path.join(TEST_DIR, '*.zoa'))
for zoa_file in sorted(zoa_files):
    with open(zoa_file, 'r', errors='replace') as f:
        for line in f:
            stripped = line.strip()
            if not stripped or stripped.startswith('!'):
                continue  # blank or comment
            # Split on ';' to handle multi-command lines like "VIE ; GO"
            segments = stripped.split(';')
            for seg in segments:
                seg = seg.strip()
                if not seg or seg.startswith('!'):
                    continue
                tokens = seg.split()
                if tokens:
                    used_commands.add(tokens[0].upper())

# ---------------------------------------------------------------------------
# 3. Categorise
# ---------------------------------------------------------------------------
tested = sorted(used_commands & registered_commands)
untested = sorted(registered_commands - used_commands)
unregistered = sorted(used_commands - registered_commands)

# ---------------------------------------------------------------------------
# 4. Print report
# ---------------------------------------------------------------------------
def print_section(title, items):
    print(f"{title} ({len(items)}):")
    for item in items:
        print(f"  {item}")
    print()

print_section("TESTED", tested)
print_section("UNTESTED", untested)
print_section("USED-BUT-UNREGISTERED", unregistered)

sys.exit(0)
