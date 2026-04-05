#!/usr/bin/env python3
"""gen_docs.py — Extract !## docblocks from Fortran source files and generate command_table.md.

Usage: python3 help/gen_docs.py  (run from repo root)

Docblock format in source files:
    !## cmd:      RDY
    !## syntax:   RDY Sk X
    !## category: Surface Parameters
    !## desc:     Sets the radius of surface Sk to X.
    !##           Multi-line descriptions use continuation lines like this.
    !##
    module procedure setRadius   ← block ends here (non-!## line)

Rules:
  - Lines starting with !## belong to the block.
  - key: value  starts a new field.
  - Continuation lines (no key:) extend the previous field (joined with a space).
  - A bare !## line (nothing after !##) is an explicit end-of-block marker.
  - Any non-!## line also ends the current block.
"""

import re
import os
from collections import defaultdict

SOURCE_FILES = [
    'src/codeV-commands.f90',
    'src/codeV-sur.f90',
    'src/codeV-plot.f90',
]

OUTPUT_MD = 'help/md/command_table.md'

# Ordered list of known categories for output ordering
CATEGORY_ORDER = [
    'File I/O',
    'System Parameters',
    'Surface Parameters',
    'Solves',
    'System Management',
]

KEY_PATTERN = re.compile(r'^(\w+)\s*:\s*(.*)')


def parse_source_file(filepath):
    """Return a list of command-doc dicts extracted from !## blocks in filepath."""
    commands = []
    current = None   # dict of field -> list-of-strings while inside a block
    last_key = None  # last field key seen (for continuation lines)

    if not os.path.exists(filepath):
        print(f'  Warning: {filepath} not found, skipping.')
        return commands

    with open(filepath, encoding='utf-8', errors='replace') as f:
        for line in f:
            stripped = line.rstrip('\n')
            # Check if line starts with !## (possibly indented)
            if re.match(r'^\s*!##', stripped):
                # Strip leading whitespace and the !## prefix
                content = re.sub(r'^\s*!##', '', stripped, count=1)

                if content.strip() == '':
                    # Bare !## → explicit end-of-block
                    if current is not None and 'cmd' in current:
                        commands.append(_finalize(current))
                    current = None
                    last_key = None
                    continue

                m = KEY_PATTERN.match(content.strip())
                if m:
                    # New key: value field
                    key = m.group(1).lower()
                    value = m.group(2).strip()
                    if current is None:
                        current = {}
                    current.setdefault(key, []).append(value)
                    last_key = key
                elif current is not None and last_key is not None:
                    # Continuation of the previous field
                    current[last_key].append(content.strip())
                # else: !## line before any key — ignore (shouldn't happen in well-formed blocks)

            else:
                # Non-!## line — implicitly ends the block
                if current is not None and 'cmd' in current:
                    commands.append(_finalize(current))
                current = None
                last_key = None

    # Handle block that runs to end of file
    if current is not None and 'cmd' in current:
        commands.append(_finalize(current))

    return commands


def _finalize(raw):
    """Join multi-part field values into single strings."""
    return {k: ' '.join(v) for k, v in raw.items()}


def _category_sort_key(cat):
    try:
        return CATEGORY_ORDER.index(cat)
    except ValueError:
        return len(CATEGORY_ORDER)  # unknown categories go at the end


def generate_markdown(commands):
    """Return the full command_table.md content as a string."""
    by_category = defaultdict(list)
    for cmd in commands:
        cat = cmd.get('category', 'Other')
        by_category[cat].append(cmd)

    lines = [
        '# Command Reference',
        '',
        'Commands listed here are the new-style Zoa commands.',
        'Most original KDP commands still work — see the original manual.',
        '',
    ]

    for cat in sorted(by_category.keys(), key=_category_sort_key):
        lines.append(f'## {cat}')
        lines.append('')
        lines.append('| Parameter | Description |')
        lines.append('| --------- | ----------- |')
        for cmd in sorted(by_category[cat], key=lambda x: x.get('cmd', '')):
            syntax = cmd.get('syntax', cmd.get('cmd', ''))
            desc = cmd.get('desc', '')
            lines.append(f'| {syntax} | {desc} |')
        lines.append('')

    return '\n'.join(lines) + '\n'


if __name__ == '__main__':
    all_commands = []
    for src in SOURCE_FILES:
        found = parse_source_file(src)
        all_commands.extend(found)
        print(f'  {src}: {len(found)} command(s)')

    print(f'Total: {len(all_commands)} command(s)')

    if not all_commands:
        print('No commands found — command_table.md not modified.')
    else:
        md = generate_markdown(all_commands)
        with open(OUTPUT_MD, 'w', encoding='utf-8') as f:
            f.write(md)
        print(f'Written: {OUTPUT_MD}')
