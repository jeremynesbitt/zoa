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

Optional key:  alias: W1 W2 ...  -- other command words the block documents
(HELP W1 shows it); the alternatives at the head of the syntax (REX|REY|...)
count as aliases without it.

Rules:
  - Lines starting with !## belong to the block.
  - key: value  starts a new field.
  - Continuation lines (no key:) extend the previous field (joined with a space).
  - A bare !## line (nothing after !##) is an explicit end-of-block marker.
  - Any non-!## line also ends the current block.
"""

import re
import os
import glob
from collections import defaultdict

# Every source file holding docblocks (globbed, so a new codeV-*.f90 file is
# picked up without editing this list).
SOURCE_GLOB = 'src/*.f90'

OUTPUT_MD = 'help/md/command_table.md'
# The data file the HEL / HELP command reads at run time (same docblocks).
OUTPUT_TXT = 'help/commands.txt'
# Where the registered CODE V-style commands are listed (zoaCmds table).
REGISTRY_FILE = 'src/codeV-commands.f90'

# Ordered list of known categories for output ordering
CATEGORY_ORDER = [
    'File I/O',
    'System Parameters',
    'Fields & Wavelengths',
    'Surface Parameters',
    'Apertures',
    'Lens System Commands',
    'Optimization',
    'Analysis',
    'Plotting',
    'Plot Settings',
    'Tilts and Decenters',
    'Glass Catalogs',
    'Zoom / Multi-Configuration',
    'Utilities',
    'Diagnostics',
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
                    if current is not None and ('cmd' in current or 'section' in current):
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
                if current is not None and ('cmd' in current or 'section' in current):
                    commands.append(_finalize(current))
                current = None
                last_key = None

    # Handle block that runs to end of file
    if current is not None and ('cmd' in current or 'section' in current):
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


def generate_markdown(blocks):
    """Return the full command_table.md content as a string.

    `blocks` mixes command blocks (have a 'cmd') and section-note blocks (have a
    'section' + 'note', no 'cmd').  A section note is rendered as intro text under
    that section's header, above the table.  Add one anywhere the script scans:

        !## section:  Zoom / Multi-Configuration
        !## note:     Free text shown above the Zoom table (continuation lines ok).
        !##
    """
    by_category = defaultdict(list)
    notes = {}   # category -> intro text
    for b in blocks:
        if 'section' in b:
            notes[b['section']] = b.get('note', '')
        elif 'cmd' in b:
            by_category[b.get('category', 'Other')].append(b)

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
        if notes.get(cat):
            lines.append(notes[cat])
            lines.append('')
        lines.append('| Parameter | Description |')
        lines.append('| --------- | ----------- |')
        for cmd in sorted(by_category[cat], key=lambda x: x.get('cmd', '')):
            syntax = cmd.get('syntax', cmd.get('cmd', ''))
            desc = cmd.get('desc', '')
            # Escape '|' so it doesn't split the two-column Markdown table.
            syntax = syntax.replace('|', '\\|')
            desc = desc.replace('|', '\\|')
            lines.append(f'| {syntax} | {desc} |')
        lines.append('')

    return '\n'.join(lines) + '\n'


def generate_help_text(blocks):
    """Return the HEL/HELP data file: one record per command, fields tagged
    #CMD #SYN #CAT #DES, records separated by a blank line.  (Read by
    execHELP in src/codeV-commands-utils.f90 -- keep the tags in step.)"""
    lines = ['# Zoa command help -- generated by help/gen_docs.py from the !## docblocks.',
             '# Do not edit; edit the docblocks in the source and re-run the script.', '']
    cmds = [b for b in blocks if 'cmd' in b and not b['cmd'].startswith('(')]
    for b in sorted(cmds, key=lambda x: (x.get('cmd', ''), x.get('syntax', ''))):
        lines.append('#CMD ' + b['cmd'].strip())
        lines.append('#ALS ' + ' '.join(sorted(aliases(b))))
        lines.append('#SYN ' + b.get('syntax', b['cmd']).strip())
        lines.append('#CAT ' + b.get('category', 'Other').strip())
        lines.append('#DES ' + b.get('desc', '').strip())
        lines.append('')
    return '\n'.join(lines) + '\n'


def aliases(b):
    """The words a block documents: its cmd, and the alternatives at the head
    of its syntax ('REX|REY|ELX ... value' documents REX, REY and ELX)."""
    words = {b['cmd'].split()[0]}
    # an explicit  !## alias: XIM XRI  line names the others
    words |= set(b.get('alias', '').split())
    syn = b.get('syntax', '').strip()
    if syn:
        head = syn.split()[0]
        for w in head.split('|'):
            w = w.strip('[]')
            if re.fullmatch(r'[A-Z][A-Z0-9]*', w):
                words.add(w)
    return words


def registered_commands(path):
    """The command words registered in the zoaCmds table."""
    words = set()
    pat = re.compile(r"zoaCmds\(\s*\w+\s*\)%cmd\s*=\s*['\"]([^'\"]+)['\"]")
    with open(path, encoding='utf-8', errors='replace') as f:
        for line in f:
            if line.lstrip().startswith('!'):
                continue
            m = pat.search(line)
            if m:
                words.add(m.group(1).strip())
    return words


if __name__ == '__main__':
    # Anchor all paths to the repo root (parent of help/) so the script works
    # whether invoked as `python3 help/gen_docs.py` from the root or as
    # `python3 ./gen_docs.py` from help/ (how genHelpFiles.sh runs it).
    REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    import sys
    # --check: only verify that help/commands.txt is current (a meson test)
    CHECK = '--check' in sys.argv[1:]

    all_commands = []
    for src in sorted(glob.glob(os.path.join(REPO_ROOT, SOURCE_GLOB))):
        found = parse_source_file(src)
        if found:
            all_commands.extend(found)
            print(f'  {os.path.relpath(src, REPO_ROOT)}: {len(found)} block(s)')

    print(f'Total: {len(all_commands)} block(s)')

    if CHECK:
        txt = generate_help_text(all_commands)
        try:
            with open(os.path.join(REPO_ROOT, OUTPUT_TXT), encoding='utf-8') as f:
                current = f.read()
        except OSError:
            current = ''
        if current != txt:
            print(f'{OUTPUT_TXT} is out of date with the !## docblocks: run python3 help/gen_docs.py')
            sys.exit(1)
        print(f'{OUTPUT_TXT} is current')
        sys.exit(0)

    if not all_commands:
        print('No commands found -- outputs not modified.')
    else:
        md = generate_markdown(all_commands)
        with open(os.path.join(REPO_ROOT, OUTPUT_MD), 'w', encoding='utf-8') as f:
            f.write(md)
        print(f'Written: {OUTPUT_MD}')
        txt = generate_help_text(all_commands)
        with open(os.path.join(REPO_ROOT, OUTPUT_TXT), 'w', encoding='utf-8') as f:
            f.write(txt)
        print(f'Written: {OUTPUT_TXT}')

        # Registered commands without a docblock (HEL shows "no help yet")
        documented = set()
        for b in all_commands:
            if 'cmd' in b:
                documented |= aliases(b)
        missing = sorted(registered_commands(os.path.join(REPO_ROOT, REGISTRY_FILE)) - documented)
        if missing:
            print(f'Registered commands with no !## docblock ({len(missing)}):')
            print('  ' + ' '.join(missing))
