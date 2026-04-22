#!/usr/bin/env python3
"""Convert all .FOR files to free-form .f90 using findent.
Also patches C-style comments in .INC files so they're valid when
included from free-form source.

Usage:  python3 convert_for_to_f90.py
"""

import subprocess
import os
import sys
import re

SRC = os.path.join(os.path.dirname(__file__), 'src')

# ── helpers ────────────────────────────────────────────────────────────────

def fix_bare_continuations(text: str) -> str:
    """Fix two findent quirks when converting fixed → free form.

    1. 'TOKEN&'  →  'TOKEN &'   (missing space before continuation amp)

    2. '.AND &'  →  '.AND. &'   (operator dot split across lines)
       When fixed-form split '.AND.' as '...AND' / '.NEXT...', findent keeps
       that split: line N ends '...AND &', line N+1 starts '&.NEXT...'.
       We complete the operator on line N and strip the orphaned '.' from N+1.
    """
    # Fix 1: no-space-before-&
    text = re.sub(r'(\w)&(\s*)$', r'\1 &\2', text, flags=re.MULTILINE)

    # Fix 1b: fixed-form allows spaces inside numeric literals; free-form does not.
    # Pattern: digit SPACE '.' digit  →  digit '.' digit
    # (Safe: if the dot were a logical operator, it would be followed by a letter.)
    text = re.sub(r'(\d) \.(\d)', r'\1.\2', text)

    # Fix 1c: fixed-form ignores blanks within tokens, so 'INTEGERSF' means
    # 'INTEGER SF'. Free-form requires the space. Only INTEGER needs this fix
    # in this codebase — REAL/CHARACTER etc. are sometimes legitimately used
    # as identifier prefixes (e.g. REALINDEX1 is a variable name).
    text = re.sub(
        r'^(\s*(?:&\s*)?)INTEGER(?=[A-Za-z_])',
        r'\1INTEGER ', text, flags=re.IGNORECASE | re.MULTILINE,
    )

    # Fix 2: split logical operators — needs paired line processing.
    # In fixed form, '.AND.' could be split across lines as '...AND' / '.NEXT'.
    # findent reproduces this: line N ends '...AND &', next starts '&.NEXT...'.
    # Fix: add trailing '.' to the operator on line N; drop leading '.' on N+1.
    ops_end = re.compile(
        r'\.(AND|OR|NOT|EQ|NE|LT|GT|LE|GE|EQV|NEQV|TRUE|FALSE) &\s*$',
        re.IGNORECASE,
    )
    cont_dot = re.compile(r'^(\s*&)\.')

    # Fix 3: findent adds spurious '&' continuation to END IF/DO/SELECT/WHERE when
    # followed by a blank comment line ('C' in column 1 in fixed form).
    # Pattern: 'END IF &\n      C\n' → 'END IF\n' (drop the lone C line).
    end_stmt_amp = re.compile(
        r'^(\s*END\s+(?:IF|DO|SELECT|WHERE|FORALL))\s*&\s*$',
        re.IGNORECASE,
    )
    lone_c = re.compile(r'^\s+C\s*$')

    lines = text.splitlines(keepends=True)
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        me = end_stmt_amp.match(line)
        if me and i + 1 < len(lines) and lone_c.match(lines[i + 1]):
            # Emit END statement without the spurious continuation; skip lone C
            out.append(me.group(1) + '\n')
            i += 2
        else:
            m = ops_end.search(line)
            if m and i + 1 < len(lines):
                # Add missing trailing dot to operator
                op = m.group(1).upper()
                new_line = ops_end.sub(f'.{op}. &\n', line)
                out.append(new_line)
                # Drop the orphaned leading '.' from the next continuation line
                out.append(cont_dot.sub(r'\1', lines[i + 1]))
                i += 2
            else:
                out.append(line)
                i += 1
    return ''.join(out)


def convert_to_free(src_path: str, force_fixed: bool = True) -> str:
    """Run findent on a source file and return free-form output.
    force_fixed=True for .FOR files; False uses auto-detect (for .INC files
    that may already be free-form or hybrid).
    """
    input_flag = '-ifixed' if force_fixed else '-iauto'
    result = subprocess.run(
        ['findent', input_flag, '-ofree'],
        stdin=open(src_path, 'rb'),
        capture_output=True,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f'findent failed on {src_path}:\n{result.stderr.decode()}')
    text = result.stdout.decode('utf-8', errors='replace')
    return fix_bare_continuations(text)



def update_meson(meson_path: str, renames: dict) -> None:
    """Replace every 'OLD.FOR' with 'NEW.f90' in src/meson.build."""
    with open(meson_path, 'r') as f:
        text = f.read()
    for old, new in renames.items():
        text = text.replace(f"'{old}'", f"'{new}'")
    with open(meson_path, 'w') as f:
        f.write(text)


# ── main ───────────────────────────────────────────────────────────────────

def main():
    for_files = sorted(
        f for f in os.listdir(SRC)
        if f.upper().endswith('.FOR')
    )
    inc_files = sorted(
        f for f in os.listdir(SRC)
        if f.upper().endswith('.INC')
    )

    print(f'Found {len(for_files)} .FOR files and {len(inc_files)} .INC files')

    # 1. Convert .INC files in-place using findent (fixed→free form).
    # They stay as .INC (INCLUDE statements reference them by name) but
    # must be free-form since they're included from .f90 files.
    # codeV-interfaces.INC is already free-form — skip it.
    SKIP_INC = {'codeV-interfaces.INC'}
    print('\n── Converting .INC files to free form ──')
    for fname in inc_files:
        if fname in SKIP_INC:
            print(f'  skip     {fname}')
            continue
        path = os.path.join(SRC, fname)
        try:
            free_source = convert_to_free(path, force_fixed=False)
        except RuntimeError as e:
            print(f'  ERROR    {fname}: {e}')
            continue
        with open(path, 'w') as f:
            f.write(free_source)
        print(f'  converted {fname}')

    # 2. Convert each .FOR → .f90
    print('\n── Converting .FOR → .f90 ──')
    renames = {}   # basename_old -> basename_new, for meson update
    errors = []

    for fname in for_files:
        src_path = os.path.join(SRC, fname)
        new_fname = fname[:-4] + '.f90'   # e.g. RAYTRA1.FOR → RAYTRA1.f90
        dst_path = os.path.join(SRC, new_fname)

        try:
            free_source = convert_to_free(src_path, force_fixed=True)
        except RuntimeError as e:
            print(f'  ERROR    {fname}: {e}')
            errors.append(fname)
            continue

        with open(dst_path, 'w') as f:
            f.write(free_source)

        os.remove(src_path)
        renames[fname] = new_fname
        print(f'  converted {fname} → {new_fname}')

    # 3. Update src/meson.build
    meson_path = os.path.join(SRC, 'meson.build')
    print(f'\n── Updating {meson_path} ──')
    update_meson(meson_path, renames)
    print(f'  replaced {len(renames)} filenames')

    if errors:
        print(f'\n{len(errors)} file(s) failed conversion: {errors}')
        sys.exit(1)
    else:
        print(f'\nDone. {len(renames)} files converted.')


if __name__ == '__main__':
    main()
