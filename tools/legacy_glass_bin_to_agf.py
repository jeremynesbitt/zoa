#!/usr/bin/env python3
"""One-time conversion of Zoa's legacy binary glass catalogs to AGF.

Usage:  python3 tools/legacy_glass_bin_to_agf.py <repo root>

Kept as the record of how Library/LIBGLA/*.AGF were made.  The .BIN files
were removed in the same commit; to rerun, check them out from its parent.

Reads Library/LIBGLA/*.BIN (74-byte records: 13-char name, 13-char number,
six real64 coefficients; record 1 holds the glass count) and writes one AGF
per catalog into Library/LIBGLA.  The coefficients are carried over exactly
(Python's shortest round-trip repr), so the indices do not change:

  ID_PNSC catalogs (Sellmeier: n^2 = 1 + sum A_i L^2/(L^2 - A_{i+3}))
      -> AGF formula 2 (Sellmeier 1), CD K1 L1 K2 L2 K3 L3
  ID_PN catalogs (Schott: n^2 = A0 + A1 L^2 + A2 L^-2 + ... + A5 L^-8)
      -> AGF formula 1 (Schott), CD A0..A5

Each glass's LD (wavelength range) line is what the legacy LAMHILO routine
returns for it as GLSRES calls it -- LAMHILO(catalog, glass, ' ') -- read
from src/LDM18.f90, so the wavelength-range checks do not change either.
OHARA.BIN and OHARA-O.BIN (both catalog OHARA) are merged into OHARA.AGF.
A glass listed twice keeps its first entry, as the legacy lookup did.
"""
import math
import os
import re
import struct
import sys

PNSC = 2   # AGF Sellmeier 1
PN = 1     # AGF Schott

# catalog -> source files in search order, each with its formula.  The
# formula is the one the data fit: every glass's index at the d line matches
# its glass code (nd = 1 + first three digits / 1000) under that formula.
# OHARA-O.BIN is Schott-formula data although legacy GLSRES evaluated it as
# Sellmeier (giving e.g. nd 1.79 for i-FPL51Y, code 497811).
CATALOGS = [
    ('SCHOTT', [('SCHOTT.BIN', PNSC)]),
    ('SCH2000', [('SCH2000.BIN', PNSC)]),
    ('OHARA', [('OHARA.BIN', PNSC), ('OHARA-O.BIN', PN)]),
    ('HOYA', [('HOYA.BIN', PN)]),
    ('CHANCE', [('CHANCE.BIN', PN)]),
    ('CORNIN', [('CORNIN.BIN', PN)]),
    ('HIKARI', [('HIKARI.BIN', PN)]),
    ('RADHARD', [('RADHARD.BIN', PN)]),
    ('SCH2023', [('SCH2023.BIN', PNSC)]),
]
DESCRIPTIONS = {
    'SCHOTT': 'Schott Optical Glass Catalog',
    'SCH2000': 'SCHOTT POST-2000 Optical Glass Catalog',
    'OHARA': 'OHara Optical Glass Catalog',
    'HOYA': 'Hoya Optical Glass Catalog',
    'CHANCE': 'Chance-Pilkington Optical Glass Catalog',
    'CORNIN': 'Corning-France Optical Glass Catalog',
    'HIKARI': 'Hikara Optical Glass Catalog',
    'RADHARD': 'Radiation Resistant Optical Materials',
    'SCH2023': 'Schott 2023 Preferred Glass Catalog',
}
REC = 74


def read_bin(path):
    data = open(path, 'rb').read()
    total = struct.unpack_from('<i', data, 0)[0]
    out = []
    for j in range(1, total + 1):
        off = j * REC
        name = data[off:off + 13].decode('latin-1').rstrip()
        number = data[off + 13:off + 26].decode('latin-1').strip()
        coeffs = struct.unpack_from('<6d', data, off + 26)
        out.append((name, number, coeffs))
    return out


def fortran_real(s):
    return float(s.replace('D', 'E').replace('d', 'e'))


def lamhilo_rules(src):
    """The LAMHILO IF blocks, in order: (conditions, low, high)."""
    m = re.search(r'SUBROUTINE LAMHILO.*?\nEND', src, re.S)
    # join continuation lines, so each IF condition sits on one line
    body = re.sub(r'&[ \t]*\n[ \t]*&?', '', m.group(0))
    rules = []
    pat = re.compile(r"IF\s*\(([^\n]*)\)\s*THEN[ \t]*\n\s*LAMLOW=([0-9.D+-]+)\s*\n\s*LAMUPP=([0-9.D+-]+)")
    for cond, lo, hi in pat.findall(body):
        assert '.AND.' not in cond.upper(), cond
        terms = re.findall(r"(NAME1|NAME2|FLNAME)\.EQ\.'([^']*)'", cond)
        if not terms:          # the LOWER (FLNAME list) block
            continue
        rules.append((terms, fortran_real(lo), fortran_real(hi)))
    return rules


def limits(rules, cat, glass):
    lo, hi = 0.0, 1.0e10
    for terms, rlo, rhi in rules:
        for var, val in terms:
            val = val.rstrip()
            if (var == 'NAME1' and cat == val) or (var == 'NAME2' and glass == val):
                lo, hi = rlo, rhi
                break
            # FLNAME is blank when GLSRES calls LAMHILO: never matches
    return lo, hi


def index(formula, c, lam):
    l2 = lam * lam
    if formula == PNSC:
        n2 = 1.0 + c[0] * l2 / (l2 - c[3]) + c[1] * l2 / (l2 - c[4]) + c[2] * l2 / (l2 - c[5])
    else:
        n2 = c[0] + c[1] * l2 + c[2] / l2 + c[3] / l2**2 + c[4] / l2**3 + c[5] / l2**4
    return math.sqrt(n2)


def agf_coeffs(formula, c):
    if formula == PNSC:
        return [c[0], c[3], c[1], c[4], c[2], c[5]]
    return list(c)


def num(x):
    return repr(float(x))


def main():
    root = sys.argv[1]
    lib = os.path.join(root, 'Library', 'LIBGLA')
    rules = lamhilo_rules(open(os.path.join(root, 'src', 'LDM18.f90'), encoding='latin-1').read())
    for cat, sources in CATALOGS:
        files = [f for f, _ in sources]
        seen = set()
        lines = ['CC ' + DESCRIPTIONS[cat],
                 '! Converted from the legacy Zoa ' + ', '.join(files)
                 + ' by tools/legacy_glass_bin_to_agf.py']
        count = 0
        for f, formula in sources:
            for name, number, c in read_bin(os.path.join(lib, f)):
                # AGF names are single tokens.  A few old Ohara names have a
                # space ('S-FSL 5'); Ohara's own spelling has none, and no
                # dropped-space name collides with another glass.
                name = name.replace(' ', '')
                assert name, f
                if name in seen:
                    continue
                seen.add(name)
                nd = index(formula, c, 0.5875618)
                nf = index(formula, c, 0.4861327)
                nc = index(formula, c, 0.6562725)
                vd = (nd - 1.0) / (nf - nc) if nf != nc else 0.0
                mil = number if number and ' ' not in number else '0'
                lo, hi = limits(rules, cat, name)
                lines.append('NM %s %d %s %.6f %.2f 0 0 0' % (name, formula, mil, nd, vd))
                lines.append('ED 0 0 0 0 0')
                lines.append('CD ' + ' '.join(num(x) for x in agf_coeffs(formula, c)))
                lines.append('TD 0 0 0 0 0 0 20')
                lines.append('OD -1 -1 -1 -1 -1 -1')
                lines.append('LD %s %s' % (num(lo), num(hi)))
                count += 1
        out = os.path.join(lib, cat + '.AGF')
        with open(out, 'w', newline='\n') as fh:
            fh.write('\n'.join(lines) + '\n')
        print('%-8s %4d glasses -> %s' % (cat, count, out))


if __name__ == '__main__':
    main()
