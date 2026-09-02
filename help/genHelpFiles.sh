#!/bin/bash

python3 ./gen_docs.py

# LaTeX math: +tex_math_dollars enables $...$ (inline) and $$...$$ (display).
# --mathml renders it as self-contained MathML (no network needed, works when
# the help files are opened offline).  Swap --mathml for --mathjax if you prefer
# MathJax's rendering and can rely on internet access.
# NOTE: inside the pipe-tables, write |x| as \lvert x \rvert (a bare | breaks
# the table column AND is escaped by gen_docs.py).
PANDOC_OPTS="-f gfm+tex_math_dollars -t html -s --mathml"

pandoc ./md/index.md            $PANDOC_OPTS -o ./html/index.html
pandoc ./md/command_table.md    $PANDOC_OPTS -o ./html/command_table.html
pandoc ./md/constraints_table.md $PANDOC_OPTS -o ./html/constraints_table.html

sed -i '' -e 's/command_table.md/command_table.html/g' ./html/index.html
