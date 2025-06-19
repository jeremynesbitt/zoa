#!/bin/bash

pandoc ./md/index.md            -f gfm -t html -s -o ./html/index.html
pandoc ./md/command_table.md    -f gfm -t html -s -o ./html/command_table.html
pandoc ./md/constraints_table.md -f gfm -t html -s -o ./html/constraints_table.html

sed -i -e 's/command_table.md/command_table.html/g' ./html/index.html
