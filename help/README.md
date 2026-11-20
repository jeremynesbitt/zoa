At present, the Zoa help files are written in github compatible Markdown and translated to
html with the genHelpFiles.sh script.

So to generate the help files, make sure there is a html sub directory and run the genHelpFiles.sh script.



## Command help (HELP / HEL) and the docblocks

Every command's help comes from the `!##` docblock above its handler in
`src/*.f90` (keys `cmd`, `syntax`, `category`, `desc`, optional `alias`).
`python3 help/gen_docs.py` turns them into `md/command_table.md` and
`commands.txt` -- the data file the `HELP` / `HEL` command reads at run time
(installed in the app bundle's `Resources/help`).  It also lists registered
commands that have no docblock.  After editing a docblock, re-run the
script and commit `commands.txt`; the `help_data_current` test fails while
it is out of date.
