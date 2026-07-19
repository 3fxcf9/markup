# Builtin parsers

This directory contains the builtin parser definitions used by Markup.

During the build process, all `.markup` files in this directory will be:
- loaded by the parser generator,
- converted into OCaml values,
- embedded into the final executable/library.

The program does not read these files when it starts.

## Adding a builtin parser

Simply add a parser definition to any of the `.markup` files or create a new `.markup` file. Then, rebuild the module with `dune build`.
