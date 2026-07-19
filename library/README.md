# Markup library

This directory contains the public OCaml library, exposing the public API used to convert the Markup format into HTML.

It combines:

- the [core parser implementation](../core)
- the [builtin parser generator](generator)

Builtin parsers are loaded automatically through the generated module. This process in part of the library internals, user do not need to know how it works.

## Build process

When compiled with `dune build`, the `Generated_bultin_parsers` module is generated from the [parsers definition](../builtin_parsers). The module file is stored inside Dune’s build folder and should not be manually edited.

