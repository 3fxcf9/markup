# Markup

Markup is a parser-oriented markup language written in OCaml.

It is heavily inspired from the [Scroll](https://github.com/breck7/scroll) language.

Read the language description and some example [here](docs/index.md).

## Project structure

- `builtin_parsers`: Builtin parser definitions, compiled at build time.
- `core`: Core parsing implementation.
- `markup`: Public library.
- `main`: Command-line application.

## How to use

- Build the project:
  ```bash
  dune build
  ```
- Run the CLI:
  ```bash
  dune exec main/main.exe
  ```
- Generate the documentation:
  ```bash
  dune build @doc
  ```
