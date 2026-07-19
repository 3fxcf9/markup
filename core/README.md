# Core library

This directory contains the core implementation of the Markup parser.

It is independent from builtin parser generation and only contains the generic parsing engine.

## Contents

Main modules:

- `types.ml`: Defines the internal data structures used by the parser.
- `markup_parser.ml`: Handles the parsing of Markup parser definitions.
- `parser.ml`: Converts a text file into a tree of particles, with their associated parser.
- `interpreter.ml`: Renders the tree into HTML.
- `lua_eval.ml`: Provides Lua evaluation support.

## Usage

This library is used by:

- the public `markup` library,
- the builtin parsers generator.

The core library does not depend on generated builtin parsers.
