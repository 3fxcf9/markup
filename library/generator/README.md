# Builtin parser generator

This executable is a build-time tool used to generate the builtin parser module. It reads all builtin parser definitions from `../../builtin_parsers/*.markup`, parses them and generates the `Generated_builtin_parsers` OCaml module containing those parsers in OCaml format (it is created inside Dune’s build directory).

This avoids loading and parsing the `.markup` files when the program starts.

The generator is automatically executed by Dune whenever a builtin parser definition changes.
