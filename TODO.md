### General

- Write builtin parsers
- optimize ?
- inline markup

### Parser syntax

- allow markup usage in build_html
- references
- `ctx` lua table
  - important ?
- parser extend
- allow empty value

### Parser ideas

- ```
  - support for footnotes^foot

  ^foot Caption here.
    label +
  ```

### Documentation

- project mode
- error (invalid parser, lua error…)
- clarify fallback parser
- external_metadata example
- note about tab indentation
- cli usage
- lua exposed functions
  - parse_markup parsed after the whole document -> no parser definition
