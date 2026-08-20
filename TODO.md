### General

- Write builtin parsers
- optimize ?

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
- escaping
- lua exposed functions
  - parse_markup parsed after the whole document -> no parser definition
- builtin parsers (by default, only parser definitions are seen but can be parsed as markup files. Just create a new file)
- update aftertext accepting a pattern
