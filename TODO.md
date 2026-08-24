### General

- Write builtin parsers
- optimize ?
- use http_root

### Parser syntax

- references
- `ctx` lua table
  - important ?
- allow empty value

### Parser ideas

```markup
parser exo
	extends db

exo
	date 12/01/2026
	text
		markup here
			bold here
	difficulty 4

exo
	date 13/01/2026
	text
		other exercise
	difficulty 3

parser display_exo
	build_html
		<div class="exercise">
			<span class="date">$sub[date].content</span>
			<span class="difficulty">Difficulty: $sub[difficulty].arg</span>
			<span class="date">$sub[text].content</span>
		</div>


// parser display_db
	build_html
		lua
		for _, p in ipairs(metadata[this.atoms[2]]) do
			if p.name == "left" then
				attr = ' class="float-left"'
				break
			elseif p.name == "right" then
				attr = ' class="float-right"'
				break
			end
		end


display_db exo display_exo
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
- extends
