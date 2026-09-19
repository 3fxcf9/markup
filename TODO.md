### General

- Make the tree walkable, create a getter and expose it to lua (allow this.parent.parent.children.content for example)
- Write builtin parsers
- optimize ?
- Number equations with respect to sections
  - look at the heading levels, remember last equation number, compare to current heading structure
  - ```lua
        function structure(headings)
    		local counters = {}

    		for _, line in ipairs(headings) do
    			local level, id, text = line:match("^(%d+);([^;]*);(.*)$")
    			level = tonumber(level)

    			counters[level] = (counters[level] or 0) + 1

    			for i = level + 1, #counters do
    					counters[i] = 0
    			end
    		end

    		local parts = {}
    		for i = 1, level do
    				parts[i] = tostring(counters[i] or 0)
    		end
    		return table.concat(parts, ".")
    	end
    ```

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
