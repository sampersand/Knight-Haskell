# knight

## Syntax
```
expr
	:= primary
	| unary expr
	| binary expr expr
	| ternary expr expr expr

primary
	:= `null`
	| `true` | `false`
	| [a-z_][a-z_0-9]*
	| [0-9]+
	| `'` [^']* `'` | `"` [^"]* `"`

unary
	:= `!`
	 | `F` | `FNDEF`
	 | `C` | `CALL`
	 | `P` | `PROMPT`
	 | `O` | `OUTPUT`
	 | `Q` | `QUIT`
	 | `E` | `EVAL`
	 | `S` | `SYSTEM`

binary
	:= `R` | `RAND`
	 | `W` | `WHILE`
	 | `;` | `=`
	 | `+` | `-` | `*` | `/` | `^`
	 | `<` | `>` | `&` | `|`

ternary
	:= `IF`
```
