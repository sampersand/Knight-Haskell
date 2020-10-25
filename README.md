# knight

## Example — Guessing Game
```
; = max 100                                   # max = 100
; = secret (RAND 1 max)                       # secret = rand(1, max)
; = nguess 0                                  # nguess = 0
; = guess 0                                   # guess = 0
; OUTPUT (+ 'guess 1-' max)                   # print("pick from 1-" + m)
; WHILE (| (< guess secret) (> guess secret)) # while guess != s:
  ; = guess (+ 0 (PROMPT '> '))               #   guess = int(prompt("> "))
  ; = nguess (+ nguess 1)                     #   nguess += 1
    OUTPUT (                                  #   print(
     IF (< guess secret) 'too low'            #     if guess < secret: 'too low'
     IF (> guess secret) 'too high'           #     if guess > secret: 'too high'
                         'correct')           #     else: 'correct')
OUTPUT (+ 'tries: ' nguess)                   # print("tries: " + n)
```

## Syntax
```
expr
 := nullary
  | unary expr
  | binary expr expr
  | ternary expr expr expr

nullary 
 := [0-9]+           # integer
  | `'` [^']* `'`    # single-quoted string
  | `"` [^"]* `"`    # double-string string
  | [a-z_][a-z_0-9]* # variable name
  | `T` | `TRUE`     # The value true.
  | `F` | `FALSE`    # The value false.
  | `N` | `NULL`     # The value null.
  | 'P' | 'PROMPT'   # Get a line of input from stdin.

unary
 := `B` | `BLOCK`  # A block of code (used for defining functions)
  | `C` | `CALL`   # Execute a block of code.
  | `O` | `OUTPUT` # Print something to stdout with a newline appended.
  | `Q` | `QUIT`   # Stop the program with the given exit code
  | `E` | `EVAL`   # Evaluate code as Knight code.
  | `S` | `SHELL`  # Run a shell command.
  | `!`            # Invert the next value.

binary
 := `R` | `RAND`   # Get a random int within [arg1, arg2].
  | `W` | `WHILE`  # Evaluate the second argument while the first one is true.
  | `;`            # Execute the LHS, then execute and return the RHS.
  | `=`            # assigns the RHS to the LHS; LHS should be a variable name.
  | `+` | `-` | `*` | `/` | `^` # Basic ops. `^` is power of.
  | `<` | `>`      # Basic comparison operations. 
  | `&` | `|`      # Logical AND and OR.
  
ternary
 := `I` | `IF` # The if function.
```
