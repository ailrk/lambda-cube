### UNtyped lambda calculus.

#### Let binding
Top level declarations needs to start with `let`.
```
let const = \x y -> x
```

#### IO
Supports two primitive IO operators `print` and `read`. `print`  will push a value to stdout, and `read` will read from the `stdin`.

#### Use standard library
To use standard library, import the std.lam file in the code as following.

```
import "std.lam" as s
s.mut 1 2
```

