# dipl

**D**ynamic, **I**nterpreted, **P**rogramming **L**anguage

## Quick Start

---

### For examples see [here](examples)

| Run REPL | Run from file   |
|----------|-----------------|
| `./dipl` | `./dipl <file>` |

---

### Hello, World

```
print("Hello, World!") 
// or
"Hello, World!" |> print
```

---

### Recursive Fibanocci

```
fun fib(n) {
    if n <= 2
        return 1
    return fib(n-1) + fib(n-2)
}
```

---
