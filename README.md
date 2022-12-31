# belki

## Quick Start

---

### For examples see [here](examples)

| Run REPL  | Run from file    |
|-----------|------------------|
| `./belki` | `./belki <file>` |

---

### Hello, World

```belki
print("Hello, World!") 
// or
"Hello, World!" |> print
```

---

### Recursive Fibanocci

```belki
fun fib(n) {
    if n <= 2
        return 1
    return fib(n-1) + fib(n-2)
}
```

---
