# kaleidoscope-rs

I wanted to learn about LLVM and practice Rust, so I wrote a compiler for a toy language that mostly follows the official LLVM [Kaleidoscope tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html). There were some modifications that I made on a whim, like making function parameters comma-separated and changing the for-loop semantics slightly. The output is correct for all tutorial examples, but I haven't done much further testing.

Run interactive mode with `cargo run` and try the following:

```
>>> def binary : 1 (x, y) y
>>> def fibi(x) var a, b = 1, c in (for i = 0, i < x in c = a + b : a = b : b = c) : a
>>> fibi(10)
```

Some things to explore in the future:
- [ ] Cleanup (some code from the tutorial is hacky)
- [ ] More primitive types and a static type checker
- [ ] Structs and arrays
- [ ] Heap allocated data and garbage collection
