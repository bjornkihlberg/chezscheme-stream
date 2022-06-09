# chezscheme-stream

Lazy streams for Chez Scheme

---

Construct lazy streams with the `stream` macro.

```scheme
(stream 1 2 3)
```

- Empty streams are represented as `'()` so they can be checked with `null?`.
- Non-empty streams are represented as a suspension of a pair of a value and a stream.
  - For example, a stream of two elements `1` and `2` could be constructed directly
    ```scheme
    (lambda () (cons 1 (lambda () (cons 2 '()))))
    ```
- Turn lists into streams with `list->stream` and vice versa with `stream->list`
- Append streams with `stream-append`
