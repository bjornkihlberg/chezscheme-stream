# chezscheme-stream

Lazy streams for Chez Scheme

---

## Core

Construct lazy list-like streams with the `stream` macro.

```scheme
(stream 1 2 3)
```

which compiles to

```scheme
(lambda () (cons (lambda () 1) (lambda () (cons (lambda () 2) (lambda () (cons (lambda () 3) '()))))))
```

Since streams are represented this way they can be evaluated once to be checked with `null?` and `pair?` without touching the individual values. The values can be accessed with `car` and `cdr` (The suspensions still need to be evaluated, ofcourse).

## Utility procedures

- `stream->list`
- `list->stream`
- `stream-append`
