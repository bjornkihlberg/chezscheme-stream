# chezscheme-stream

Lazy streams for Chez Scheme

---

## Core

Construct a lazy pair with the `stream-cons` macro:

```scheme
(stream-cons 1 2)
```

which compiles to:

```scheme
(cons (lambda () 1) (lambda () 2))
```

Construct lazy list-like streams with the `stream` macro.

```scheme
(stream 1 2 3)
```

which compiles to

```scheme
(cons (lambda () 1) (lambda () (cons (lambda () 2) (lambda () (cons (lambda () 3) '())))))
```

Since streams are represented this way they can be checked with `null?` and `pair?` and values can be accessed with `car` and `cdr` (The suspensions still need to be evaluated, ofcourse).

## Utility procedures

- `stream->list`
- `list->stream`
- `stream-append`
