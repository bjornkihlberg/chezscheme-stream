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
- `stream-for-each`
- `stream-append` appends any number of streams

## Utility procedures for functional programming

- `stream-take` cuts off a stream after `n` elements.
  ```scheme
  (stream-take 3 (stream 1 2 3 4 5 6 7)) ; (stream 1 2 3)
  ```
- `stream-take-while` cuts off a stream after last element passing a given predicate
  ```scheme
  (stream-take-while even? (stream 2 4 6 1 8 3 10)) ; (stream 2 4 6)
  ```
- `stream-skip` drops `n` elements from a stream
  ```scheme
  (stream-skip 3 (stream 1 2 3 4 5 6 7)) ; (stream 4 5 6 7)
  ```
- `stream-skip-while` drops elements from a stream while elements pass a given predicate
  ```scheme
  (stream-skip-while even? (stream 2 4 6 1 8 3 10)) ; (stream 1 8 3 10)
  ```
- `stream-map` maps elements
  ```scheme
  (stream-map add1 (stream 1 2 3 4)) ; (stream 2 3 4 5)
  ```
- `stream-filter` keeps elements passing a given predicate
  ```scheme
  (stream-filter odd? (stream 1 2 3 4 5 6 7 8 9)) ; (stream 1 3 5 7 9)
  ```
- `stream-filter-map` maps and keeps truthy elements
  ```scheme
  (stream-filter-map (lambda (x) (and (even? x) (- x))) (stream 1 2 3 4)) ; (stream -2 -4)
  ```
- `stream-iterate` builds a potentially infinite stream or until given procedure returns `#f`
  ```scheme
  (stream-iterate add1 0) ; (stream 0 1 2 3 4 ...
  ```
- `stream-flat-map` maps individual elements to streams and appends them
  ```scheme
  (stream-flat-map (lambda (x) (stream 'hey 'ho x)) (stream 1 2)) ; (stream 'hey 'ho 1 'hey 'ho 2)
  ```
