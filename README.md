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

## Utilities for functional programming

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

## Utilities for logic programming

### `stream-let`

The `stream-let` macro is basically `do` notation from Haskell but more in line with the design of `let`.

```scheme
(stream-let ([x (stream 1 2 3)])
  (stream 'hello x)) ; (stream 'hello 1 'hello 2 'hello 3)
```

`stream-let` is algorithmically powerful when used together with the following procedures:

- `(stream-when value)` fails an execution branch when `value` is `#f`
  ```scheme
  (stream-let ([x (stream 1 2 3 4 5 6)])
    (stream-when (odd? x))
    (stream x)) ; (stream 1 3 5)
  ```
- `(stream-unless value)` fails an execution branch if `value` is truthy
  ```scheme
  (stream-let ([x (stream 1 2 3 4 5 6)])
    (stream-unless (odd? x))
    (stream x)) ; (stream 2 4 6)
  ```
- `(stream-not stream)` fails an execution branch when passed a non-empty stream and succeeds when passed an empty stream
  ```scheme
  (stream-let ([x (stream 1 2 3 4 5 6)])
    (stream-not (stream-unless (odd? x)))
    (stream x)) ; (stream 1 3 5)
  ```
- `(stream-if test-stream kleisli else-stream)` returns the flat map of `kleisli` on `test-stream` if `test-stream` is a non-empty stream, else it returns the stream `else-stream`
  ```scheme
  (stream-if
    (stream 1 2 3)
    (lambda (x) (stream x 'hey 'ho))
    (stream 'huey 'dewey 'louie)) ; (stream 1 'hey 'ho 2 'hey 'ho 3 'hey 'ho)
  ```
- `(stream-once stream)` returns a stream with the first element of `stream` or an empty stream if `stream` is empty
  ```scheme
  (stream-once (stream 'huey 'dewey 'louie)) ; (stream 'huey)
  ```
