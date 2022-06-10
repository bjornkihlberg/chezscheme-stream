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

- `(stream->list stream)` collects elements from `stream` and returns them in a list
  ```scheme
  (stream->list (stream 1 2 3)) ; '(1 2 3)
  ```
- `(list->stream list)` populates a stream with elements from `list`
  ```scheme
  (list->stream '(1 2 3)) ; (stream 1 2 3)
  ```
- `(stream-for-each f stream . streams)` zips `stream` and `streams`, runs them with `f` and returns `(void)`
  ```scheme
  (stream-for-each (lambda (x) (format #t "~a\n" x)) (stream 1 2 3)) ; (void)
  ```
- `(stream-append . streams)` appends `streams`
  ```scheme
  (stream-append (stream 1 2) (stream 3 4) (stream 5 6)) ; (stream 1 2 3 4 5 6)
  ```

## Utilities for functional programming

- `(stream-take n stream)` cuts off `stream` after `n` elements.
  ```scheme
  (stream-take 3 (stream 1 2 3 4 5 6 7)) ; (stream 1 2 3)
  ```
- `(stream-take-while predicate stream)` cuts off `stream` after last element passing `predicate`
  ```scheme
  (stream-take-while even? (stream 2 4 6 1 8 3 10)) ; (stream 2 4 6)
  ```
- `(stream-skip n stream)` drops `n` elements from `stream`
  ```scheme
  (stream-skip 3 (stream 1 2 3 4 5 6 7)) ; (stream 4 5 6 7)
  ```
- `(stream-skip-while predicate stream)` drops elements from `stream` while elements pass `predicate`
  ```scheme
  (stream-skip-while even? (stream 2 4 6 1 8 3 10)) ; (stream 1 8 3 10)
  ```
- `(stream-map f stream . streams)` zips and maps elements in `stream` and `streams`
  ```scheme
  (stream-map add1 (stream 1 2 3 4)) ; (stream 2 3 4 5)
  ```
- `(stream-filter predicate stream)` keeps elements from `stream` passing `predicate`
  ```scheme
  (stream-filter odd? (stream 1 2 3 4 5 6 7 8 9)) ; (stream 1 3 5 7 9)
  ```
- `(stream-filter-map f stream . streams)` zips, maps and keeps truthy elements returned by `f`
  ```scheme
  (stream-filter-map (lambda (x) (and (even? x) (- x))) (stream 1 2 3 4)) ; (stream -2 -4)
  ```
- `(stream-iterate f x)` builds a potentially infinite stream by collecting elements from `f` until it returns `#f`
  ```scheme
  (stream-iterate add1 0) ; (stream 0 1 2 3 4 ...
  ```
- `(stream-flat-map kleisli stream . streams)` maps individual elements to streams with `kleisli` and appends them
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
