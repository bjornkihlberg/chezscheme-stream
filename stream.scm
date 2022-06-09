(library (stream)
  (export stream)
  (import (chezscheme))
  (define-syntax stream
    (syntax-rules ()
      [(_) '()]
      [(_ x xs ...) (lambda () (cons x (stream xs ...)))])))
