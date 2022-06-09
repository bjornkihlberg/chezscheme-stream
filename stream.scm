(library (stream)
  (export list->stream stream stream->list stream-append stream-cons)
  (import (chezscheme))
  (define-syntax stream-cons
    (syntax-rules () [(_ x y) (cons (lambda () x) (lambda () y))]))
  (define-syntax stream
    (syntax-rules () [(_) '()] [(_ x xs ...) (stream-cons x (stream xs ...))]))
  (define (stream->list xs)
    (let loop ([acc '()] [xs xs])
      (if (null? xs) (reverse acc) (loop (cons ((car xs)) acc) ((cdr xs))))))
  (define (list->stream xs)
    (if (null? xs) (stream) (stream-cons (car xs) (list->stream (cdr xs)))))
  (define stream-append
    (case-lambda
      [() (stream)]
      [(xs) xs]
      [(xs . xss)
       (if (null? xs)
           (apply stream-append xss)
           (cons
             (car xs)
             (lambda () (apply stream-append (cons ((cdr xs)) xss)))))])))
