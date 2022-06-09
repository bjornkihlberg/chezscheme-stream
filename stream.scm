(library (stream)
  (export list->stream stream stream->list stream-append)
  (import (chezscheme))
  (define-syntax stream
    (syntax-rules () [(_) '()] [(_ x xs ...) (lambda () (cons x (stream xs ...)))]))
  (define (stream->list xs)
    (let loop ([acc '()] [xs xs])
      (if (null? xs)
          (reverse acc)
          (let ([xs (xs)]) (loop (cons (car xs) acc) (cdr xs))))))
  (define (list->stream xs)
    (if (null? xs) '() (lambda () (cons (car xs) (list->stream (cdr xs))))))
  (define stream-append
    (case-lambda
      [() '()]
      [(xs) xs]
      [(xs . xss)
       (if (null? xs)
           (apply stream-append xss)
           (lambda ()
             (let ([xs (xs)])
               (cons (car xs) (apply stream-append (cons (cdr xs) xss))))))])))
