(library (stream)
  (export <-
          list->stream
          stream
          stream->list
          stream-append
          stream-cons
          stream-do
          stream-filter
          stream-filter-map
          stream-find
          stream-flat-map
          stream-for-each
          stream-if
          stream-iterate
          stream-let
          stream-map
          stream-not
          stream-once
          stream-skip
          stream-skip-while
          stream-take
          stream-take-while
          stream-unless
          stream-when)
  (import (chezscheme))
  (define-syntax stream-cons
    (syntax-rules () [(_ x xs) (lambda () (cons (lambda () x) xs))]))
  (define-syntax stream
    (syntax-rules ()
      [(_) (lambda () '())]
      [(_ x xs ...) (stream-cons x (stream xs ...))]))
  (define (stream->list xs)
    (let loop ([acc '()] [xs (xs)])
      (if (null? xs) (reverse acc) (loop (cons ((car xs)) acc) ((cdr xs))))))
  (define (list->stream xs)
    (if (null? xs) (stream) (stream-cons (car xs) (list->stream (cdr xs)))))
  (define stream-append
    (case-lambda
      [() (stream)]
      [(xs) xs]
      [(xs . xss)
       (let ([xs (xs)])
         (if (null? xs)
             (apply stream-append xss)
             (lambda ()
               (cons (car xs) (apply stream-append (cons (cdr xs) xss))))))]))
  (define (stream-take n xs)
    (if (positive? n)
        (lambda ()
          (let ([xs (xs)])
            (if (null? xs) '() (cons (car xs) (stream-take (sub1 n) (cdr xs))))))
        (stream)))
  (define (stream-take-while f xs)
    (let loop ([xs xs])
      (lambda ()
        (let ([xs (xs)])
          (if (null? xs)
              '()
              (let ([x ((car xs))])
                (if (f x) (cons (lambda () x) (loop (cdr xs))) '())))))))
  (define (stream-skip n xs)
    (if (positive? n)
        (lambda ()
          (let ([xs (xs)]) (if (null? xs) '() ((stream-skip (sub1 n) (cdr xs))))))
        xs))
  (define (stream-skip-while f xs)
    (let loop ([xs xs])
      (lambda ()
        (let ([xs (xs)])
          (if (null? xs)
              '()
              (let ([x ((car xs))]) (if (f x) ((loop (cdr xs))) xs)))))))
  (define (stream-filter f xs)
    (let loop ([xs xs])
      (lambda ()
        (let ([xs (xs)])
          (if (null? xs)
              '()
              (let ([x ((car xs))])
                (if (f x)
                    (cons (lambda () x) (loop (cdr xs)))
                    ((loop (cdr xs))))))))))
  (define (stream-filter-map f xs . xss)
    (let loop ([xss (cons xs xss)])
      (lambda ()
        (let ([xss (map (lambda (xs) (xs)) xss)])
          (if (exists null? xss)
              '()
              (let* ([x (apply f (map (lambda (xs) ((car xs))) xss))])
                (if x
                    (cons (lambda () x) (loop (map (lambda (xs) (cdr xs)) xss)))
                    ((loop (map (lambda (xs) (cdr xs)) xss))))))))))
  (define (stream-iterate f x)
    (lambda ()
      (let loop ([x x]) (if x (cons (lambda () x) (lambda () (loop (f x)))) '()))))
  (define (stream-map f xs . xss)
    (let loop ([xss (cons xs xss)])
      (lambda ()
        (let ([xss (map (lambda (xs) (xs)) xss)])
          (if (exists null? xss)
              '()
              (cons
                (lambda () (apply f (map (lambda (xs) ((car xs))) xss)))
                (loop (map cdr xss))))))))
  (define (stream-for-each f xs . xss)
    (let loop ([xss (cons xs xss)])
      (let ([xss (map (lambda (xs) (xs)) xss)])
        (unless (exists null? xss)
          (apply f (map (lambda (xs) ((car xs))) xss))
          (loop (map cdr xss))))))
  (define (stream-flat-map f xs)
    (let loop ([xs xs])
      (lambda ()
        (let ([xs (xs)])
          (if (null? xs) '() ((stream-append (f ((car xs))) (loop (cdr xs)))))))))
  (define (stream-when x) (if x (stream (void)) (stream)))
  (define (stream-unless x) (if x (stream) (stream (void))))
  (define (stream-not xs) (if (null? (xs)) (stream (void)) (stream)))
  (define (stream-if test-stream f else-stream)
    (let ([test-stream (test-stream)])
      (if (null? test-stream)
          else-stream
          (stream-flat-map f (lambda () test-stream)))))
  (define (stream-once xs)
    (lambda () (let ([xs (xs)]) (if (null? xs) '() (cons (car xs) (stream))))))
  (define (stream-find f xs) (stream-once (stream-filter f xs)))
  (define-syntax stream-let
    (syntax-rules ()
      [(_ () e) e]
      [(_ () e es ...) (stream-flat-map (lambda (x) (stream-let () es ...)) e)]
      [(_ ((x v) xs ...) es ...)
       (stream-flat-map (lambda (x) (stream-let (xs ...) es ...)) v)]))
  (define-syntax <- (lambda (x) (syntax-error x "invalid context")))
  (define-syntax stream-do
    (syntax-rules (<- define)
      [(_ (<- x v) e es ...) (stream-flat-map (lambda (x) (stream-do e es ...)) v)]
      [(_ (define x v ...) e es ...) (begin (define x v ...) (stream-do e es ...))]
      [(_ e) e]
      [(_ e es ...) (stream-flat-map (lambda (x) (stream-do es ...)) e)])))
