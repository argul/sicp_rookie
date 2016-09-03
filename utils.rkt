#lang racket

(define (print . args)
  (define (print-inner list)
    (if (null? list)
        (newline)
        (begin (display (car list))
               (display " ")
               (print-inner (cdr list)))))
  (print-inner args))
(provide print)