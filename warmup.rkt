#lang racket

(define (test_function_defination)
  (define (add x y)
    (+ x y))
  (add 1 2))
(test_function_defination)