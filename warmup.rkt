#lang racket

(define (test_function_defination)
  (define (add x y)
    (+ x y))
  (add 1 2))
;(test_function_defination)

(define (test_sum)
  
  (define (sum term a next b)
    (define (sum_recursive term a next b)
      (if (> a b)
          0
          (+ (term a) (sum_recursive term (next a) next b))))
    ;(sum_recursive term a next b)
    
    (define (sum_iter term a next b result)
      (if (> a b)
          result
          (sum_iter term (next a) next b (+ result (term a)))))
    (sum_iter (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b 0))
  (sum (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 10))
;(test_sum)

(define (div_n n x y)
  (define (inner loop)
    (if (> loop n)
        (/ x y)
        (/ x (+ y (inner (+ loop 1))))))
  (inner 1))
(/ 1.0 (div_n 20 1.0 1.0))

(define (test_nested_function)
  (define (sqrt x)
    (sqrt-iter 1.0 x))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.001))

  (define (average x y)
    (/ (+ x y) 2.0))

  (define (improve guess x)
    (average guess (/ x guess)))

  (sqrt 2))
;(test_nested_function)
