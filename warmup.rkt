#lang racket
(require "utils.rkt")
(require "core.rkt")

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

(define (test_div_n)
  (/ 1.0 (div_n 20 1.0 1.0)))
;(test_div_n)

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

(define (test_cons)
  (define x (cons 1 2))
  (display (car x))
  (display "/")
  (display (cdr x)))
;(test_cons)

(define (test_list)
  (display (list 1 2 3 4)))
;(test_list)

(define (test_map)
  (display (map (lambda (x) (* x x)) (list 1 2 3 4))))
;(test_map)

(define (test_reduce)
  (display (reduce (lambda (x y) (+ x y)) (list 1 2 3 4 5) 0)))
;(test_reduce)

(define (test_map_tree)
  (define tree (cons (list (list 1 2) 3) (list 4 5)))
  (display tree)
  (newline)
  (display (map_tree (lambda (x) (* x x)) tree)))
;(test_map_tree)

(define (test_filter)
  (define arr (list 1 2 3 4 5 6 7))
  (define (even? x) (= 0 (remainder x 2)))
  (display arr)
  (newline)
  (display (filter even? arr))
  (newline)
  (display (filter (wrap not even?) arr)))
;(test_filter)

(define (test_wrapall)
  (define (test1 x y z) #t)
  (define (test2 x y z) #t)
  (define (test3 x y z) #t)
  (define (test4 x y z) #f)
  (define result (wrapall (lambda (arr) (not (contains? #f arr))) test1 test2 test3 test4))
  (display (result 1 2 3)))
(test_wrapall)