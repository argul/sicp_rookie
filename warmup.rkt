#lang racket
(require "utils.rkt")
(require "core.rkt")
(require "array.rkt")

(define (test-function-defination)
  (define (add x y)
    (+ x y))
  (add 1 2))
;(test-function-defination)

(define (test-sum)
  
  (define (sum term a next b)
    (define (sum-recursive term a next b)
      (if (> a b)
          0
          (+ (term a) (sum-recursive term (next a) next b))))
    ;(sum-recursive term a next b)
    
    (define (sum-iter term a next b result)
      (if (> a b)
          result
          (sum-iter term (next a) next b (+ result (term a)))))
    (sum-iter (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b 0))
  (sum (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 10))
;(test-sum)

(define (div-n n x y)
  (define (inner loop)
    (if (> loop n)
        (/ x y)
        (/ x (+ y (inner (+ loop 1))))))
  (inner 1))

(define (test-div-n)
  (/ 1.0 (div-n 20 1.0 1.0)))
;(test-div-n)

(define (test-nested-function)
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
;(test-nested-function)

(define (test-cons)
  (define x (cons 1 2))
  (display (car x))
  (display "/")
  (display (cdr x)))
;(test-cons)

(define (test-list)
  (display (list 1 2 3 4)))
;(test-list)

(define (test-map)
  (display (map (lambda (x) (* x x)) (list 1 2 3 4))))
;(test-map)

(define (test-reduce)
  (display (reduce (lambda (x y) (+ x y)) (list 1 2 3 4 5) 0)))
;(test-reduce)

(define (test-accumulate)
  (display (accumulate (lambda (x y) (+ x y)) 0 (list 1 2 3 4 5))))
;(test-accumulate)

(define (test-map-tree)
  (define tree (cons (list (list 1 2) 3) (list 4 5)))
  (display tree)
  (newline)
  (display (map-tree (lambda (x) (* x x)) tree))
  (newline)
  (display (tree-to-array tree)))
;(test-map-tree)

(define (test-filter)
  (define arr (list 1 2 3 4 5 6 7))
  (define (even? x) (= 0 (remainder x 2)))
  (display arr)
  (newline)
  (display (filter even? arr))
  (newline)
  (display (filter (wrap not even?) arr)))
;(test-filter)

(define (test-wrapall)
  (define (test1 x y z) #t)
  (define (test2 x y z) #t)
  (define (test3 x y z) #t)
  (define (test4 x y z) #f)
  (define result (wrapall (lambda (arr) (not (contains? #f arr))) test1 test2 test3 test4))
  (display (result 1 2 3)))
;(test-wrapall)

(define (test-arr)
  (display (make-arr 1 2 3 4))
  (newline)
  (display (concat (make-arr 1 2 3 4) (make-arr 4 5 6)))
  (newline)
  (display (reverse (make-arr 1 2 3 (make-arr 4 5) 6)))
  (newline)
  (display (cons (list (list 1 2) 3) (list 4 5)))
  (newline)
  (display (tree-to-array (cons (list (list 1 2) 3) (list 4 5))))
  (newline)
  (display (find-first (lambda (x) (> x 3)) (list 1 2 3 4 5)))
  (newline)
  (display (find-index 3 (list 1 2 3 4 5)))
  (newline)
  (display (sub-array 2 5 (list 1 2 3 4 5 6 7 8 9 10))))
(test-arr)
