#lang racket
(require "core.rkt")

(define (make-arr . args)
  (apply list args))
(provide make-arr)

(define (arr-length arr)
  (accumulate (lambda (cur sub) (+ sub 1)) 0 arr))
(provide arr-length)

(define (concat arr1 arr2)
  (if (null? arr1) arr2
      (cons (car arr1) (concat (cdr arr1) arr2))))
(provide concat)

(define (reverse arr)
  (if (null? arr) arr
      (concat (reverse (cdr arr)) (make-arr (car arr)))))
(provide reverse)

(define (tree-to-array tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (concat (tree-to-array (car tree)) (tree-to-array (cdr tree))))))
(provide tree-to-array)

(define (find-first predicator arr)
  (cond ((null? arr) nil)
        ((predicator (car arr)) (car arr))
        (else (find-first predicator (cdr arr)))))
(provide find-first)

(define (find-index idx arr)
  (define (iter idx arr)
    (if (= 0 idx) (car arr)
        (iter (- idx 1) (cdr arr))))
  (if (or (< idx 0) (> idx (- (arr-length arr) 1))) (error "input index is invalid!")
      (iter idx arr)))
(provide find-index)