#lang racket

(require "core.rkt")

(define (make-arr . args)
  (apply list args))
(provide make-arr)

(define (arr-length arr)
  (accumulate (lambda (cur sub) (+ sub 1)) 0 arr))
(provide arr-length)

(define (concat arr1 arr2)
  (accumulate (lambda (cur result) (cons cur result))
              arr2
              arr1))
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
  (define (find-index-imp idx arr)
    (if (= 0 idx) (car arr)
        (find-index-imp (- idx 1) (cdr arr))))
  (if (or (< idx 0) (> idx (- (arr-length arr) 1))) (error "invalid input!")
      (find-index-imp idx arr)))
(provide find-index)

;end-idx < 0 means till the end
(define (sub-array start end arr)
  (define len (arr-length arr))
  (let ((max-idx (- len 1))
        (min-idx 0)
        (start-idx start)
        (end-idx (if (< end 0) (- len 1)
                     end)))
    (cond ((or (< start-idx min-idx) (> start-idx max-idx)) (error "invalid input!"))
          ((or (< end-idx min-idx) (> end-idx max-idx)) (error "invalid input!"))
          ((> start-idx end-idx) (error "invalid input!"))
          (else (filter-with-idx
                 (lambda (idx item) (>= idx start-idx))
                 (filter-with-idx
                  (lambda (idx item) (<= idx end-idx)) arr))))))
(provide sub-array)