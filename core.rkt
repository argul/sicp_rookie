#lang racket

(define nil '())
(provide nil)

(define (map proc arr)
  (if (null? arr)
      nil
      (cons (proc (car arr)) (map proc (cdr arr)))))
(provide map)

(define (reduce proc arr nullval)
  (if (null? arr)
      nullval
      (proc (car arr) (reduce proc (cdr arr) nullval))))
(provide reduce)

(define (accumulate proc initial arr)
  (if (null? arr) initial
      (proc (car arr) (accumulate proc initial (cdr arr)))))
(provide accumulate)

(define (accumulate-reverse proc initial arr)
  (define (accumulate-reverse-imp proc result arr)
    (if (null? arr) result
        (accumulate-reverse-imp proc (proc (car arr) result) (cdr arr))))
  (accumulate-reverse-imp proc initial arr))
(provide accumulate-reverse)
      

(define (map-tree proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree)) (map-tree proc (cdr tree))))))
(provide map-tree)

(define (filter predicator arr)
  (cond ((null? arr) '())
        ((predicator (car arr)) (cons (car arr) (filter predicator (cdr arr))))
        (else (filter predicator (cdr arr)))))
(provide filter)

(define (filter-with-idx predicator arr)
  (define (filter-with-idx-imp predicator arr idx)
    (cond ((null? arr) '())
          ((predicator idx (car arr)) (cons (car arr) (filter-with-idx-imp predicator (cdr arr) (+ idx 1))))
          (else (filter-with-idx-imp predicator (cdr arr) (+ idx 1)))))
  (filter-with-idx-imp predicator arr 0))
(provide filter-with-idx)

(define (contains? value arr)
  (not (null? (filter (lambda (x) (equal? x value)) arr))))
(provide contains?)

;TODO : use macros
(define (arbiter proc)
  (lambda args (proc args)))

(define (arbiter2 proc)
  (lambda (arg1 . args) (proc arg1 args)))

(define (arbiter3 proc)
  (lambda (arg1 arg2 . args) (proc arg1 arg2 args)))

(define (wrap-imp post-process proc)
  (lambda (args) (post-process (apply proc args))))

(define (wrap post-process proc) (arbiter (wrap-imp post-process proc)))
(provide wrap)

(define (wrapall-imp post-process procs)
  (lambda (args) (post-process (map (lambda (p) (apply p args)) procs))))

(define wrapall
  (arbiter2 (lambda (post-process procs) (arbiter (wrapall-imp post-process procs)))))
(provide wrapall)
