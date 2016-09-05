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

(define (map-tree proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree)) (map-tree proc (cdr tree))))))
(provide map-tree)

(define (filter predicator arr)
  (cond ((null? arr) nil)
        ((predicator (car arr)) (cons (car arr) (filter predicator (cdr arr))))
        (else (filter predicator (cdr arr)))))
(provide filter)

(define (filter-with-index predicator arr)

(define (contains? value arr)
  (not (null? (filter (lambda (x) (equal? x value)) arr))))
(provide contains?)

;TODO : use macros
(define (arbiter proc)
  (define (ret . args)
    (proc args))
  ret)

(define (arbiter2 proc)
  (define (ret arg1 . args)
    (proc arg1 args))
  ret)

(define (arbiter3 proc)
  (define (ret arg1 arg2 . args)
    (proc arg1 arg2 args))
  ret)

(define (wrap-imp post-process proc)
  (lambda (args) (post-process (apply proc args))))

(define (wrap post-process proc) (arbiter (wrap-imp post-process proc)))
(provide wrap)

(define (wrapall-imp post-process procs)
  (lambda (args) (post-process (map (lambda (p) (apply p args)) procs))))

(define wrapall
  (arbiter2 (lambda (post-process procs) (arbiter (wrapall-imp post-process procs)))))
(provide wrapall)
