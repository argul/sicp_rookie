#lang racket

(define nil '())

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

(define (map_tree proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (map_tree proc (car tree)) (map_tree proc (cdr tree))))))
(provide map_tree)

(define (filter proc arr)
  (cond ((null? arr) nil)
        ((proc (car arr)) (cons (car arr) (filter proc (cdr arr))))
        (else (filter proc (cdr arr)))))
(provide filter)

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

(define (wrap_imp post_process proc)
  (lambda (args) (post_process (apply proc args))))

(define (wrap post_process proc) (arbiter (wrap_imp post_process proc)))
(provide wrap)

(define (wrapall_imp post_process procs)
    (lambda (args) (post_process (map (lambda (p) (apply p args)) procs))))

(define wrapall
  (arbiter2 (lambda (post_process procs) (arbiter (wrapall_imp post_process procs)))))
(provide wrapall)
  