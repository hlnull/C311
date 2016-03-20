#lang racket

(define n* #f)
(define k* #f)
(define v* #f)
(define pc* #f)
(define done* #f)

(define apply-k
  (lambda ()
    (match k*
      [`(empty-k) (set! done* #t)]
      [`(big-k ,n ,k) (begin
                        (set! n* (- n 2))
                        (set! k* (little-k v* k))
                        (set! pc* fib))]
      [`(little-k ,w ,k) (begin
                          (set! v* (+ w v*))
                          (set! k* k)
                          (set! pc* apply-k))])))

(define empty-k
  (lambda ()
    `(empty-k)))

(define big-k
  (lambda (n k)
    `(big-k ,n ,k)))

(define little-k
  (lambda (w k)
    `(little-k ,w ,k)))

(define fib
  (lambda ()
    (cond
      [(zero? n*) (begin (set! v* 0) (set! pc* apply-k))]
      [(= n* 1) (begin (set! v* 1) (set! pc* apply-k))]
      [else (begin
              (set! k* (big-k n* k*))
              (set! n* (- n* 1))
              (set! pc* fib))])))

(define trampoline
  (lambda ()
    (if done* v* (begin (pc*) (trampoline)))))

(define fib-driver
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k))                                               
      (set! done* #f)
      (set! pc* fib)
      (trampoline))))

(fib-driver 5)