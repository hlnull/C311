#lang racket

(define empty-k-cps
  (lambda ()
    (lambda (v) v)))

(define fib-cps
  (lambda (n k)
    (cond
      [(zero? n) (k 0)]
      [(= n 1) (k 1)]
      [else (fib-cps (- n 1) (lambda (x)
                           (fib-cps (- n 2) (lambda (x^)
                                          (k (+ x x^))))))])))

(define fib
  (lambda (n k)
    (cond
      [(zero? n) (apply-k k 0)]
      [(= n 1) (apply-k k 1)]
      [else (fib (- n 1) (big-k n k))])))

(define empty-k
  (lambda ()
    `(empty-k)))

(define big-k
  (lambda (n k)
    `(big-k ,n ,k)))

(define little-k
  (lambda (w k)
    `(little-k ,w ,k)))

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(big-k ,n ,k) (fib (- n 2) (little-k v k))]
      [`(little-k ,w ,k) (apply-k k (+ w v))])))

(fib 5 (empty-k))
(fib-cps 5 (empty-k-cps))