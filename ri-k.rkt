#lang racket
;1
(foo-cps 'cat
         (lambda (v)
           (foo-cps 'dog (lambda (w)
                           (k (cons v w))))))

(foo-cps 'cat
         (lambda (v)
           (foo-cps 'dog (lambda (w)
                           (app-k k (cons v w))))))

;2
(foo-cps 'cat (lambda (v)
                (foo-cps 'dog)))

; the inner-most one
(lambda (w)
  (app-k k (cons v w)))

;3
;giving it a name
(define inner-k
  (lambda ()
    (lambda (w)
      (app-k k (cons v w)))))

(define inner-k
  (lambda (v k)
    (lambda (w)
      (app-k k (cons v w)))))

; continuation constructor
(foo-cps 'cat
         (lambda (v)
           (foo-cps 'dog (inner-k v k))))

;4
(define outer-k
  (lambda ()
    (lambda (v)
      (foo-cps 'dog (inner-k v k)))))

(define outer-k
  (lambda (k)
    (lambda (v)
      (foo-cps 'dog (inner-k v k)))))

(foo-cps 'cat (outer-k k))

;5
(define app-k
  (lambda (k v)
    (k v)))

(define inner-k
  (lambda (v^ k^)
    (lambda (v)
      (app-k k^ (cons v^ v)))))

(define outer-k
  (lambda (k^)
    (lambda (v)
      (foo-cps 'dog (inner-k v k^)))))

;6
;build a tagged list
(define outer-k
  (lambda (k^)
    `(outer-k ,k^)
    (`(outer-k ,k^)
      (foo-cps 'dog (inner-k v k^)))))

;7
(define outer-k
  (lambda (k^)
    `(outer-k ,k^)))

(define app-k
  (lambda (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^))))))

;8
(define outer-k
  (lambda (k^)
    `(outer-k ,k^)))

(define inner-k
  (lambda (v^ k^)
    `(inner-k ,v^ ,k^)))

(define app-k
  (lambda (k v)
    (match k
      (`(outer-k ,k^) (foo-cps 'dog (inner-k v k^)))
      (`(inner-k ,v^ ,k^) (app-k k^ (cons v^ v)))
      (`(empty-k v)))))