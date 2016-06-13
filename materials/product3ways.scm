(define product
  (lambda (v)
    (letrec ((loop (lambda (v init-acc)
                     (cond
                       ((null? v) init-acc)
                       (else (loop (cdr v) (* (car v) init-acc)))))))
      (loop v 1))))

;; > (product '(1 2 3 4 0 5))
;; 0
;; > (product '(1 2 3 4 5))
;; 120
;; How many multiplications are wasted when we return 0?  Answer: 6
;; How can we prove this: trace asterisk.

(define product
  (lambda (v)
    (letrec ((loop (lambda (v init-acc)
                     (cond
                       ((null? v) init-acc)
                       (else (loop (cdr v) (asterisk (car v) init-acc)))))))
      (loop v 1))))

(define asterisk
  (trace-lambda * (n m)
    (* n m)))

;; But what if we make an explicit test for 0.

(define product
  (lambda (v)
    (letrec ((loop (lambda (v init-acc)
                     (cond
                       ((null? v) init-acc)
                       ((zero? (car v)) 0)
                       (else (loop (cdr v) (asterisk (car v) init-acc)))))))
      (loop v 1))))

;; Notice the order of the *'s (small to large)

;; |(* 1 1)
;; |1
;; |(* 2 1)
;; |2
;; |(* 3 2)
;; |6
;; |(* 4 6)
;; |24
;; 0

;; > (product '(1 2 3 4 0 5))
;; 0
;; > (product '(1 2 3 4 5))
;; 120

;; How many multiplications are wasted? Answer: 4.

;; Let's revert to the direct style.  There are 6 wasted *'s.

(define product
  (lambda (v)
    (cond
      ((null? v) 1)
      (else (asterisk (car v) (product (cdr v)))))))

;; We can do better if we notice when we have a 0 (an annihilator).

(define product
  (lambda (v)
    (cond
      ((null? v) 1)
      ((zero? (car v)) 0)
      (else (asterisk (car v) (product (cdr v)))))))

;; Yes, now we only have 4, but notice the order of the *'s. (large to small)

;; |(* 4 0)
;; |0
;; |(* 3 0)
;; |0
;; |(* 2 0)
;; |0
;; |(* 1 0)
;; |0
;; 0

;; Let's write it in cps style.

(define product
  (lambda (v k-init)
    (letrec ((loop (lambda (v growing-k)
                       (cond
                         ((null? v) (growing-k 1))
                         ((zero? (car v)) (k-init 0))
                         (else (loop (cdr v)
                                 (lambda (p)
                                   (growing-k (asterisk (car v) p)))))))))
      (loop v k-init))))

;; > (product '(1 2 3 4 0 5) (lambda (x) x))
;; 0
;; > (product '(1 2 3 4 5) (lambda (x) x))
;; 120

;; What! There are no invoations of *!

;; So, we can get k-init by asking Scheme for it.

(define product
  (lambda (v)
    (call/cc
      (lambda (k-init)
        (letrec ((loop (lambda (v growing-k)
                       (cond
                         ((null? v) (growing-k 1))
                         ((zero? (car v)) (k-init 0))
                         (else (loop (cdr v)
                                 (lambda (p)
                                   (growing-k (asterisk (car v) p)))))))))
          (loop v k-init))))))

;;> (product '(1 2 3 4 5))
;;120
;;> (product '(1 2 3 4 0 5))
;;0

;; And, once again there are no calls to *!

;; Do we really need growing-k.  If we go back to the direct style,
;; we can get rid of the growing-k.

(define product
  (lambda (v)
    (call/cc
      (lambda (k-init)
        (letrec ((loop (lambda (v)
                       (cond
                         ((null? v) 1)
                         ((zero? (car v)) (k-init 0))
                         (else (asterisk (car v) (loop (cdr v))))))))
          (loop v))))))

;; > (product '(1 2 3 4 5))
;; 120
;; > (product '(1 2 3 4 0 5))
;; 0

;; And, still there are no *'s.

;; So, in Scheme, whenever you want a continuation, you can get it
;; using call/cc.  Furthermore, you don't have to cps your code, since
;; you can write it directly and use call/cc.