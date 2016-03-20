#lang racket
(require test-engine/racket-tests)

(define empty-k
  (lambda ()
    `(empty-k)))

; ack
(define ack-m* #f)
(define ack-n* #f)
(define ack-k* #f)
(define ack-v* #f)
(define ack-pc* #f)
(define ack-done* #f)

(define ack-apply-k
  (lambda ()
    (match ack-k*
      [`(empty-k) (set! ack-done* #t)]
      [`(ack-inner-k ,m ,k^) (begin
                               (set! ack-m* (sub1 m))
                               (set! ack-n* ack-v*)
                               (set! ack-k* k^)
                               (set! ack-pc* (ack)))])))

(define ack
  (lambda ()
    (cond
      [(zero? ack-m*) (begin
                        (set! ack-k* ack-k*)
                        (set! ack-v* (add1 ack-n*))
                        (set! ack-pc* (ack-apply-k)))]
      [(zero? ack-n*) (begin
                        (set! ack-k* ack-k*)
                        (set! ack-m* (sub1 ack-m*))
                        (set! ack-n* 1)
                        (set! ack-pc* (ack)))]
      [else (begin
              (set! ack-k* (ack-inner-k ack-m* ack-k*))
              (set! ack-m* ack-m*)
              (set! ack-n* (sub1 ack-n*))
              (set! ack-pc* (ack)))])))

(define ack-inner-k
  (lambda (m k^)
    `(ack-inner-k ,m ,k^)))

(define ack-trampoline
  (lambda ()
    (if ack-done* ack-v* (begin (ack-pc*) (ack-trampoline)))))

(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! ack-m* m)
      (set! ack-n* n)
      (set! ack-k* (empty-k))                                               
      (set! ack-done* #f)
      (set! ack-pc* ack)
      (ack-trampoline))))

;depth
(define depth-ls* #f)
(define depth-k* #f)
(define depth-l* #f)
(define depth-v* #f)
(define depth-pc* #f)
(define depth-done* #f)

(define depth
  (lambda ()
    (cond
      [(null? depth-ls*) (begin
                           (set! depth-v* 1)
                           (set! depth-k* depth-k*)
                           (set! depth-pc* depth-apply-k))]
      [(pair? (car depth-ls*)) (begin
                                 (set! depth-ls* (car depth-ls*))
                                 (set! depth-k* (depth-outer-k depth-ls* depth-k*))
                                 (set! depth-pc* depth))]
      [else (begin
              (set! depth-ls* (cdr depth-ls*))
              (set! depth-k* depth-k*)
              (set! depth-pc* depth))])))

(define depth-apply-k
  (lambda ()
    (match depth-k*
      [`(empty-k) (set! depth-done* #t)]
      [`(depth-outer-k ,ls ,k) (begin
                                  (set! depth-ls* (cdr ls))
                                  (set! depth-k* (depth-inner-k depth-v* k))
                                  (set! depth-pc* depth))]
      [`(depth-inner-k ,l ,k) (let ((l (add1 l)))
                                (if (< l depth-v*)
                                    (begin
                                      (set! depth-k* k)
                                      (set! depth-v* depth-v*)
                                      (set! depth-pc* depth-apply-k))
                                    (begin
                                      (set! depth-k* k)
                                      (set! depth-v* l)
                                      (set! depth-pc* depth-apply-k))))])))

(define depth-outer-k
  (lambda (ls k)
    `(depth-outer-k ,ls ,k)))

(define depth-inner-k
  (lambda (l k)
    `(depth-inner-k ,l ,k)))

(define depth-trampoline
  (lambda ()
    (if depth-done* depth-v* (begin (depth-pc*) (depth-trampoline)))))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! depth-ls* ls)
      (set! depth-k* (empty-k))                                               
      (set! depth-done* #f)
      (set! depth-pc* depth)
      (depth-trampoline))))

;fact
(define fact-n* #f)
(define fact-k* #f)
(define fact-v* #f)

#|
(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (fact-apply-k k 1)]
         [else (fact fact (sub1 n) (fact-inner-k n k))])) k)))
|#

(define fact
  (lambda ()
    ((lambda (fact)
       (fact fact))
     (lambda (fact)
       (cond
         [(zero? fact-n*) (begin
                            (set! fact-v* 1)
                            (set! fact-k* fact-k*)
                            (fact-apply-k))]
         [else (begin
                 (set! fact-k* (fact-inner-k fact-n* fact-k*))
                 (set! fact-n* (sub1 fact-n*))
                 (fact fact))])))))

(define fact-apply-k
  (lambda ()
    (match fact-k*
      [`(empty-k) fact-v*]
      [`(fact-inner-k ,n ,k) (begin
                               (set! fact-v* (* n fact-v*))
                               (set! fact-k* k)
                               (fact-apply-k))])))

(define fact-inner-k
  (lambda (n k)
    `(fact-inner-k ,n ,k)))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! fact-n* n)
      (set! fact-k* (empty-k))
      (fact))))

;pascal
(define pascal-n* #f)
(define pascal-v* #f)
(define pascal-k* #f)
(define pascal-m* #f)
(define pascal-a* #f)

(define pascal
  (lambda ()
    (let ((pascal
           (lambda (pascal)
             (begin
               (set! pascal-k* pascal-k*)
               (set! pascal-v* (lambda (m a)
                                 (cond
                                   [(> m pascal-n*) (begin (set! pascal-k* pascal-k*) (set! pascal-v* '()) (pascal-apply-k))]
                                   [else (let ((a (+ a m)))
                                           (begin (set! pascal-k* (pascal-outer-k m a pascal-k*)) (pascal pascal)))])))
               (pascal-apply-k)))))
      (begin (set! pascal-k* (pascal-inner-k-f pascal-k*)) (pascal pascal)))))

(define pascal-apply-k
  (lambda ()
    (match pascal-k*
      [`(empty-k) pascal-v*]
      [`(pascal-inner-k ,a^ ,k^) (begin (set! pascal-v* (cons a^ pascal-v*)) (set! pascal-k* k^) (pascal-apply-k))]
      [`(pascal-outer-k ,m^ ,a^ ,k^) (begin (set! pascal-k* (pascal-inner-k a^ k^)) (pascal-v* (add1 m^) a^))]
      [`(pascal-inner-k-f ,k^) (begin (set! pascal-k* k^) (pascal-v* 1 0))])))

(define pascal-inner-k
  (lambda (a^ k^)
    `(pascal-inner-k ,a^ ,k^)))

(define pascal-outer-k
  (lambda (m^ a^ k^)
    `(pascal-outer-k ,m^ ,a^ ,k^)))

(define pascal-inner-k-f
  (lambda (k^)
    `(pascal-inner-k-f ,k^)))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n* n)
      (set! pascal-k* (empty-k))
      (pascal))))

(check-expect (fact-reg-driver 5) 120)
(check-expect (ack-reg-driver 2 2) 7)
(check-expect (depth-reg-driver '(1 (2 (3 (4))))) 4)
(check-expect (pascal-reg-driver 10) '(1 3 6 10 15 21 28 36 45 55))
(test)