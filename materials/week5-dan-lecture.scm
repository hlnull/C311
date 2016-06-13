;;; Now, we take the abstractable information from the three interpreters.
;;; and by writing just a small number of macros, we can get the behavior
;;; of call-by-ref, call-by-name, and call-by-need.  This is the first time
;;; I have ever actually derived these macros, and it all works because we
;;; left the lambda-line of the interpreters unchanged.  So, the only concerns
;;; are dealing with what should happen with the rand and what should happen
;;; with variable lookup.  Thus, by leaving apply-env unchanged, we get a clean
;;; characterization of variable lookup, which then allows for the writing of
;;; Scheme macro!!!  More to the point, we can see immediately how to derive
;;; the stream operators.  This is very different from just knowing what they
;;; are.

(define-syntax ref-rand
  (syntax-rules ()
    ((_ (x ...)) (box (x ...)))
    ((_ x) (if (box? x) x (box x)))))

(let ((x (box 5)))
  (let ((f (lambda (y) (set-box! y (+ (unbox x) (unbox y))))))
    (f (ref-rand x))
    (unbox x)))

;; 10

(define-syntax lazy-rand
  (syntax-rules ()
    ((_ (x ...)) (box (lambda () (x ...))))
    ((_ x) (if (box? x) x (box (lambda () x))))))

(let ((x (box (lambda () 5))))
  (let ((f (lambda (y) (+ ((unbox y)) ((unbox y))))))
    (f (lazy-rand x))))

(define !
  (lambda (n)
    (if (zero? n) 1 (* n (! (sub1 n))))))

(let ((f (lambda (y) (+ ((unbox y)) ((unbox y))))))
  (f (lazy-rand (! 5))))

;; 240

;; 135 (if this were call-by-need; then it would have to be even.)

(define not-always-even
  (lambda ()
    (let ((x (box (lambda () 5))))
      (let ((f (lambda (y) (+ ((unbox y)) ((unbox y))))))
        (f (lazy-rand (random 2)))))))

;; > (not-always-even)
;; 2
;; > (not-always-even)
;; 0
;; > (not-always-even)
;; 1

;; ------------------------------

(let ((x (box (lambda () 5))))
  (let ((f (lambda (y) (+ (let ((y-box y))
                            (let ((v ((unbox y))))
                              (set-box! y-box (lambda () v))
                              v))
                           (let ((y-box y))
                             (let ((v ((unbox y))))
                               (set-box! y-box (lambda () v))
                               v))))))
    (f (lazy-rand x))))

;; 240

(define always-even
  (lambda ()
    (let ((f (lambda (y) (+ (let ((y-box y))
                              (let ((v ((unbox y-box))))
                                (set-box! y-box (lambda () v))
                                v))
                            (let ((y-box y))
                              (let ((v ((unbox y-box))))
                                (set-box! y-box (lambda () v))
                                v))))))
      (f (lazy--rand (random 2))))))

;;; Now, we can define something like (lambda (y-box) ...)

(define unbox*
  (lambda (y-box)
    (let ((v ((unbox y-box))))
      (set-box! y-box (lambda () v))
      v)))

;;; Which makes the test program much simpler.
;;; Notice the similarity to not-always-even.

(define always-even
  (lambda ()
    (let ((f (lambda (y) (+ (unbox* y) (unbox* y)))))
      (f (lazy-rand (random 2))))))

;; > (always-even)
;; 0
;; > (always-even)
;; 2
;; > (always-even)
;; 0
;; > (always-even)
;; 0
;; > (always-even)
;; 2
;; > (always-even)
;; 0

(define-syntax cons$
  (syntax-rules ()
    ((_ a d) (cons (lazy-rand a) (lazy-rand d)))))

(define fa (cons$ (random 10) (cons$ (random 100) (cons$ (random 1000) '()))))
;; > (unbox* (car fa))
;; 0
;; > (unbox* (car fa))
;; 0
;; > (unbox* (car fa))
;; 0
;; > (define ga (unbox* (cdr fa)))
;; > (unbox* (car ga))
;; 89
;; > (unbox* (car ga))
;; 89
;; > (define ha (unbox* (cdr ga)))
;; > (unbox* (car ha))
;; 129
;; > (unbox* (car ha))
;; 129

(define car$
  (lambda (cons-cell)
    (unbox* (car cons-cell))))

(define cdr$
  (lambda (cons-cell)
    (unbox* (cdr cons-cell))))

(define prefix$
  (lambda ($ n)
    (cond
      ((zero? n) '())
      ((null? $) '())
      (else (cons (car$ $) (prefix$ (cdr$ $) (sub1 n)))))))

(define ref$
  (lambda ($ n)
    (cond
      ((zero? n) (car$ $))
      ((null? $) (error 'ref$ "Stream is too short"))
      (else (ref$ (cdr$ $) (sub1 n))))))

(define primes
  (lambda ($)
    (cons$ (car$ $) (primes (rem-mults (car$ $) (cdr$ $))))))

(define rem-mults
  (lambda (n $)
    (cond
      ((zero? (remainder (car$ $) n)) (rem-mults n (cdr$ $)))
      (else (cons$ (car$ $) (rem-mults n (cdr$ $)))))))

(define omega
  (lambda (n)
    (cons$ n (omega (add1 n)))))

(define 5000-answers (lambda () (prefix$ p* 5000)))

;;; Now, run (5000-answers) and keep track of the time.
;;; Next, do it again and keep track of the time.  Did you notice a difference?
;;; Can you explain why there is a difference?
