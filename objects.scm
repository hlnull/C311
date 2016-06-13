;;; c = (c-name m* f-name*) m = (m-name closure f-name*) o = (c-name . f*)
(load "pmatch.scm")
(define c-name->f-names (lambda (c-name) (list-ref (assq c-name c*) 1)))
(define c-name->m* (lambda (c-name) (list-ref (assq c-name c*) 2)))
(define o->c-name car) (define o->f* cdr)

(define value-of-program
  (lambda (c-decls e)
    (let ([env (base-env)])
      (for-each (lambda (c-decl) (set! c* (cons (elab-c-decl c-decl env) c*)))
        c-decls)
      (value-of e env))))

(define elab-c-decl
  (lambda (c-decl env)
    (pmatch c-decl
      [(,c-name ,s-name ,f-names ,m-exprs)
       (let ([f-names
              (append (map (lambda (sf-name)
                             (if (memv sf-name f-names) #f sf-name))
                           (c-name->f-names s-name))
                f-names)])
         (let ([m* (map
                     (let ([env (extend-env 'super-name s-name env)])
                       (lambda (m-expr)
                         (pmatch m-expr
                           [(,m-name ,id ,body)
                            `(,m-name ,(closure id body env) ,f-names)])))
                     m-exprs)])
           `(,c-name ,f-names ,(merge-m* (c-name->m* s-name) m*))))]
      [else (error 'ecd "Unmatched expression ~s" c-decl)])))

(define merge-m*
  (lambda (s* m*)
    (cond
      [(null? s*) m*]
      [(assv (caar s*) m*)
       => (lambda (p) (cons p (merge-m* (cdr s*) (remq p m*))))]
      [else (cons (car s*) (merge-m* (cdr s*) m*))])))

(define value-of
  (lambda (e env)
    (pmatch e
      [(super ,m-name ,rand)
       (let ([o (unbox (apply-env env 'self))]
             [c-name (apply-env env 'super-name)])
         (find-m-&-apply m-name c-name o (value-of rand env)))]
      [(send ,o-expr ,m-name ,rand)
       (let ([o (value-of o-expr env)])
         (find-m-&-apply m-name (o->c-name o) o (value-of rand env)))]
      [(new ,c-name ,rand)
       (let ([o `(,c-name . ,(map (lambda (x) (box (void)))
                                  (c-name->f-names c-name)))])
         (find-m-&-apply 'initialize c-name o (value-of rand env))
         o)]
      [else (value-of-expr e env)])))

(define find-m-&-apply
  (lambda (m-name c-name o arg)
    (pmatch (assq m-name (c-name->m* c-name))
      [(,m-name (closure ,id ,body ,env) ,f-names)
       (value-of body (extend-env id (box arg)
                        (extend-env 'self (box o)
                          (extend-env* f-names (o->f* o) env))))]
      [else (error 'find-m-&-apply "No method for name ~s" m-name)])))
