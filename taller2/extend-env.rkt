#lang eopl
(require rackunit)
;

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;empty-env : () → Env
(define empty-env
  (lambda () (list 'empty-env)))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;extend − env∗
(define extend-env*
  (lambda (vars vals env)
    (list 'extend-env* vars vals env)))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
((eqv? (car env) 'extend-env)
(let ((saved-var (cadr env))
(saved-val (caddr env))
(saved-env (cadddr env)))
(if (eqv? search-var saved-var)
saved-val
(apply-env saved-env search-var))))
(else
(report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error apply-env "Bad environment: ~s" env)))


(define invert-env
  (lambda (env)
    (cond
      [(null? env) (cons 'empty-env env)]
      [else (cons (invert-env (cdr env)) (car env))])))

(define count-env
  (lambda (env count)
    (cond
      [(equal? (car env) 'empty-env) count]
      [else (count-env (cadddr env) (+ count 1))])))

(define cartesian-product
  (lambda (lst1 lst2)
    (cond
      [(equal? (length lst1) (length lst2))
       (cond
         [(zero? (length lst1)) empty]
         [else (cons (list (car lst1) (car lst2)) (cartesian-product (cdr lst1) (cdr lst2)))])]
      [else (eopl:error 'cartesian-product: " Lists of different lengths have been entered")])))

(define check-env
  (lambda (env n)
    (cond
      [(= n 0) '()]
      [(> n (count-env env 0)) (eopl:error 'check-env: " Not possible to search depth on environment")]
      [(= n (count-env env 0))
       (cond
         [(equal? (car env) 'extend-env) (list (list (cadr env) (caddr env)))]
         [(equal? (car env) 'extend-env*) (cartesian-product (cadr env) (caddr env))])]
      [else (check-env (cadddr env) n)]
      )))

(define e
  (extend-env 'y 8
             (extend-env* '(x z w) '(1 4 5)
                         (extend-env 'a 7
                                    (empty-env)))))

(check-equal? (check-env e 0) '())
(check-equal? (check-env e 1) '((a 7)))
(check-equal? (check-env e 2) '((x 1) (z 4) (w 5)))
(check-equal? (check-env e 3) '((y 8)))