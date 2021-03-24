#lang eopl
;

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define-datatype prefix-list prefix-list?
  (pref-exp
   (pref prefix-exp?)))

(define parse-prefix-exp
  (lambda(exp)
    (cond
      [(< (length exp) 2) exp]
      [(and (prefix-exp? (cadr exp)) (prefix-exp? (caddr exp)))
       (cons (diff-exp (cadr exp) (caddr exp)) (parse-prefix-exp (cdddr exp)))]
      [(number? exp) (const-exp exp)]
      [(number? (car exp)) (const-exp (car exp))]
      [else (cons (car exp) (parse-prefix-exp (cdr exp)))]
      )))

(define parse-prefix-list
  (lambda(exp)
    (cond
      [(equal? (length exp) 1) (pref-exp (car exp))]
      [else (parse-prefix-list (parse-prefix-exp (aux exp)))])))

(define aux
  (lambda(list)
    (cond
      [(null? list) '()]
      [(number? (car list)) (cons (const-exp (car list)) (aux (cdr list)))]
      [else (cons (car list) (aux (cdr list)))])))

(parse-prefix-exp '(- - 3 2 - 4 - 12 7))

;(parse-prefix-exp '(- - 3 2 - 4 - 12 7))

;unparse-pref-list
(define unparse-prefix-list
  (lambda(list)
    (cases prefix-list list
      (pref-exp (pref) (append (unparse-prefix-exp pref)))
      (else 0))))

;unparse-pref-exp
(define unparse-prefix-exp
  (lambda(exp)
    (cases prefix-exp exp
      (const-exp (num) (list num))
      (diff-exp (operand1 operand2)
               (append (list '-) (append (unparse-prefix-exp operand1) (unparse-prefix-exp operand2)))))))

(define exp1 (diff-exp
(diff-exp
(const-exp 3)
(const-exp 2))
(diff-exp
(const-exp 4)
(diff-exp
(const-exp 12)
(const-exp 7)))))