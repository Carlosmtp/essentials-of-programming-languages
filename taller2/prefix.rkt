#lang eopl
;Taller 2 - Punto 6: Notación prefijo polaca

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699

;-----------------------------GRAMÁTICA DE PREFIX-------------------------------------------

;Prefix-list ::= (Prefix-exp)
;              pref-exp (pref)
;Prefix-exp ::= Int
;              const-exp (num)
;              ::= - Prefix-exp Prefix-exp
;              diff-exp (operand1 operand2)

;-------------------------IMPLEMENTACIÓN DE PREFIX---------------------------------------

;prefix-exp : int l (diff-exp op1 op2) -> sintaxis abstracta
;usage : (prefix-exp) = funcion constructora de 
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;----------------------------------------------------------------------------------------------------

;prefix-list : (prefix-exp) → prefix-list
;usage : (stack) = constructor del refix-list con datatypes

(define-datatype prefix-list prefix-list?
  (pref-exp
   (pref prefix-exp?)))

;----------------------------------------------------------------------------------------------------
;parse-prefix-exp : sintaxis concreta → sintaxis abstracta
;usage : (parse-prefix-exp exp) = retorna prefix-exp en sintaxis abstracta

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

;parse-prefix-list : sintaxis concreta → sintaxis abstracta
;usage : (parse-prefix-list exp) = retorna prefix-list en sintaxis abstracta

(define parse-prefix-list
  (lambda(exp)
    (cond
      [(equal? (length exp) 1) (pref-exp (car exp))]
      [else (parse-prefix-list (parse-prefix-exp (aux exp)))])))

;aux : list → list
;usage : (aux list) = retorna una lista convirtiendo los numeros en sintaxis abstractas

(define aux
  (lambda(list)
    (cond
      [(null? list) '()]
      [(number? (car list)) (cons (const-exp (car list)) (aux (cdr list)))]
      [else (cons (car list) (aux (cdr list)))])))

;pruebas
(parse-prefix-list '(- - 3 2 - 4 - 12 7))
(aux '(- - 3 2 - 4 - 12 7))

;----------------------------------------------------------------------------------------------------

;unparse-pref-list : sintaxis abstracta → sintaxis concreta
;usage : (unparse-prefix-list list) = retorna prefix-list en sintaxis concreta

(define unparse-prefix-list
  (lambda(list)
    (cases prefix-list list
      (pref-exp (pref) (append (unparse-prefix-exp pref)))
      (else 0))))

;unparse-pref-exp : exp → sintaxis concreta
;usage : (unparse-prefix-exp exp) = retorna prefix-exp en sintaxis concreta

(define unparse-prefix-exp
  (lambda(exp)
    (cases prefix-exp exp
      (const-exp (num) (list num))
      (diff-exp (operand1 operand2)
               (append (list '-) (append (unparse-prefix-exp operand1) (unparse-prefix-exp operand2)))))))

;prueba
(unparse-prefix-list (parse-prefix-list '(- - 3 2 - 4 - 12 7)))