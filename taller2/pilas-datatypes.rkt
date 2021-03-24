#lang eopl
;

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)

(define scheme-value? (lambda (value) #t))

(define-datatype stack stack?
  (empty-stack)
  (push (val scheme-value?) (bottom-stack stack?)))

(define pop
  (lambda(the-stack)
    (cases stack the-stack
      (empty-stack () '())
      (push (val bottom-stack)
            bottom-stack))))

(define top
  (lambda(the-stack)
    (cases stack the-stack
      (empty-stack () '())
      (push (val bottom-stack)
            val))))

(define parse-stack
  (lambda (the-stack)
    (cond
      [(equal? (top) 'empty-stack) (empty-stack)]
      [(equal? (car the-stack) 'push) (push (top) (parse-stack (pop the-stack)))]
      [else (eopl:error 'stack "La pila ingresada no es válida")])))
