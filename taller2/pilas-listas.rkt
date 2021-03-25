#lang eopl
;Taller 2 - Punto 3: Pilas con listas

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699

;       Gramática

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)


;----------------------------------------------------------------------------------------------------

;empty-stack : () -> list(empty-stack)
;usage : (empty-stack()) definimos la lista vacía

(define empty-stack
  (lambda ()
    (list 'empty-stack)))

;----------------------------------------------------------------------------------------------------

;empty-stack? : stack -> boolean
;usage: (empty-stack? stack) retorna un booleano al comparar si el stack que recibe es vacío

(define empty-stack?
  (lambda (stack)
    (equal? (car stack) 'empty-stack)))

;pruebas
(empty-stack? '(empty-stack))

;----------------------------------------------------------------------------------------------------

;push : stack -> list
;usage : (push val stack) retorna una lista con el valor insertado al inicio del stack

(define push
  (lambda(val stack)
    (list 'push val stack)))

;prueba
(define s1
  (push 'a
        (push 'b
              (push 'c
                    (push 'd
                          (empty-stack))))))

(push "s" '(empty-stack))
(push "s" s1)

;----------------------------------------------------------------------------------------------------

;pop : stack -> list
;usage : (pop stack) retorna una lista retirando el elemento inicial del stack

(define pop
  (lambda (stack)
    (cond
      [(empty-stack? stack)'()]
      [else (caddr stack)])))

;prueba
(pop s1)

;----------------------------------------------------------------------------------------------------

;top : stack -> list
;usage : (top stack) retorna el elemento superior del stack

(define top
  (lambda (stack)
    (cond
      [(empty-stack? stack)'()]
      [else (cadr stack)])))

;prueba
(top(pop s1))
