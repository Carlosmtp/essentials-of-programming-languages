#lang eopl
;Taller 2 - Punto 3: Pilas con datatypes

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699


;       Gramática

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)


;----------------------------------------------------------------------------------------------------
;scheme-value? : value → boolean
;usage : (scheme-value? value) = funcion de scheme que valida su un valor ingresado es correcto

(define scheme-value? (lambda (value) #t))

;stack : ()|(push (val X scheme-value?)) → stack 
;usage : (stack) = constructor del stack con datatypes

(define-datatype stack stack?
  (empty-stack)
  (push (val scheme-value?) (bottom-stack stack?)))

;pruebas
(define s1
  (push 'a
        (push 'b
              (push 'c
                    (push 'd
                          (empty-stack))))))


(push "s" (empty-stack))
(push "s" s1)
;----------------------------------------------------------------------------------------------------

;empty-stack? : stack -> boolean
;usage: (empty-stack? stack) retorna un booleano al comparar si el stack que recibe es vacío

(define empty-stack?
  (lambda(the-stack)
    (cases stack the-stack
      (empty-stack #t))))

;pruebas
(empty-stack? (empty-stack))

;----------------------------------------------------------------------------------------------------

;pop : the-stack -> list
;usage : (pop stack) retorna una lista retirando el elemento inicial del stack

(define pop
  (lambda(the-stack)
    (cases stack the-stack
      (empty-stack () '())
      (push (val bottom-stack)
            bottom-stack))))
;pruebas
(pop s1)

;----------------------------------------------------------------------------------------------------
;top : the-stack -> list
;usage : (top stack) retorna el elemento superior del stack

(define top
  (lambda(the-stack)
    (cases stack the-stack
      (empty-stack () '())
      (push (val bottom-stack)
            val))))
;pruebas
(top(pop s1))



