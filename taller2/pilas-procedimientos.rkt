#lang eopl
(require rackunit)
;Taller 2 - Punto 3: Pilas con procedimientos

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

;------------------------------GRAMÁTICA DE PILAS--------------------------------------------

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)

;--------------------------IMPLEMENTSCIÓN DE PILAS----------------------------------------

;scheme-value? : value → boolean
;usage : (scheme-value? value) = funcion de scheme que valida su un valor ingresado es correcto
(define scheme-value? (lambda (value) #t))

;empty-stack: stack-procedure -> stack-procedure | symbol
;purpose: constructor de un stack vacío como procedimiento

(define empty-stack
  (lambda()
    (lambda(signal)
      (cond
        [(zero? signal) 'empty-stack]
        [(equal? signal 1) 'error]))))

;push: scheme-value X stack-procedure -> stack-procedure | scheme-value | symbol
;purpose: constructor de un stack como procedimiento
;              que recibe un valor y un stack e inserta el valor sobre el stack dado
(define push
  (lambda(val bottom-stack)
    (lambda(signal)
      (cond
        [(zero? signal) 'push]
        [(equal? signal 1) val]
        [(equal? signal 2) bottom-stack]))))

;push: stack-procedure -> stack-procedure | symbol
;purpose: recibe un stack y retira el elemento superior
(define pop
  (lambda(stack)
    (lambda(signal)
      (cond
        [(zero? signal) 'pop]
        [(equal? signal 1)
         (cond
           [(empty-stack? stack) (stack 0)]
           [(pop? stack) ((pop (stack 1)) 1)]
           [(push? stack) (stack 2)]
           )]
        ))))

;push: scheme-value X stack-procedure -> stack-procedure | scheme-value | symbol
;purpose: recibe un stack y devuelve el elemento superior sin retirarlo
(define top
  (lambda(stack)
    (cond
      [(empty-stack? stack) (stack 0)]
      [(push? stack) (stack 1)]
      [(pop? stack) (stack 1)])))

;--------------------------------PREDICADOS------------------------------------------------------

(define push?
  (lambda(stack)
    (if (equal? (stack 0) 'push) #t #f)))

(define pop?
  (lambda(stack)
    (if (equal? (stack 0) 'pop) #t #f)))

(define empty-stack?
  (lambda(stack)
    (if (equal? (stack 0) 'empty-stack) #t #f)))

;--------------------------------PRUEBAS------------------------------------------------------
(check-equal? ((push '1 ((empty-stack)2)) 0) 'push)
(check-equal? ((push 'a ((empty-stack)0)) 1) 'a)
(check-equal? ((push 'a ((empty-stack)0)) 2) 'empty-stack)
(check-equal? (empty-stack?(push 'a (empty-stack))) #f)
(check-equal? (push? (push 'a (empty-stack))) #t)