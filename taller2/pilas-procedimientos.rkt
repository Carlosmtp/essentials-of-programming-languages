#lang eopl
;

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)

(define scheme-value? (lambda (value) #t))

(define empty-stack
  (lambda()
    (lambda(signal)
      (cond
        [(zero? signal) 'empty-stack]
        [(equal? signal 1) 'error]))))

(define push
  (lambda(val bottom-stack)
    (lambda(signal)
      (cond
        [(zero? signal) 'push]
        [(equal? signal 1) val]
        [(equal? signal 2) bottom-stack]))))

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

(define top
  (lambda(stack)
    (cond
      [(empty-stack? stack) (stack 0)]
      [(push? stack) (stack 1)]
      [(pop? stack) (stack 1)])))

(define push?
  (lambda(stack)
    (if (equal? (stack 0) 'push) #t #f)))

(define pop?
  (lambda(stack)
    (if (equal? (stack 0) 'pop) #t #f)))

(define empty-stack?
  (lambda(stack)
    (if (equal? (stack 0) 'empty-stack) #t #f)))