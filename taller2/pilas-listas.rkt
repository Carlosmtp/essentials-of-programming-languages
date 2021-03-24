#lang eopl
;
;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

; <stack> ::= (empty-stack)
;              ::=(push <scheme-value> <stack>)

;empty stack
(define empty-stack
  (lambda ()
    (list 'empty-stack)))

;empty stack?
(define empty-stack?
  (lambda (stack)
    (equal? (car stack) 'empty-stack)))

;push stack
(define push
  (lambda(val stack)
    (list 'push val stack)))

;pop
(define pop
  (lambda (stack)
    (cond
      [(empty-stack? stack)'()]
      [else (caddr stack)])))

;top
(define top
  (lambda (stack)
    (cond
      [(empty-stack? stack)'()]
      [else (cadr stack)])))

(define s1
  (push 'a
        (push 'b
              (push 'c
                    (push 'd
                          (empty-stack))))))

(push "s" '(empty-stack))
(empty-stack? '(empty-stack))
(push "s" s1)
(pop f1)
(top(pop s1))