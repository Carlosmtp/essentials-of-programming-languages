#lang eopl
(require rackunit)

;Taller 2 - Punto 2 Interfaz Diff-tree

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616


;          Gramática BNF recursiva

;Diff-tree ::= (one) l (diff Diff-tree Diff-tree)

;----------------------------------------------------------------------------------------------------

;diff : List(Diff-tree) → List(Diff-tree)
;usage : (diff Diff-tree Diff-tree) constructor que retorna una lista Diff-tree

(define diff
  (lambda (left right)
    `(diff ,left ,right)))

;----------------------------------------------------------------------------------------------------

;one : () → '(one)
;usage: definimos la variable one con una lista '(one)

(define one
  (lambda ()
    '(one)))
;pruebas
(check-equal? (one) '(one))
;----------------------------------------------------------------------------------------------------

;left : List(diff-tree) → List
;usage: retorna el lado izquierdo de la lista diff-tree

(define left
  (lambda (diff-tree)
    (if (eqv? (car diff-tree) 'one)
      (one)
      (cadr diff-tree))))

;Pruebas
(check-equal? (left (diff (diff (one) (one)) (one))) '(diff (one) (one)))

;----------------------------------------------------------------------------------------------------

;right : List(diff-tree) → List
;usage: retorna el lado derecho de la lista diff-tree

(define right
  (lambda (diff-tree)
    (if (eqv? (car diff-tree) 'one)
      (diff (one) (one))
      (caddr diff-tree))))

;pruebas
(check-equal? (right (diff (diff (one) (one)) (one))) '(one))

;----------------------------------------------------------------------------------------------------

;zero : () → '(diff (one) (one))
;usage: definimos la variable zero con una lista '(diff (one) (one))

(define zero
  (lambda ()
    (diff (one) (one))))

;pruebas
(check-equal? (zero) '(diff (one) (one)))

;----------------------------------------------------------------------------------------------------

;is-zero? : List(diff-tree) → boolean
;usage: retorna verdadero si la lista diff-tree es un cero, de lo contrario retorna falso

(define (is-zero? diff-tree)
  (define (number diff-tree)
    (if (eq? (car diff-tree) 'one) 1
        (- (number (left diff-tree))
           (number (right diff-tree)))))
  (zero? (number diff-tree)))

;pruebas
(check-equal? (is-zero? (diff (diff (one) (one)) (one))) #f)
(check-equal? (is-zero? (diff (one) (one))) #t)

;----------------------------------------------------------------------------------------------------

;successor : List(diff-tree) → List(diff-tree)
;usage: retorna el valor siguiente a la entrada en una lista diff-tree

(define successor
  (lambda (diff-tree)
    (diff (left diff-tree)
          (diff (right diff-tree) (one)))))

;prueba
(check-equal? (successor (diff (diff (one) (one)) (one)))
              '(diff (diff (one) (one)) (diff (one) (one))))
(check-equal? (successor (diff (one) (one)))
              '(diff (one) (diff (one) (one))))
(check-equal? (successor (diff (one) (diff (one) (one))))
              '(diff (one) (diff (diff (one) (one)) (one))))
;----------------------------------------------------------------------------------------------------

;predecessor : List(diff-tree) → List(diff-tree)
;usage: retorna el valor anterior a la entrada en una lista diff-tree

(define predecessor
  (lambda (diff-tree)
    (diff diff-tree (one))))

;pruebas
(check-equal? (predecessor (diff (one) (diff (one) (one))))
              '(diff (diff (one) (diff (one) (one))) (one)))
(check-equal? (predecessor (diff (one) (one)))
              '(diff (diff (one) (one)) (one)))
(check-equal? (predecessor (diff (diff (one) (one)) (one)))
              '(diff (diff (diff (one) (one)) (one)) (one)))

;----------------------------------------------------------------------------------------------------

;diff-tree-plus : List(diff-left diff-right) → List(diff-tree)
;usage: retorna la suma de los valores de la lista diff-tree

(define diff-tree-plus
  (lambda (diff-left diff-right)
    (diff diff-left
          (diff (right diff-right)
                (left diff-left)))))

;pruebas
(check-equal? (diff-tree-plus (diff (diff (one) (one)) (one)) (diff (diff (one) (one)) (one)))
              '(diff (diff (diff (one) (one)) (one)) (diff (one) (diff (one) (one)))))
(check-equal? (diff-tree-plus (diff (one) (one)) (diff (one) (one)))
              '(diff (diff (one) (one)) (diff (one) (one))))
(check-equal? (diff-tree-plus (diff (one) (diff (one) (one))) (diff (one) (diff (one) (one))))
              '(diff (diff (one) (diff (one) (one))) (diff (diff (one) (one)) (one))))