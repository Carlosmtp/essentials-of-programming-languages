#lang eopl
(require rackunit)

;Taller 2 - Punto 5 Árbol Binario de Búsqueda con listas

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699

;       Gramática Árbol Binario 

; Bintree ::= () l (Int Bintree Bintree)

;bintree : number X left X right → bintree 
;usage : (bintree number left righ) = constructor del árbol binario

(define bintree
  (lambda (number left right)
    `(,number ,left ,right)))

;----------------------------------------------------------------------------------------------------

;empty-bintree : empty-bintree → () 
;usage : (empty-bintree) = definimos el arbol binario vacío

(define (empty-bintree) '())

;pruebas :
(check-equal? (empty-bintree)
'())
;----------------------------------------------------------------------------------------------------

;current-element : bintree → raiz
;usage : (current-element bintree) = retorna el elemento raiz del árbol binario

(define current-element
  (lambda(bintree)
    (car bintree)))

;pruebas :
(check-equal? (current-element '(7 () ()))
7)
 
;----------------------------------------------------------------------------------------------------

;move-to-left-son : bintree → list
;usage : (move-to-left-son bintree) = retorna el lado izquierdo del árbol binario

(define move-to-left-son
  (lambda(bintree)
    (cadr bintree)))

;pruebas :
(check-equal? (move-to-left-son '(12 (1 () ()) (31 () ())))
'(1 () ()))
 
;----------------------------------------------------------------------------------------------------

;move-to-right-son : bintree → list
;usage : (move-to-right-son bintree) = retorna el lado derecho del árbol binario

(define move-to-right-son
  (lambda(bintree)
    (caddr bintree)))

;pruebas :
(check-equal? (move-to-right-son '(12 (1 () ()) (31 () ())))
'(31 () ()))
 
;----------------------------------------------------------------------------------------------------

;number→bintree : number → list
;usage : (number->bintree number) = retorna un árbol binario en donde la raiz en el numero recibido

(define number->bintree
  (lambda (number)
    (list number '() '())))

;pruebas :
(check-equal? (number->bintree 93)
'(93 () ()))
 
;----------------------------------------------------------------------------------------------------

;empty-bintree? : bintree → boolean
;usage : (empty-bintree bintree) = retorna si el árbol binario está vacío

(define empty-bintree?
  (lambda (bintree)
    (null? bintree)))

;pruebas :
(check-equal? (empty-bintree? (move-to-left-son '(13 () ())))
#t)
 
;----------------------------------------------------------------------------------------------------

;at-leaf? : bintree → boolean
;usage : (at-leaf bintree) = retorna si el nodo del arbol binario es una hoja

(define at-leaf?
  (lambda (bintree)
    (and (empty-bintree? (move-to-left-son bintree)) (empty-bintree? (move-to-right-son bintree)))
   ))  

;pruebas :
(check-equal? (at-leaf? '(7 () ()))
#t)
 
;----------------------------------------------------------------------------------------------------

;bintree-with-at-least-one-child? : bintree → boolean
;usage : (bintree-with-at-least-one-child? bintree) = retorna si el nodo del árbol binario tiene al menos un hijo

(define bintree-with-at-least-one-child?
  (lambda (bintree)
    (or (not (empty-bintree? (move-to-left-son bintree))) (not (empty-bintree? (move-to-right-son bintree))))
   ))

;pruebas :
(check-equal? (bintree-with-at-least-one-child? '(18 () (38 () ())))
#t)
 
;----------------------------------------------------------------------------------------------------

;insert-to-left : num bintree → list
;usage : (insert-to-left num bintree) = retorna un árbol binario que inserta al lado izquierdo el numero dado

(define insert-to-left
  (lambda (num bintree)
    (list (current-element bintree) (number->bintree num) (caddr bintree))))

;pruebas :
(check-equal? (insert-to-left 9 '(18 () ()))
'(18 (9 () ()) ()))

;----------------------------------------------------------------------------------------------------

;insert-to-right : num bintree → raiz
;usage : (insert-to-right num bintree) = retorna un árbol binario que inserta al lado derecho el numero dado

(define insert-to-right
  (lambda (num bintree)
    (list (current-element bintree) (cadr bintree) (number->bintree num))))

;pruebas :
(check-equal? (insert-to-right 27 (insert-to-left 9 '(18 () ())))
'(18 (9 () ()) (27 () ())))

;----------------------------------------------------------------------------------------------------

;bintree-order-validation : bintree → boolean
;usage : (bintree-order-validation bintree) = retorna si el árbol binario esta organizado correctamente

(define bintree-order-validation
  (lambda (bintree)
    (and (aux-validation > (current-element bintree) (move-to-left-son bintree))
         (aux-validation < (current-element bintree) (move-to-right-son bintree))
          (if (empty-bintree? (move-to-left-son bintree))
              #t
              (bintree-order-validation (move-to-left-son bintree)))
          (if (empty-bintree? (move-to-right-son bintree))
              #t
              (bintree-order-validation (move-to-right-son bintree))))
    
        ))

; aux-validation : bintree → boolean or list
;usage : (aux-validation bintree) = valida árbol binario está vacío, si no retorna la lista
(define aux-validation
  (lambda (sym current bintree)
    (if (empty-bintree? bintree)
        #t
        (sym current (current-element bintree)))))

;pruebas :
(check-equal? (bintree-order-validation '(8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
#t)

;----------------------------------------------------------------------------------------------------

;insert-element-into-bintree : arbol n → árbol binario
;usage : (insert-element-into-bintree arbol n) = retorna el árbol binario con el nuevo número añadido

(define insert-element-into-bintree
  (lambda (arbol n)
    (cond
          [(empty-bintree? arbol) (number->bintree n)]
          [(equal? (current-element arbol) n)
           (bintree (current-element arbol)  (move-to-left-son arbol) (move-to-right-son arbol))]
          [(> (current-element arbol) n)
           (list (current-element arbol)
                 (insert-element-into-bintree (move-to-left-son arbol) n)
                 (move-to-right-son arbol))]
          [(< (current-element arbol) n)
           (list (current-element arbol)
                 (move-to-left-son arbol)
                 (insert-element-into-bintree (move-to-right-son arbol) n))])
         ))

;pruebas :
(check-equal?(insert-element-into-bintree '(8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))) 2)
'(8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
