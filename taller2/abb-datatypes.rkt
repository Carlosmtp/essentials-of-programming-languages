#lang eopl

; Taller 2 - Punto 5 Árbol Binario de Búsqueda con Datatypes

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699

;       Gramática Árbol Binario 

;Bintree ::= () l (Int Bintree Bintree)

;---------------------------------------Datatype---------------------------------

;bintree : ()|(node (number X left X right)) → bintree 
;usage : (bintree) = constructor del árbol binario con datatypes

(define-datatype bintree bintree?
  (empty-bintree)
  (node (num number?)
        (left bintree?)
        (right bintree?)))

;---------------------------------------Unparse---------------------------------

;unparse-bintree : sintaxis abstracta → sintaxis concreta
;usage : (parse-bintree arbol) = retorna arbol binario en sintaxis concreta

(define unparse-bintree
  (lambda (arbolB)
    (cases bintree arbolB
      (empty-bintree ()
                     '(empty-bintree))
      (node (num left right)
            (list 'num num
                  'left (unparse-bintree left)
                  'right (unparse-bintree right)))
      )))

;prueba :
(unparse-bintree (node 7 (empty-bintree) (empty-bintree)))

;---------------------------------------Parse-----------------------------------

;parse-bintree : sintaxis concreta → sintaxis abstracta
;usage : (parse-bintree arbol) = retorna arbol binario en sintaxis abstracta

(define parse-bintree
  (lambda (arbolB)
    (cond
      [(eq? (car arbolB) 'empty-bintree) (empty-bintree)]
      [(and (eq? (car arbolB) 'num)
            (eq? (caddr arbolB) 'left)
            (eq? (caddr (cddr arbolB)) 'right))
       (node
        (cadr arbolB)
        (parse-bintree (caddr (cdr arbolB)))
        (parse-bintree (caddr (cdddr arbolB))))]      
      )))

;prueba :
(parse-bintree(unparse-bintree (node 7 (empty-bintree) (empty-bintree))))

;---------------------------------------Funciones--------------------------------

;current-element : bintree → raiz
;usage : (current-element arbol) = retorna el elemento raiz del árbol binario

(define current-element
  (lambda(arbol)
    (cases bintree arbol
      (empty-bintree () '())
      (node (num left right)
            num))))

;pruebas :

(define arbol (node 7 (empty-bintree) (empty-bintree)))
(current-element arbol)

;----------------------------------------------------------------------------------------------------

;move-to-left-son : bintree → list
;usage : (move-to-left-son arbol) = retorna el lado izquierdo del árbol binario en sintaxis abstracta

(define move-to-left-son
  (lambda(arbol)
    (cases bintree arbol
      (empty-bintree () '())
      (node (num left right)
            left))))

;Pruebas :
                
(define arbol1 (node 12 (node 1 (empty-bintree) (empty-bintree)) (node 31 (empty-bintree) (empty-bintree))))
(move-to-left-son arbol1) 

;----------------------------------------------------------------------------------------------------

;move-to-right-son : bintree → list
;usage : (move-to-right-son arbol) = retorna el lado derecho del árbol binario en sintaxis abstracta

(define move-to-right-son
  (lambda(arbol)
    (cases bintree arbol
      (empty-bintree () '())
      (node (num left right)
            right))))
;Pruebas :
                
(define arbol2 (node 12 (node 1 (empty-bintree) (empty-bintree)) (node 31 (empty-bintree) (empty-bintree))))
(move-to-right-son arbol2)

;----------------------------------------------------------------------------------------------------

;number→bintree : number → list
;usage : (number->bintree number) = retorna un árbol binario abstracto en donde la raiz en el numero recibido

(define number->bintree
  (lambda(number)
    (node number (empty-bintree) (empty-bintree))))

;pruebas :
(number->bintree 93)

;----------------------------------------------------------------------------------------------------

;empty-bintree? : bintree → boolean
;usage : (empty-bintree arbol) = retorna si el árbol binario está vacío

(define empty-bintree?
  (lambda (arbol)
    (cases bintree arbol
      (empty-bintree () #t)
      (node (num left right) #f))))

;pruebas :
(empty-bintree? (move-to-left-son (node 13 (empty-bintree) (empty-bintree))))

;----------------------------------------------------------------------------------------------------

;at-leaf? : bintree → boolean
;usage : (at-leaf arbol) = retorna si el nodo del arbol binario es una hoja

(define at-leaf?
  (lambda(arbol)
    (cases bintree arbol
      (empty-bintree () #f)
      (node (num left right)
            (and (empty-bintree? left)
                 (empty-bintree? right))))))
;pruebas :
(at-leaf? (node 7 (empty-bintree) (empty-bintree)))

;----------------------------------------------------------------------------------------------------

;bintree-with-at-least-one-child? : bintree → boolean
;usage : (bintree-with-at-least-one-child? arbol) = retorna si el nodo del árbol binario tiene al menos un hijo

(define bintree-with-at-least-one-child?
  (lambda(arbol)
    (cases bintree arbol
      (empty-bintree () #f)
      (node (num left right)
            (or (empty-bintree? left)
                 (empty-bintree? right))))))

;pruebas :
(bintree-with-at-least-one-child? (node 18 (empty-bintree) (node 38 (empty-bintree) (empty-bintree))))

;----------------------------------------------------------------------------------------------------

;insert-to-left : num bintree → list
;usage : (insert-to-left num arbol) = retorna un árbol binario abstracto que inserta al lado izquierdo el numero dado

(define insert-to-left
  (lambda (number arbol)
    (cases bintree arbol
      (empty-bintree () '())
      (node (num left right)
            (node (current-element arbol) (number->bintree number) (move-to-right-son arbol))))))
;pruebas :
(insert-to-left 9 (node 18 (empty-bintree) (empty-bintree)))

;----------------------------------------------------------------------------------------------------

;insert-to-right : num bintree → raiz
;usage : (insert-to-right num arbol) = retorna un árbol binario abstracto que inserta al lado derecho el numero dado

(define insert-to-right
  (lambda (number arbol)
    (cases bintree arbol
      (empty-bintree () '())
      (node (num left right)
            (node (current-element arbol) (move-to-left-son arbol) (number->bintree number))))))

;pruebas :
(insert-to-right 27 (insert-to-left 9 (node 18 (empty-bintree) (empty-bintree))))
;----------------------------------------------------------------------------------------------------

;bintree-order-validation : bintree → boolean
;usage : (bintree-order-validation arbol) = retorna si el árbol binario esta organizado correctamente

(define bintree-order-validation
  (lambda (arbol)
    (cases bintree arbol
      (empty-bintree () #t)
      (node (num left right)
            (and (aux-validation > (current-element arbol) (move-to-left-son arbol))
                 (aux-validation < (current-element arbol) (move-to-right-son arbol))
                 (if (empty-bintree? (move-to-left-son arbol))
                    #t
                    (bintree-order-validation (move-to-left-son arbol)))
                 (if(empty-bintree? (move-to-right-son arbol))
                    #t
                    (bintree-order-validation (move-to-right-son arbol))))))))

;aux-validation : pred X current X bintree → boolean
;usage : (aux-validation arbol) = mira si el arbol esta vacio (ya está ordenado) o si no, retorna los valores para comparar en la principal

(define aux-validation
  (lambda (pred current arbol)
    (cases bintree arbol
      (empty-bintree () #t)
      (node (num left right)
            (if (empty-bintree? arbol)
                #t
                (pred current (current-element arbol)))))))

;pruebas :
(define arbol3
  (node 8
        (node 3
              (node 1 (empty-bintree)
                    (node 2 (empty-bintree) (empty-bintree)))
              (node 6
                    (node 4 (empty-bintree) (empty-bintree))
                    (node 7 (empty-bintree) (empty-bintree))))
        (node 10
              (empty-bintree)
              (node 14
                    (node 13 (empty-bintree) (empty-bintree))
                    (empty-bintree)))))
(bintree-order-validation arbol3)

;----------------------------------------------------------------------------------------------------

;insert-element-into-bintree : arbol n → árbol binario
;usage : (insert-element-into-bintree arbol n) = retorna el árbol binario con el nuevo número añadido

(define insert-element-into-bintree
  (lambda (arbol n)
    (cases bintree arbol
      (empty-bintree () #t)
      (node (num left right)
            (cond
              [(empty-bintree? arbol) (number->bintree n)]
              [(equal? (current-element arbol) n)
               (node (current-element arbol)  (move-to-left-son arbol) (move-to-right-son arbol))]
              [(> (current-element arbol) n)
               (list (current-element arbol)
                     (insert-element-into-bintree (move-to-left-son arbol) n)
                     (move-to-right-son arbol))]
              [(< (current-element arbol) n)
               (list (current-element arbol)
                     (move-to-left-son arbol)
                     (insert-element-into-bintree (move-to-right-son arbol) n))])
            ))))

;pruebas :
(insert-element-into-bintree arbol3 2)