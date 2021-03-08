#lang eopl
(require rackunit)

;Taller 1 - Fundamentos de Lenguajes de Programación

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

;1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sublistas : List → int 
;usage : (sublistas lst) = cantidad de sublistas que posee lst
(define sublistas
  (lambda (lst)
   (if (null? lst)
      0
      (if (list? (car lst))
         (+ 1 (sublistas (car lst)) (sublistas (cdr lst)))
         (sublistas (cdr lst))))))
;pruebas :
(check-equal? (sublistas '(4 5 8 (1 2) 8 (7 8 9 (2 4)))) 3)
(check-equal? (sublistas '(((a b) c) (1 (2 (3 4)) (d e f (g))) 5 ((6 (7) 8) 9))) 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;filtro : Pred X List → List
;usage : (filtro pred lst) retorna una lista con los elementos de lst que cumplen con el predicado pred

  (define filtro
    (lambda (pred lst)
      (if (null? lst)
          '()
          (if (pred (car lst))
              (cons (car lst) (filtro pred (cdr lst)))
              (filtro pred (cdr lst)))
          )))
;pruebas :
(check-equal? (filtro number? '((1 2 a (b)) 3 4 c 5 d)) '(3 4 5))
(check-equal? (filtro symbol? '((1 2 a (b)) 3 4 c 5 d)) '(c d))
(check-equal? (filtro list? '((1 2 a (b)) 3 4 c 5 d)) '((1 2 a (b))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;inversion-listas : List(pair) → Listof(pair)
;usage : (inversion-listas lst) = la lista lst con sus elementos 2-List invertidos
(define (inversion-listas lista)
  (map (lambda ( 2-List)
         (list (cadr  2-List) (car  2-List)))
       lista))
;pruebas :
 (check-equal?(inversion-listas '((2 1) (4 3) (b a) (d c)))
'((1 2) (3 4) (a b) (c d)))
(check-equal?(inversion-listas '((es Racket) (genial muy) (21 20)))
'((Racket es) (muy genial) (20 21)))
(check-equal? (inversion-listas '((es Racket) (genial muy) (21 20)))
'((Racket es) (muy genial) (20 21)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;situar-en-lista : List X int X SchemeVal → List
;usage : (situar-en-lista lst pos elem) retorna la lista lst reemplazando el 
;         elemento en la posición pos por el elemento elem
(define situar-en-lista
 (lambda (lst pos elem)
   (if (equal? pos 0)
      (cons elem (cdr lst))
      (cons (car lst) (situar-en-lista (cdr lst) (- pos 1) elem)))))
;pruebas :
(check-equal?  (situar-en-lista '(1 2 3 4 5 6 7 8 9) 0 'Comienzo)
                     '(Comienzo 2 3 4 5 6 7 8 9))
(check-equal?  (situar-en-lista '(1 2 3 4 5 6 7 8 9) 4 'Mitad)
                     '(1 2 3 4 Mitad 6 7 8 9))
(check-equal?  (situar-en-lista '(1 2 3 4 5 6 7 8 9) 8 'Final)
                      '(1 2 3 4 5 6 7 8 Final))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ordenar : Pred X Listof(int) → Listof(int)
;usage : (ordenar pred lst-num) retorna una lista que contiene los elementos de lst-num
;        ordenados según el predicado pred que puede tomar el valor mayor qué (>) ó menor qué (<)
(define ordenar
  (lambda (pred lst)
    (if (null? lst)
        '()
        (append (ordenar pred (filtro (funNotAux pred (car lst)) (cdr lst))) (list (car lst))
                (ordenar pred (filtro (funAux pred (car lst)) (cdr lst)))) 
)))

;funNotAux: Pred X Num → #bool
;usage: (funNotAux pred n m) función auxiliar que retorna la negación del booleano al comparar 2 valores
;de acuerdo a la indicación del predicado
(define funNotAux
  (lambda (pred n)
    (lambda (m) (not (pred n m))
    )))

;funAux: Pred X Num → #bool
;usage: (funAux pred n m) función auxiliar que retorna un booleano al comparar 2 valores
;de acuerdo a la indicación del predicado
(define funAux
  (lambda (pred n)
    (lambda (m) (pred n m)
    )))

;pruebas :
(check-equal? (ordenar < '(58 41 67 54 32 10)) '(10 32 41 54 58 67))
(check-equal? (ordenar > '(58 41 67 54 32 10)) '(67 58 54 41 32 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;indice-lista : List X Pred → Int
;usage: (indice-lista lst pred) retorna la posición del primer elemento de lst que cumpla con el predicado pred
(define (indice-lista pred lst)
  (cond ((null? lst) -1)
        ((pred (car lst)) 0)
        ((letrec ((x (indice-lista pred (cdr lst))))
            (if x (+ 1 x) -1)))))
;pruebas :
(check-equal? (indice-lista (lambda (x) (eqv? x 'd)) '(a b c d e f g))3)
(check-equal? (indice-lista (lambda (x) (> x 3)) '(0 1 2 3 4 5 6))4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;contar-ocurrencias : Symbol X Listof(Sym) → int
;usage : (contar-ocurrencias elem lst-sym) retorna el número de ocurrencias de elem en lst-sym
(define contar-ocurrencias
  (lambda (elem lst)
   (if (null? lst)
      0
      (if (list? (car lst))
         (+ (contar-ocurrencias elem (car lst)) (contar-ocurrencias elem (cdr lst)))
         (if (equal? elem (car lst))
            (+ 1 (contar-ocurrencias elem (cdr lst)))
            (contar-ocurrencias elem (cdr lst)))))))
;pruebas :
(check-equal? (contar-ocurrencias 'x '((f x) y (((x z) x)))) 3)
(check-equal? (contar-ocurrencias 'x '((f x) y (((x z) () x)))) 3)
(check-equal?  (contar-ocurrencias 'w '((f x) y (((x z) x)))) 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;intercambio : Sym1 X Sym2 X Listof(Sym) | Symbol ∈ S-List → S-List
;usage : (intercambio elem1 elem2 lst-sym) retorna una lista similar a S-list sólo que cada ocurrencia anterior
;        de elem1 será reemplazada por elem2 y cada ocurrencia anterior de elem2 será reemplazada por elem1
(define intercambio
  (lambda (elem1 elem2 S-lst)
    (cond
      [(null? S-lst) '()]
      [(list? (car S-lst)) (cons (intercambio elem1 elem2 (car S-lst)) (intercambio elem1 elem2 (cdr S-lst)))]
      [(eqv? elem1 (car S-lst)) (cons elem2 (intercambio elem1 elem2 (cdr S-lst)))]
      [(eqv? elem2 (car S-lst)) (cons elem1 (intercambio elem1 elem2 (cdr S-lst)))]
      [else (cons (car S-lst) (intercambio elem1 elem2 (cdr S-lst)))]
      )))
;pruebas :
(check-equal? (intercambio 'a 'd '(a b c d)) '(d b c a))
(check-equal? (intercambio 'a 'd '(a d () c d)) '(d a () c a))
(check-equal? (intercambio 'x 'y '((x) y (z (x)))) '((y) x (z (y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;9;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;producto : List X List → 2-List
;usage : (producto lst1 lst2) retorna una lista de los pares correspondientes al producto cartesiando de lst1 y lst2
(define (producto lst1 lst2)
  (apply append
         (map (lambda (x)
                (map (lambda (y)
                       (list x y))
                     lst2))
              lst1)))
;pruebas :
(check-equal?(producto '(a b c)'(x y))
             '((a x) (a y) (b x) (b y) (c x) (c y)))
(check-equal?(producto '(1 3 0)'(2 4))
             '((1 2) (1 4) (3 2) (3 4) (0 2) (0 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;10;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;filter-acum : int X int X Binary-Procedure X int X Unitary-Procedure → int
;usage : (filter-acum a b F acum filter) retorna el resultado de aplicar la función binaria F a todos los números
;enteros que se encuentren en el intérvalo [a,b] y que cumplan el predicado filter, esto sumado al valor del
;acumulador acum
(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)
       0
       (if (filter a)
           (F a (filter-acum (+ a 1) b F (+ acum a) filter))
           (filter-acum (+ a 1) b F acum filter)))))
;pruebas :
(check-equal?  (filter-acum 1 10 + 0 odd?) 25)
(check-equal?  (filter-acum 1 10 + 0 even?) 30)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;11;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;list-append : List X List → List
;usage : (list-append lst1 lst2) retorna una lista con los elementos de lst2 concatenados a los de  lst1
(define list-append
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) lst2]
      [(null? lst2) lst1]
      [(cons (car lst1) (list-append (cdr lst1) lst2))])))
;pruebas :
(check-equal? (list-append '(1 2 3) '(4 5)) '(1 2 3 4 5))
(check-equal? (list-append '(a b c) '(d e)) '(a b c d e))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;12;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;operate : Listof(Binary-Procedure) X Listof(int) → int
;usage : (operate lrators lrands) n retorna el resultado de aplicar
;         sucesivamente las operaciones en lrators a los valores en lrands
(define operate
  (lambda (irators irands)
    (cond
      [(and (odd? (contar-ocurrencias - irators))
           (odd? (contar-ocurrencias / irators))) (* (expt (operador (revertir irators) (revertir irands)) -1) -1)]
      [(odd? (contar-ocurrencias - irators)) (* (operador (revertir irators) (revertir irands)) -1)]
      [(odd? (contar-ocurrencias / irators)) (expt (operador (revertir irators) (revertir irands)) -1)]
      [else (operador (revertir irators) (revertir irands))])))

(define operador
  (lambda (irators irands)
    (if (null? irators)
       (car irands)
       ((car  irators) (car irands) (operador (cdr irators) (cdr irands)))
       )))

;revertir: list → list
;usage: función que revierte la lista
(define revertir
  (lambda (lista)
  (if (null? lista)
      '()
      (append (revertir (cdr lista)) (list (car lista))))))

;pruebas :
(check-equal? (operate (list + * + - *) '(1 2 8 4 11 6))102)
(check-equal? (operate (list *) '(4 5))20)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;13;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;zip : Binary-Procedure X L1 X L2 | beList(L1) & beList(L2) & equals(length(L1) length(L2))
;usage : (zip F L1 L2) retorna una lista donde la posición n-ésima es el resultado de aplicar la 
;        función F a los n-esimos elementos de L1 Y L2
(define zip
  (lambda (F L1 L2)
    (if (and (null? L1) (null? L2))
       '()
       (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))))))
;pruebas :
(check-equal? (zip + '(1 4) '(6 2)) '(7 6))
(check-equal? (zip * '(11 5 6) '(10 9 8)) '(110 45 48))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;14;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;path : int X arbol → List        
;usage: (n arbol) retorna una lista con las direcciones que tuvo que recorrer para encontrar un
;número en las ramas del árbol
(define path
  (lambda (n arbol)
    (cond
      [(null? arbol) '()]
      [(eq? n (car arbol)) '()]
      [(< n (car arbol)) (cons 'left (path n (cadr arbol)))]
      [else (cons 'right (path n (caddr arbol)))]
    )))
;pruebas :
(check-equal? (path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ())))) '(right left left))
(check-equal? (path 10 '(1 (7 ())(2 (0 ())(13 () ()))))'(right right left))

;15;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;compose: Procedure proc1 & proc2 (val)->List
;usage: (compose proc1 proc2 val) Retorna la composición de proc1 y proc2 aplicados sobre val 
(define compose
     (lambda(proc1 proc2)
       (lambda(val)
         (proc1 [proc2 val]))))
;pruebas :
(check-equal?  ((compose car cdr) '(a b c d))'b)
(check-equal?  ((compose number? car)'(a b c d))#f)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;16;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;carCdr : elem lst errvalue acum -> expression
;usage : (carCdr elem lst errvalue)función principal de la estructura S-list que llama la función auxiliar
;        aux para que evalue los s-list
(define carCdr
  (lambda (elem lst errvalue)
    (define acum(lambda(list)list))
    (aux elem lst errvalue acum)))

;aux : elem lst errvalue retlst -> expression
;usage : (aux elem lst errvalue retlst)retorna el código para un procedimiento que toma una lista de estructura,
;        este devuelve el valor en la misma posición de la primera aparición de elem, si este no existe arroja
;        errvalue
(define aux
  (lambda(elem lst errvalue retlst)
    (cond
      [(null? lst)(retlst errvalue)]
      [(list? (car lst))(let
                            ((x(aux elem(car lst)errvalue(lambda(y)(retlst `(compose,y car))))))
                          (if(equal? x errvalue)
                             (aux elem (cdr lst) errvalue(lambda(y)(retlst `(compose,y cdr))))
                             x))]
      [(symbol?(car lst))(if(equal? elem (car lst))
                            (retlst 'car)
                            (aux elem (cdr lst) errvalue(lambda (y)
                                 (if(equal? errvalue y)
                                    errvalue
                                    (retlst `(compose, y cdr))))))]
      [else (eopl:error 'aux "Se ingresó un dato invalido" elem)])))

;pruebas:
(check-equal? (carCdr 'dog '(cat lion (fish dog ()) pig) 'fail)
              '(compose (compose (compose (compose car cdr) car) cdr) cdr))

(check-equal? (carCdr 'a '(a b c) 'fail)
               'car)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
