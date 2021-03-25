#lang eopl
(require rackunit)
;Taller 2 - Punto 1: Bignum para N=32

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699

;----------------------------GRAMÁTICA DE BIGNUM-------------------------------------------

;para N=32
;<bignum> ::=empty ,  (n=0)
;                ::= (cons r [q]) , (n=qN+r  0<=r <N)


;--------------------------IMPLEMENTACIÓN DE BIGNUM-------------------------------------

;Base
(define N 32)

;----------------------------------------------------------------------------------------------------

;zero: void -> null list
;purpose: definición de n=0
;usage: zero() retorna una lista vacía
(define zero (lambda () '()))

;----------------------------------------------------------------------------------------------------

;is-zero?: list -> boolean
;purpose: verificar si la lista ingresada es zero
;usage: (is-zero? n) retorna #t si n es zero, de lo contrario retorna #f
(define is-zero? (lambda (n) (or (null? n) (equal? n '(0)))))

;----------------------------------------------------------------------------------------------------

;valid-in-N-base?: list -> boolean
;purpose: verificar si la lista ingresada es un n válido en la base N
;usage: (valid-in-N-base n) retorna #t si n es válido en la base N, de lo contrario retorna #f
(define valid-in-N-base?
  (lambda (n)
    (cond
      [(is-zero? n) #t]
      [(>= (car n) N) #f]
      [(<(car n) N) (valid-in-N-base? (cdr n))])))

;----------------------------------------------------------------------------------------------------

;sucessor: list -> list
;purpose: retornar el sucesor de n en base N
;usage (successor n) retrona una lista con el sucesor de n si es una
;                             representación válida en N, de lo contrario
;                             retorna un error
(define successor
  (lambda (n)
    (cond
      [(is-zero? n) (list 1)]
      [(valid-in-N-base? n)
       (cond
         [(= (car n) (- N 1))
          (cons 0 (successor (cdr n)))]
         [(< (car n) (- N 1)) (cons (+ (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))

;----------------------------------------------------------------------------------------------------

;predecessor: list -> list
;purpose: retornar el predecesor de n en base N
;usage (successor n) retrona una lista con el predecesor de n si es una
;                             representación válida en N, de lo contrario
;                             retorna un error
(define predecessor
    (lambda (n)
    (cond
      [(is-zero? n) (eopl:error 'Bignum "No se pueden representar números menores que 0")]
      [(valid-in-N-base? n)
       (cond
         [(equal? n (successor '())) '()]
         [(= (car n) 0) (cons (- N 1) (predecessor (cdr n)))]
         [else (cons (- (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))


;-------------------------------------CÓDIGO CLIENTE------------------------------------------

;suma: list -> list
;purpose: sumar dos n en base N
;usage (suma x y) retorna la suma en base N de x e y
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

;----------------------------------------------------------------------------------------------------

;resta: list -> list
;purpose: restar dos n en base N
;usage (resta x y) retorna la resta en base N de x e y
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;----------------------------------------------------------------------------------------------------

;multiplicacion: list -> list
;purpose: multiplicar dos n en base N
;usage (multiplicacion x y) retorna la multiplicación en base N de x e y
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))

;----------------------------------------------------------------------------------------------------

;potencia: list -> list
;purpose: realizar potencia de dos n en base N
;usage (multiplicacion x y) retorna la potencia x a la y en base N
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

;----------------------------------------------------------------------------------------------------

;factorial: list -> list
;purpose: realizar el factorial de un n en base N
;usage (factorial x y) retorna el factorial de x en base N
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

;------------------------------------PRUEBAS-------------------------------------------------------


(check-equal? (suma '(2 0 1) '(1 2)) '(3 2 1)) ; En decimal: 1026 + 65 = 1091
(check-equal? (suma '(15 24 8) '(12 27)) '(27 19 9)) ; En decimal: 8975 + 876 = 9851
(check-equal? (suma '(31) '(0)) '(31)) ; En decimal: 31 + 0 = 31
(check-equal? (suma '() '(25)) '(25)) ; En decimal: 0 + 25 = 25

(check-equal? (resta '(5 2) '(3 1)) '(2 1)) ; En decimal: 69 - 35 = 34
(check-equal? (resta '(24 31) '(15 8)) '(9 23)) ; En decimal: 1016 - 271 = 745
(check-equal? (resta '() '(0)) '()) ; En decimal: 0 - 0 = 0
(check-equal? (resta '(13 25) '(0)) '(13 25)) ; 813 - 0 = 813

(check-equal? (multiplicacion '(5 3) '(8 1)) '(8 30 3)) ; En decimal: 101 * 40 = 4040
(check-equal? (multiplicacion '(2 4) '(6 2)) '(12 28 8)) ; En decimal: 130 * 70 = 9100
(check-equal? (multiplicacion '(31 4) '()) '()) ; En decimal: 160 * 0 = 0
(check-equal? (multiplicacion '(0) '(2 29)) '()) ; En decimal: 0 * 66 = 0

(check-equal? (potencia '(2 4) '(2)) '(4 16 16)) ; En decimal: 130 ^ 2 = 16900
(check-equal? (potencia '(15) '(3)) '(15 9 3)) ; En decimal: 15 ^ 3 = 3375
(check-equal? (potencia '(1 7) '(0)) '(1)) ; En decimal: 71 ^ 0 = 1
(check-equal? (potencia '(0) '(2)) '()) ; En decimal: 0 ^ 2 = 0

(check-equal? (factorial '(5)) '(24 3)) ; En decimal: 5! = 120
(check-equal? (factorial '()) '(1)) ; En decimal: 0! = 1
(check-equal? (factorial '()) (factorial '(1))) ; En decimal: 0! = 1!
(check-equal? (factorial '(8)) '(0 12 7 1)) ; En decimal: 8! = 40320