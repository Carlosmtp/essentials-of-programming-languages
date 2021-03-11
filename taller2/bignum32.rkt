#lang eopl
(require rackunit)
;Bignum para N=32

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

;******************************************************************
;Implementación Bignum para N=32

(define N 32)
(define zero '())
(define is-zero? (lambda (n) (or (null? n) (equal? n '(0)))))
(define valid-in-N-base?
  (lambda (n)
    (cond
      [(null? n) #t]
      [(>= (car n) N) #f]
      [(<(car n) N) (valid-in-N-base? (cdr n))])))

(define successor
  (lambda (n)
    (cond
      [(valid-in-N-base? n)
       (cond
         [(is-zero? n) '(1)]
         [(= (car n) (- N 1))
          (cons 0 (successor (cdr n)))]
         [(< (car n) (- N 1)) (cons (+ (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))

(define predecessor
    (lambda (n)
    (cond
      [(valid-in-N-base? n)
       (cond
         [(is-zero? n) (eopl:error 'Bignum "No se pueden representar números menores que 0")]
         [(equal? n (successor zero)) '()]
         [(= (car n) 0) (cons (- N 1) (predecessor (cdr n)))]
         [else (cons (- (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))

;******************************************************************

;******************************************************************
; Código Cliente

(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        zero
        (suma (multiplicacion (predecessor x) y) y))
    ))
    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))
;******************************************************************
;Pruebas

(check-equal? (suma '(2 0 1) '(1 2)) '(3 2 1)) ; En decimal: 1026 + 65 = 1091
(check-equal? (suma '(15 24 8) '(12 27)) '(27 19 9)) ; En decimal: 8975 + 876 = 9851
(check-equal? (suma '(31) '(0)) '(31)) ; En decimal: 31 + 0 = 31
(check-equal? (suma '() '(25)) '(25)) ; En decimal: 0 + 25 = 25

(check-equal? (resta '(5 2) '(3 1)) '(2 1)) ; En decimal: 69 - 35 = 34
(check-equal? (resta '(24 31) '(15 8)) '(9 23)) ; En decimal: 1016 - 271 = 745
(check-equal? (resta '() '(0)) zero) ; En decimal: 0 - 0 = 0
(check-equal? (resta '(13 25) '(0)) '(13 25)) ; 813 - 0 = 813

(check-equal? (multiplicacion '(5 3) '(8 1)) '(8 30 3)) ; En decimal: 101 * 40 = 4040
(check-equal? (multiplicacion '(2 4) '(6 2)) '(12 28 8)) ; En decimal: 130 * 70 = 9100
(check-equal? (multiplicacion '(31 4) '()) zero) ; En decimal: 160 * 0 = 0
(check-equal? (multiplicacion '(0) '(2 29)) zero) ; En decimal: 0 * 66 = 0

(check-equal? (potencia '(2 4) '(2)) '(4 16 16)) ; En decimal: 130 ^ 2 = 16900
(check-equal? (potencia '(15) '(3)) '(15 9 3)) ; En decimal: 15 ^ 3 = 3375
(check-equal? (potencia '(1 7) '(0)) '(1)) ; En decimal: 71 ^ 0 = 1
(check-equal? (potencia '(0) '(2)) zero) ; En decimal: 0 ^ 2 = 0

(check-equal? (factorial '(5)) '(24 3)) ; En decimal: 5! = 120
(check-equal? (factorial zero) '(1)) ; En decimal: 0! = 1
(check-equal? (factorial '()) (factorial '(1))) ; En decimal: 0! = 1!
(check-equal? (factorial '(8)) '(0 12 7 1)) ; En decimal: 8! = 40320