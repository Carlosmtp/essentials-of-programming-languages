#lang eopl
;Bignum para N=32

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616

(define base 16)
(define zero '())
(define is-zero? (lambda (n) (null? n)))
(define sucesor
  (lambda (n)
    (cond
    [(is-zero? n) '(1)]
    [(= (car n) (- base 1))
     (cons 0 (sucesor (cdr n)))]
    [else (cons (+ (car n) 1 ) (cdr n))])))
(define predecesor
    (lambda (n)
    (cond
          [(is-zero? n) (eopl:error "No se puede representar el valor")]
      [(equal? n (sucesor zero)) '()]
  [(= (car n) 0) (cons (- base 1) (predecesor (cdr n)))]
      [else (cons (- (car n) 1 ) (cdr n))])))                                                            