#lang eopl
;

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699
;Juan Pablo Velasco Mellizo - 1766616


;La gramática
(define gramatica
  '(
    (programa (expresion) un-program)
    (expresion (numero) num-lit)
    (expresion ("("expresion operacion expresion")") exp-lit)
    (expresion (identificador) variable)
    (expresion ("var" (arbno  identificador "=" expresion ) "in" expresion) declaracion)
    (operacion ("+") prim-suma)
    (operacion ("-") prim-resta)
    (operacion ("*") prim-multiplicacion)
    (operacion ("/") prim-division))
  )
;La léxica
(define lexica
  '(
    (espacio (whitespace) skip)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit ))) symbol)
     ))
;Definición automática de los datatypes
(sllgen:make-define-datatypes lexica gramatica)
;Muestra los datatypes en sintaxis abstract
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))
;Parser
(define scan&parse
  (sllgen:make-string-parser lexica gramatica))
;Analizador léxico
(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

(define unparse-program
  (lambda (prog)
    (cases programa prog
      (un-program (exp)
                  (unparse-expresion exp)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (num-lit (num) (number->string num))
      (exp-lit (exp1 op exp2)
               (string-append "("(unparse-expresion exp1)" "(unparse-primitiva op)" "( unparse-expresion exp2)")"))
      (variable (id) (symbol->string (car id)))
      (declaracion (id exps cuerpo)
                   (string-append
                    "var "
                    (symbol->string (car id))
                    " = "
                    (car (map (lambda (x) (unparse-expresion x)) exps))
                    " in " (unparse-expresion cuerpo))))))




(define unparse-primitiva
  (lambda (sym)
    (cases operacion sym
      (prim-suma () "+")
      (prim-resta () "-")
      (prim-multiplicacion () "*")
      (prim-division () "/"))))