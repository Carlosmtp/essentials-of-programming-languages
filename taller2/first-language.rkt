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
    (expresion (""expresion""operacion""expresion) exp-lit)
    (expresion (identificador) variable)
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) declaracion)
    (operacion ("+") primitiva)
    (operacion ("-") primitiva)
    (operacion ("*") primitiva)
    (operacion ("/") primitiva))
  )
;La léxica
(define lexica
  '(
    (espacio (whitespace) skip)
    (comentario (";" (arbno (not #\newline))) skip)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
     (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
     (texto (letter (arbno (or digit letter))) string)
     (identificador ("'" letter (arbno (or letter digit))) symbol)
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