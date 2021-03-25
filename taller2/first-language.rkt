#lang eopl
;Taller 2 - Punto 7: Primer lenguaje

;Diana Katherine Toro Ortiz - 2110046
;Carlos Mauricio Tovar Parra - 1741699


;-----------------------------------GRAMÁTICA---------------------------------------------------

; <programa> ::= <expresion>
;                          un-program (exp)
; <expresion> ::= <numero>
;                          num-lit (n)
;                    ::= (<expresion> <operacion> <expresion>)
;                          exp-lit (exp1 op exp2)
;                    ::= <identificador>
;                          variable (id)
;                    ::= var (identificador = <expresion>)* in <expresion>
;                         declaracion (ids exps cuerpo)
; <operacion> := + - * /
;                         primitiva

;------------------------------IMPLEMENTACIÓN-----------------------------------------------


;Especificación gramátical
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

;----------------------------------------------------------------------------------------------------

;Especificación léxica
(define lexica
  '(
    (espacio (whitespace) skip)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit ))) symbol)
     ))

;----------------------------------------------------------------------------------------------------

;Definición automática de los datatypes
(sllgen:make-define-datatypes lexica gramatica)

;----------------------------------------------------------------------------------------------------

;Definición automática del Parser
(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;----------------------------------------------------------------------------------------------------
;Los Unparser

;Unparser de programa
(define unparse-program
  (lambda (prog)
    (cases programa prog
      (un-program (exp)
                  (unparse-expresion exp)))))

;Unparser de primitiva
(define unparse-primitiva
  (lambda (sym)
    (cases operacion sym
      (prim-suma () "+")
      (prim-resta () "-")
      (prim-multiplicacion () "*")
      (prim-division () "/"))))

;Unparser de expresión
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