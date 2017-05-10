;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname duda1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

#lang eopl


;;
;; Jhonny Melgarejo 1310251
;; Sebastian Rios 1310105
;;


;******************************************************************************************
;;;;; Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>       ::= < expresion >
;;                      < un-programa ( exp ) >
;;
;;  <expresion>    ::= < entero >
;;                      < entero-exp ( datum ) >
;;                  ::= < flotante >
;;                      < flotante-exp ( datum ) >
;;                  ::= < "true" >
;;                      < true-exp ( datum ) >
;;                  ::= < "false" >
;;                      < false-exp ( datum ) >
;;                  ::= < texto >
;;                      < texto-exp ( datum ) >
;;                  ::= < variable >
;;                      < variable-exp ( datum ) >
;;                  ::= var < variable > = < expresion >
;;                      < variableCreacion-exp ( ?? ) >
;;                  ::= set < variable > = < expresion >
;;                      < variableAsignacion-exp ( id rhsexp ) >
;;                  ::= { { < expresion > }+ }
;;                      < lista-exp ( datum ) >
;;                  ::= # { { < expresion > }+ }
;;                      < vector-exp ( ?? )>
;;                  ::= fun ( { variable }+ ) { < expresion > }+ end
;;                      < fun-exp ( ids?? body?? )>
;;                  ::= proc ( { variable }* ) { < expresion > }+ end
;;                      < proc-exp ( ids body )>
;;                  ::= ( variable { < expresion > }* )
;;                      < app-exp ( ?? )>
;;                  ::= local ( ( { variable = < expresion > }* ) ( { < expresion > }+ ) )
;;                      < local-exp ( ?? )>
;;                  ::= if < expresion > ? < expresion > : < expresion >
;;                      < if-exp (exp1 exp2 exp3)>
;;                  ::= cond {[ < expresion expresion > ]}* else < expresion >
;;                      < cond-exp ( ?? )>
;;                  ::= < primitive > ( { < expression > }* (,) )
;;                      < primitiva-exp ( ?? ) >
;;
;;  <primitive>     ::= + | - | * | / | < | =< | > | >= | == | size | nth | agregar | delete
;;

;******************************************************************************************

;******************************************************************************************


;Especificación Léxica
(define especificacion-lexica
  '(
    (espacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (variable (letter (arbno (or letter digit "_" "?"))) symbol)
    (texto ("'" (arbno (or letter digit)) "'" ) string)
    (entero ((or digit (concat "-" digit)) (arbno digit)) string)
    (flotante ((or digit (concat "-" digit)) (arbno digit) "." digit (arbno digit)) string)
    )
  )

;Especificación Sintáctica (gramática)
(define especificacion-gramatical
  '(
    (programa  (expresion) un-programa)
    ;;Expresiones constantes
    (expresion (entero) entero-exp)
    (expresion (flotante) flotante-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion (texto) texto-exp)
    ;; Variables. Estas son globales para todo el código
    (expresion (variable) variable-exp) ;;Consulta de variable
    (expresion ("var" variable "=" expresion) variableCreacion-exp) ;;Creacion de variable, no tiene retorno
    (expresion ("set" variable "=" expresion) variableAsignacion-exp) ;;Asignacion de variable, no tiene retorno
    ;; Listas
    (expresion ("{" expresion (arbno expresion) "}") lista-exp)
    ;;Vectore
    (expresion ("#" "{" expresion (arbno expresion) "}") vector-exp)
    ;;Funciones (el primer argumento es el nombre de la función, los siguientes los argumentos, se retorna el último valor de la expresion
    (expresion ("fun" "(" variable (arbno variable) ")" expresion (arbno expresion) "end") fun-exp) ;;Crea una función y la asigna directamente a una variable
    (expresion ("proc" "(" (arbno variable) ")" expresion (arbno expresion) "end") proc-exp) ;;Retorna un valor procedimiento, es necesario asignarlo a una variable
    ;;Invoacion de funciones/procedmientos
    (expresion ("(" variable  (arbno expresion) ")") app-exp)
    ;;Locales (crea las variables y retorna un valor)
    (expresion ("local"
                   "("
                        ;;Zona de creación de variables locales
                        "("
                        (arbno variable "=" expresion)
                        ")"
                        ;;Zona de expresiones (siempre retorna la última)"
                        "("
                        expresion (arbno expresion)
                        ")"
                     ")"
                  )
               local-exp)
    ;; Condicionales
    (expresion ("if" expresion "?" expresion ":" expresion) if-exp)
    (expresion ("cond" (arbno "[" expresion expresion "]") "else" expresion) cond-exp)
    ;;Primitivas
    (expresion (primitive "(" expresion (arbno "," expresion) ")") primitiva-exp) 
   ;; Operaciones aritméticas
    (primitive ("+") sum-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mul-prim)
    (primitive ("/") div-prim)
    ;; Comparación
    (primitive ("<")  menor-prim)
    (primitive ("=<") menig-prim)
    (primitive (">")  mayor-prim)
    (primitive (">=") mayig-prim)
    (primitive ("==") igual-prim)
    ;; Listas y vectores
    (primitive ("size") size-prim) ;;Retorna el tamaño de la lista
    (primitive ("nth") nth-prim) ;; Retorna el n-esimo elemento de a lista (se cuenta desde 1 hasta n)
    ;; Vector
    (primitive ("agregar") add-vector-prim) ;;Agrega un elemento a un vector
    (primitive ("delete") borra un elemento de un vector) ;;Borrar  de un vector
    )
  )

;;Definir datatypes (Construidos automáticamente)
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;Escaner
(define just-scan (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;;Parse
(define scan&parse (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))




;*******************************************************************************************

;El Interpretador

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa  pgm))
    (sllgen:make-stream-parser 
      especificacion-lexica
      especificacion-gramatical)))



;El Interprete

; evaluar-programa: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (ambiente-inicial))))))



; evaluar-expresion: <expresion> <ambiente> -> numero
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp ambiente)
    (cases expresion exp
      (entero-exp (entero) entero)
      (flotante-exp (flotante) flotante)
      (true-exp ("true") #t)
      (false-exp ("false") #f)
      (texto-exp (texto) texto)
      (variable-exp (variable) variable)
      (variableCreacion-exp (id rhsexp) 1) ;; ??
      (variableAsignacion-exp (id rhsexp)
                              (begin
                                (setref!
                                 (aplicar-ambiente-ref ambiente id)
                                 (evaluar-expresion rhsexp ambiente))
                                1))
      (lista-exp (lista) lista)
      (vector-exp (vector) vector)
      (fun-exp (funcion) "fun" "(" variable (arbno variable) ")" expresion (arbno expresion) "end") ;; ??
      (proc-exp (ids body) (closure ids body ambiente))
      (app-exp (aplicacion) "(" variable { < expresion > }* ")") ;; ??
      (local-exp (local) "local" "(" "(" { variable = < expresion > }* ")" "(" { < expresion > }+ ")" ")") ;; ??
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (evaluar-expresion test-exp ambiente))
                  (evaluar-expresion true-exp ambiente)
                  (evaluar-expresion false-exp ambiente)))
      (cond-exp (condicional) "cond" {[ < expresion expresion > ]}* else < expresion >) ;; ??
      (primitiva-exp (prim rands)
                     (let ((args (eval-primapp-exp-rands rands ambiente)))
                       (apply-primitive prim args)))
      
      )))


;*******************************************************************************************






;;Pruebas

(scan&parse "x")
(scan&parse "X")
(scan&parse "4")
(scan&parse "true")
(scan&parse "4.5")
(scan&parse "'perro'")
(scan&parse "var x = 8")
(scan&parse "set x = 3")
(scan&parse "{ 1 2 3 4 5}")
(scan&parse "#{ 1 2 3 4 5}")
(scan&parse "+(4,2)")
(scan&parse "+(4,2,2,3,4,5)")
(scan&parse ">=(4,2)")
(scan&parse "if >=(4,2) ? 3 : 8")
(scan&parse "fun(factorial x) if ==(x,0) ? 1 : (factorial -(x,1)) end")
(scan&parse "(factorial 5)")
(scan&parse "fun(funcion x y) set x = +(x,y) set y = +(y,3) *(x,y) end")
(scan&parse "var fact = proc(x) if ==(x,0) ? 1 : (fact -(x,1)) end")
(scan&parse "(fact 4)")
(scan&parse "var func = proc(x y) set x = +(x,y) set y = +(y,3) *(x,y) end")
(scan&parse "size({1 2 3 4 5})")
(scan&parse "size(#{2 3 4 5})")
(scan&parse "nth({1 2 3 4 5}, 3)")
(scan&parse "nth(#{2 3 4 5}, 2)")
(scan&parse "agregar(#{1 2 3 4 5}, 10)")
(scan&parse "delete(#{1 2 3 4 5}, 2)")
;;Bug en cond no trabaja con textos
(scan&parse "cond [>(x,y) 1] [<(x,y) 2] else 2")
(scan&parse "if <(x,y) ? 'hola' : 'no'")
(scan&parse "local ( ( a = 3 b = 3) (+(a,b,3)) )")
(scan&parse "local ( ( a = 3 b = 3) ('prueba') )")