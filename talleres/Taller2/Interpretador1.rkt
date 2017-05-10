#lang eopl


;;
;; Jhonny Melgarejo 1310251
;; Sebastian Rios 1310105
;;


;******************************************************************************************
;; Interpretador
;;
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;  <primitive>     ::= + | - | * | add1 | sub1
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Especificación Léxica
;;
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
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Especificación Sintáctica (gramática)
;;
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
    ;;Vectores
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
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Definir datatypes (Construidos automáticamente)
;;
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)
;;
;; Ver datatypes construidos
;;
(define ver-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)
    )
  )
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Escaner El Analizador Léxico (Scanner)
;;
(define just-scan (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Parse El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;;
(define scan&parse (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))
;;
;******************************************************************************************






;******************************************************************************************
;;
;; El Interpretador (FrontEnd + Evaluación + señal para lectura )
;;
;******************************************************************************************
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm)
      (evaluar-programa pgm))
    (sllgen:make-stream-parser 
      especificacion-lexica
      especificacion-gramatical)
    )
  )
;;
;******************************************************************************************



;******************************************************************************************
;;
;; El Interprete
;;
;; evaluar-programa <programa> -> numero
;; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
;;
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (ambiente-inicial))))))
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Ambiente inicial (es vacio)
;;
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     ;'(i v x)
     ;'(1 5 10)
     (ambiente-vacio))
    )
  )
;;
;******************************************************************************************



;******************************************************************************************
;;
;; evaluar-expresion <expression> <enviroment> -> numero
;; evalua la expresión en el ambiente de entrada
;;
(define evaluar-expresion
  (lambda (exp ambiente)
    (cases expresion exp
      (lit-exp (datum) datum)
      (var-exp (id) (aplicar-ambiente ambiente id))
      (primapp-exp (prim rands)
                   (let ((args (evaluar-rands rands ambiente)))
                     (aplicar-primitiva prim args))))
    )
  )
;;
;******************************************************************************************



;******************************************************************************************
;;
;; aplicar-primitiva <primitiva> <list-of-expression> -> numero
;;
(define aplicar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))
;;
;******************************************************************************************



;******************************************************************************************
;;
;; funciones auxiliares para aplicar evaluar-expresion a cada elemento de una 
;; lista de operandos (expresiones)
;;
(define evaluar-rands
  (lambda (rands env)
    (map (lambda (x) (evaluar-rand x env)) rands)))
;;
(define evaluar-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Ambientes
;;
;; definición del tipo de dato ambiente
;;
(define-datatype ambiente ambiente?
  (ambiente-vacio-constructor)
  (ambiente-extendido-constructor (simbolos (list-of symbol?))
                                  (valores (list-of scheme-value?))
                                  (ambiente ambiente?))
  )
;;
(define scheme-value? (lambda (v) #t))
;;
;; ambiente-vacio:      -> enviroment
;; función que crea un ambiente vacío
;;
(define ambiente-vacio  
  (lambda ()
    (ambiente-vacio-constructor))) ;llamado al constructor de ambiente vacío
;;
;; ambiente-extendido: <list-of symbols> <list-of numbers> enviroment -> enviroment
;; función que crea un ambiente extendido
;;
(define ambiente-extendido
  (lambda (simbolos valores ambiente)
    (ambiente-extendido-constructor simbolos valores ambiente))) ;llamado al constructor de ambiente extendido
;;
;; función que busca un símbolo en un ambiente
;;
(define aplicar-ambiente
  (lambda (env simbolo)
    (cases ambiente env
      (ambiente-vacio-constructor ()
                                  (eopl:error 'aplicar-ambiente "No binding for ~s" simbolo))
      (ambiente-extendido-constructor (simbolos valores env)
                                      (let ((pos (list-buscar-posicion simbolo simbolos)))
                                        (if (number? pos)
                                            (list-ref valores pos)
                                            (aplicar-ambiente env simbolo))))
      )
    )
  )
;;
;******************************************************************************************



;******************************************************************************************
;;
;; Funciones Auxiliares
;;
;; funciones auxiliares para encontrar la posición de un símbolo
;; en la lista de símbolos de un ambiente
;;
(define list-buscar-posicion
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))
;;
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
;;
;******************************************************************************************







;******************************************************************************************
;;
;; Pruebas
;;
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
;;
;******************************************************************************************
