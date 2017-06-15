#lang eopl
(require racket/string)
(require racket/list)

;;
;; Jhonny Melgarejo 1310251
;; Sebastian Rios 1310105
;;



;******************************************************************************************
;;
;; Interpretador
;;
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
;;                      < true-exp >
;;                  ::= < "false" >
;;                      < false-exp >
;;                  ::= < texto >
;;                      < texto-exp ( datum ) >
;;                  ::= < variable >
;;                      < variable-exp ( id ) >
;;                  ::= var < variable > = < expresion >
;;                      < variableCreacion-exp ( id expresion ) >
;;                  ::= set < variable > = < expresion >
;;                      < variableAsignacion-exp ( id rhsexp ) >
;;                  ::= { { < expresion > }+ }
;;                      < lista-exp ( expresion body ) >
;;                  ::= # { { < expresion > }+ }
;;                      < vector-exp ( expresion body )>
;;                  ::= fun ( { variable }+ ) { < expresion > }+ end
;;                      < fun-exp ( id body expresion body )>
;;                  ::= proc ( { variable }* ) { < expresion > }+ end
;;                      < proc-exp ( ids expresion body )>
;;                  ::= ( variable { < expresion > }* )
;;                      < app-exp ( id body )>
;;                  ::= local ( ( { variable = < expresion > }* ) ( { < expresion > }+ ) )
;;                      < local-exp ( ids body expresion )>
;;                  ::= if < expresion > ? < expresion > : < expresion >
;;                      < if-exp (exp1 exp2 exp3)>
;;                  ::= cond {[ < expresion expresion > ]}* else < expresion >
;;                      < cond-exp ( cond1 cond2 expresion )>
;;                  ::= < primitive > ( { < expression > }* (,) )
;;                      < primitiva-exp ( prim expresion body ) >
;;
;;  <primitive>     ::= + | - | * | / | < | =< | > | >= | == | size | nth | agregar | delete
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
    (primitive ("delete") del-vector-prim) ;;Borrar  de un vector
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
     '(x)
     (list (direct-target 2))
     (ambiente-vacio))
    )
  )
;;
;******************************************************************************************

;**************************************************************************************
;;
;;Definición tipos de datos referencia y blanco
;;
(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))
;;
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))
;**************************************************************************************

;******************************************************************************************
;;
;; evaluar-expresion <expression> <enviroment> -> numero
;; evalua la expresión en el ambiente de entrada
;;
(define evaluar-expresion
  (lambda (exp ambiente)
    (cases expresion exp
      ;;Expresiones constantes
      (entero-exp (entero) (string->number entero))
      (flotante-exp (flotante) (string->number flotante))
      (true-exp () "true")
      (false-exp () "false")
      (texto-exp (texto) texto)
      ;; Variables. Estas son globales para todo el código
      (variable-exp (variable) (aplicar-ambiente ambiente variable)) ;;Consulta de variable
      (variableCreacion-exp (variable body) ;;Creacion de variable, no tiene retorno
                            (evaluar-crear-var '(variable) '(body) ambiente)
                            )
      (variableAsignacion-exp (variable rhsexp) ;;Asignacion de variable, no tiene retorno
                              (begin
                                (setref!
                                 (aplicar-ambiente-ref ambiente variable)
                                 (evaluar-expresion rhsexp ambiente))
                                "#void"
                                )
                              )
      ;; Listas
      (lista-exp (rator rands) (primt-list (append (list (evaluar-expresion rator ambiente)) (struct-lista rands ambiente)) "{"))
      ;;Vectores
      (vector-exp (rator rands) (primt-vect (append (list (evaluar-expresion rator ambiente)) (struct-lista rands ambiente)) "#{"))
      ;;Primitivas
      (primitiva-exp (prim rator rands)
                     (if (prim-aritmetica? prim)
                         ;"ES PRIMITIVA ARITMETICA"
                         (if (and (number? (evaluar-expresion rator ambiente))
                              (number? (car (struct-lista rands ambiente))))
                         (aplicar-primitiva prim
                                            (append
                                             (list (evaluar-expresion rator ambiente))
                                             ;(evaluar-rands rands ambiente)
                                             (struct-lista rands ambiente)
                                             ))
                         (eopl:error 'evaluar-expresion
                                 "Solo se operan numeros, listas y vectores"))
                         ;"NO ES PRIMITIVA ARITMETICA"
                         (if (prim-booleana? prim)
                             (if (and (number? (evaluar-expresion rator ambiente))
                                      (number? (car (struct-lista rands ambiente))))
                                 (aplicar-primitiva prim
                                                    (append
                                                     (list (evaluar-expresion rator ambiente))
                                                     ;(evaluar-rands rands ambiente)
                                                     (struct-lista rands ambiente)
                                                     ))
                                 (eopl:error 'evaluar-expresion
                                             "Solo se operan numeros, listas y vectores"))
                             ;"NO ES PRIMITIVA BOOLEANA"
                             (if (prim-list-vect? prim)
                                 ;"ES PRIMITIVA LISTA VECTOR"
                                  (aplicar-primitiva prim
                                                    (append
                                                     (list (evaluar-expresion rator ambiente))
                                                     ;(evaluar-rands rands ambiente)
                                                     (struct-lista rands ambiente)
                                                     ))
                                 ;"NO ES PRIMITIVA LISTA VECTOR"
                                 (if (prim-vect? prim)
                                     ;"ES PRIMITIVA VECTOR"
                                     (aplicar-primitiva prim
                                                    (append
                                                     (list (evaluar-expresion rator ambiente))
                                                     ;(evaluar-rands rands ambiente)
                                                     (struct-lista rands ambiente)
                                                     ))
                                     "NO ES PRIMITIVA VECTOR"
                                     )
                                 )
                             )
                         )
                   )
      ;; Crea una función y la asigna directamente a una variable
     (fun-exp (id ids body rands)
               (begin (closure-func id ids body rands ambiente) "#void")
                )
      ;; Retorna un valor procedimiento, es necesario asignarlo a una variable
      (proc-exp (ids body rands)
                (begin (closure-proc ids body rands ambiente) "#void")
                )
      ;; Condicionales
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (evaluar-expresion test-exp ambiente))
                  (evaluar-expresion true-exp ambiente)
                  (evaluar-expresion false-exp ambiente))
              )
      (cond-exp (rands1 rands2 rator)
                (cond-value? rands1 rands2 rator ambiente)
                )
      ;;Invoacion de funciones/procedmientos
      (app-exp (rator rands)
               rator
                                 
               ;(let ((proc (evaluar-expresion rator ambiente))
               ;      (args (evaluar-rands rands ambiente)))
               ;  ;(append (list proc) (list (car args)) (cdr args))
               ;  (if (procval? proc)
               ;      (apply-procedure proc args)
               ;      (if (funcval? proc)
               ;          "COSA"
               ;          ;(apply-function proc args)
               ;          (eopl:error 'evaluar-expresion
               ;                  "Attempt to apply non-procedure non-function ~s" proc))
               ;      )
               ;  )
               
               )
      ;; Locales (crea las variables y retorna un valor)
      (local-exp (ids rands body rator)
                 (if (null? rator)
                     (evaluar-expresion body (ambiente-extendido ids (evaluar-rands rands ambiente) ambiente))
                     (evaluar-expresion (last rator) (ambiente-extendido ids (evaluar-rands rands ambiente) ambiente))
                     )
                 )
      )
    )
  )
;;
;******************************************************************************************


;******************************************************************************************
;;
;; struct-lista: <lista-targets> <ambiente>
;; Convierte una lista de estructura de targets en una lista con sus correspondientes valores de referencia
;;
(define struct-lista
  (lambda (lista ambiente)
    (if (null? lista)
        empty
        (append  (list (evaluar-expresion (car lista) ambiente)) (struct-lista (cdr lista) ambiente)))
    )
  )
;;
;******************************************************************************************


;******************************************************************************************
;;
;; texto-value?: determina si un valor dado corresponde a un valor string
;;
(define texto-value?
  (lambda (x)
    (string? x)))
;;
;******************************************************************************************
;;
;; true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
;;
(define true-value?
  (lambda (x)
    (not (eqv? x 'false))))
;******************************************************************************************
;;
;; cond-value? determina si un valor dado corresponde a un valor booleano falso o verdadero
;;
(define cond-value?
  (lambda (conds resp defect ambiente)
    (if (null? conds)
        (evaluar-expresion defect ambiente)
        (if (true-value? (evaluar-expresion (car conds) ambiente))
            (evaluar-expresion (car resp) ambiente)
            (cond-value? (cdr conds) (cdr resp) defect ambiente)
            )
        )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funciones para imprimir lista
;;
(define primt-list
  (lambda (lista salida)
    ;lista
    (if (null? lista)
        (string-append "}")
        (string-append salida (number->string (car lista)) (primt-list (cdr lista) " "))
        )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para imprimir vector
;;
(define primt-vect
  (lambda (vector salida)
    (if (null? vector)
        (string-append "}")
        (if (null? salida)
            (string-append " " (number->string (car vector)) (primt-vect (cdr vector) empty))
            (string-append salida (number->string (car vector)) (primt-vect (cdr vector) empty))
            )
        )))
;******************************************************************************************

;******************************************************************************************
;; Funcion para imprimir un vector con un elemento nuevo (primitiva agregar)
;;
(define primt-vect-agregar
  (lambda (vector salida i)
    (if (= (length vector) i)
        (string-append " " (number->string (last vector)) "}")
        (if (null? salida)
            (string-append " " (car vector) (primt-vect-agregar (cdr vector) empty i ))
            (string-append salida (car vector) (primt-vect-agregar (cdr vector) empty i)))
        )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para imprimir un vector despues de eliminar un elemento (primita delete)
(define primt-vect-delete
  (lambda (vector salida)
    (if (null? vector)
        (string-append "}")
        (if (null? salida)
            (string-append " " (car vector) (primt-vect-delete (cdr vector) empty))
            (string-append salida (car vector) (primt-vect-delete (cdr vector) empty))
            )
        )))
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para validar si es una primitiva aritmetica
;;
(define prim-aritmetica?
  (lambda (prim)
    (cases primitive prim
      (sum-prim () #t)
      (sub-prim () #t)
      (mul-prim () #t)
      (div-prim () #t)
      (else #f)
      )
    )
  )
;;
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para validar si es una primitiva booleana
;;
(define prim-booleana?
  (lambda (prim)
    (cases primitive prim
      (menor-prim () #t)
      (menig-prim () #t)
      (mayor-prim () #t)
      (mayig-prim () #t)
      (igual-prim () #t)
      (else #f)
      )
    )
  )
;;
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para validar si es una primitiva de listas y vectores
;;
(define prim-list-vect?
  (lambda (prim)
    (cases primitive prim
      (size-prim () #t)
      (nth-prim () #t)
      (else #f)
      )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para validar si es una primitiva de vector
;;
(define prim-vect?
  (lambda (prim)
    (cases primitive prim
      (add-vector-prim () #t)
      (del-vector-prim () #t)
      (else #f)
      )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para consultar el elemento n-esimo del vector
;;
(define get-element
  (lambda (lista pos i)
    (cond
      ((null? lista)
       (eopl:error 'get-element
                   "No existen elementos")
       )
      ((= pos i) (car lista))
      (else
       (get-element (cdr lista) pos (+ i 1))
       )
      )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para eliminar el elemento n-esimo del vector
;;
(define delete-element
  (lambda (lista pos i listab)
    (cond
      ((null? lista)
       (eopl:error 'delete-element
                   "No existen elementos")
       )
      ((= pos i) (append listab (cdr lista)))
      (else
       (delete-element (cdr lista) pos (+ i 1) (append listab (list (car lista))))
       )
      )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funcion para aplicar una primitiva
;;
(define aplicar-primitiva
  (lambda (prim args)
    (cases primitive prim
      ;; Operaciones aritméticas
      (sum-prim () (if (null? args)
                       0
                       (+ (car args) (aplicar-primitiva prim (cdr args))))
                )
      (sub-prim () (if (null? args)
                       0
                       (- (car args) (aplicar-primitiva prim (cdr args)))
                       )
                )
      (mul-prim () (if (null? args)
                       1
                       (* (car args) (aplicar-primitiva prim (cdr args))))
                )
      (div-prim () (if (null? args)
                       1
                       (/ (car args) (aplicar-primitiva prim (cdr args))))
                )
      ;; Comparación
      (menor-prim () (if (< (car args) (cadr args)) 'true 'false))
      (menig-prim () (if (<= (car args) (cadr args)) 'true 'false))
      (mayor-prim () (if (> (car args) (cadr args)) 'true 'false))
      (mayig-prim () (if (>= (car args) (cadr args)) 'true 'false))
      (igual-prim () (if (= (car args) (cadr args)) 'true 'false))
      ;; Listas y vectores
      (size-prim () (if (string-prefix? (car args) "{")
                        (length (string-split (string-replace (string-replace (car args) "{" "") "}" "")))
                        (length (string-split (string-replace (string-replace (string-replace (car args) "{" "") "}" "") "#" "")))
                        )
                 )
      (nth-prim () (if (string-prefix? (car args) "{")
                       (get-element (string-split (string-replace (string-replace (car args) "{" "") "}" "")) (cadr args) 1)
                       (get-element (string-split (string-replace (string-replace (string-replace (car args) "{" "") "}" "") "#" "")) (cadr args) 1)
                       )
                )
      ;; Vector
      (add-vector-prim () (if (string-prefix? (car args) "#")
                              (primt-vect-agregar (append (string-split (string-replace (string-replace (string-replace (car args) "{" "") "}" "") "#" "")) (list (cadr args))) "#{" 1)
                              (eopl:error 'evaluar-expresion
                                             "Primitiva solo para vectores")
                              )
                       )
      (del-vector-prim () (if (string-prefix? (car args) "#")
                              (primt-vect-delete (delete-element (string-split (string-replace (string-replace (string-replace (car args) "{" "") "}" "") "#" "")) (cadr args) 1 '())"#{")
                              (eopl:error 'evaluar-expresion
                                             "Primitiva solo para vectores")
                              )
                       )
      )
    )
  )
;******************************************************************************************

;******************************************************************************************
;;
;; Funciones auxiliares para aplicar evaluar-expresion a cada elemento de una 
;; lista de operandos (expresiones)
;;
(define evaluar-rands
  (lambda (rands env)
    (map (lambda (x) (evaluar-rand x env)) rands)))
;;
(define evaluar-rand
  (lambda (rand env)
    (cases expresion rand
      (variable-exp (id)
               (indirect-target
                (let ((ref (aplicar-ambiente-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))
;; -
;(define evaluar-rand
;  (lambda (rand env)
;    (evaluar-expresion rand env)))
;;
(define evaluar-crear-var
  (lambda (simbolos valores ambiente)
    (ambiente-extendido '(variable) '(body) ambiente) "#void"))
;******************************************************************************************

;******************************************************************************************
;;
;; Ambientes
;;
;; Definición del tipo de dato ambiente
;;
;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio-constructor)
  (ambiente-extendido-constructor
   (simbolos (list-of symbol?))
   (vector vector?)
   (ambiente ambiente?)))
;; -
;(define-datatype ambiente ambiente?
;  (ambiente-vacio-constructor)
;  (ambiente-extendido-constructor (simbolos (list-of symbol?))
;                                  (valores (list-of scheme-value?))
;                                  (ambiente ambiente?))
;  )
;;
(define scheme-value? (lambda (v) #t))
;;
;; Ambiente-vacio:      -> enviroment
;; Función que crea un ambiente vacío
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
    (ambiente-extendido-constructor simbolos (list->vector valores) ambiente))) ;llamado al constructor de ambiente extendido
;;
;; Función que busca un símbolo en un ambiente
;;
(define aplicar-ambiente
  (lambda (env sym)
    (deref (aplicar-ambiente-ref env sym))))
;;
(define aplicar-ambiente-ref
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio-constructor ()
                        (eopl:error 'aplicar-ambiente-ref "No binding for ~s" sym))
      (ambiente-extendido-constructor (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (aplicar-ambiente-ref env sym)))))))
;; -
;(define aplicar-ambiente
;  (lambda (env simbolo)
;    (cases ambiente env
;      (ambiente-vacio-constructor ()
;                                  (eopl:error 'aplicar-ambiente "Error no existe ~s" simbolo))
;      (ambiente-extendido-constructor (simbolos valores env)
;                                      (let ((pos (list-buscar-posicion simbolo simbolos)))
;                                        (if (number? pos)
;                                            (list-ref valores pos)
;                                            (aplicar-ambiente env simbolo))))
;      )
;    )
;  )
;;
;(define aplicar-ambiente-ref
;  (lambda (env sym)
;    (cases ambiente env
;      (ambiente-vacio-constructor ()
;                        (eopl:error 'aplicar-ambiente-ref "No binding for ~s" sym))
;      (ambiente-extendido-constructor (syms vals env)
;                           (let ((pos (rib-find-position sym syms)))
;                             (if (number? pos)
;                                 (a-ref pos (list->vector vals))
;                                 (aplicar-ambiente-ref env sym)))))))
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
(define rib-find-position 
  (lambda (sym los)
    (list-buscar-posicion sym los)))
;;
;******************************************************************************************

;******************************************************************************************
;;
;; Referencias
;;
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x))))
;;
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))
;;
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))
;;
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))
;;
(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))
;; -
;(define setref!
;  (lambda (ref val)
;    (primitive-setref! ref val)))
;;
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
;;
;******************************************************************************************

;******************************************************************************************
;;
;; Procedimientos
;;
(define-datatype procval procval?
  (closure-proc
   (ids (list-of symbol?))
   (body expresion?)
   (rands (list-of expresion?))
   (env ambiente?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure-proc (ids body rands env)
               (evaluar-expresion body (ambiente-extendido ids args env))
               (evaluar-expresion rands (ambiente-extendido ids args env))))))
;;
;******************************************************************************************
;;
;; Funciones
;;
(define-datatype funcval funcval?
  (closure-func
   (id symbol?)
   (ids (list-of symbol?))
   (body expresion?)
   (rands (list-of expresion?))
   (env ambiente?)))

;apply-function evalua el cuerpo de una funcion en el ambiente extendido correspondiente
(define apply-function
  (lambda (func args)
    (cases funcval func
      (closure-func (id1 ids body rands env)
                    (let ([id (evaluar-expresion body (ambiente-extendido ids args env))]) id)
                    (let ([id (evaluar-expresion rands (ambiente-extendido ids args env))]) id)
                    ))))
;******************************************************************************************

;; -- Interpretador
(interpretador)

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
(scan&parse "cond [>(x,0) 1] [<(x,3) 2] else 2")
(scan&parse "if <(x,4) ? 'hola' : 'no'")
(scan&parse "local ( ( a = 3 b = 3) (+(a,b,3)) )")
(scan&parse "local ( ( a = 3 b = 3) ('prueba') )")
;;
;******************************************************************************************
