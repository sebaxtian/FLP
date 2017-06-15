#lang eopl
;(require racket/base)
(require (only-in racket/base box))
(require (only-in racket/base box?))
(require (only-in racket/base unbox))
(require (only-in racket/base set-box!))



;******************************************************************************************
;;
;; PROYECTO FINAL FUNDAMENTOS DE LENGUAJES DE PROGRAMACION
;;
;; INTERPRETADOR DE LENGUAJE OBLIQ
;;
;; Presentado por:
;;
;; Jhonny Melgarejo  -  1310251
;; Sebastian Rios  -  1310105
;;
;; Docente:
;; Carlos Andres Delgado
;;
;******************************************************************************************


;******************************************************************************************
;;
;; La definición BNF para las expresiones del Lenguaje OBLIQ:
;;
;; Algunas convenciones:
;; * Lo que va entre < > corresponde a un tipo de dato
;; * Lo que esta entre comillas " " es texto que va en la estructura de la gramatica
;; * La gramatica inicia en <programa>
;;
;******************************************************************************************
;;
;;  <programa>      ::= <expresion>
;;                      <un-programa (exp)>
;;  
;;  <expresion>     ::= <bool-expresion>
;;                      <bool-exp>
;;                  ::= <identificador>
;;                      <id-exp>
;;                  ::= <numero>
;;                      <num-exp>
;;                  ::= <caracter>
;;                      <caracter-exp>
;;                  ::= <cadena>
;;                      <cadena-exp>
;;                  ::= ok
;;                      <ok-exp>
;;                  ::= "var" { <identificador> = <expresion> }*(,) "in" <expresion> "end"
;;                      <var-exp>
;;                  ::= "let" { <identificador> = <expresion> }*(,) "in" <expresion> "end"
;;                      <let-exp>
;;                  ::= "letrec" { <identificador> " ( " { <identificador> }*(,) " ) " = <expresion> }* "in" <expresion> "end"
;;                      <letrec-exp>
;;                  ::= "set" <identificador> " := " <expresion>
;;                      <set-exp>
;;                  ::= "begin" <expresion> { <expresion> }*(;) "end"
;;                      <begin-exp>
;;                  ::= <primitiva> " ( " { <expresion> }* " ) "
;;                      <primapp-exp>
;;                  ::= "if" <bool−expresion> "then" <expresion> { "elseif" <bool−expresion> "then" <expresion> }* "else" <expresion> "end"
;;                      <cond-exp>
;;                  ::= "proc" " ( " { <identificador> }*(,) " ) " <expresion> "end"
;;                      <proc-exp>
;;                  ::= "apply" <identificador> " ( " { <expresion> }*(,) " ) "
;;                      <apply-exp>
;;                  ::= "meth" " ( " <identificador> " , " { <identificador> }*(,) " ) " <expresion> "end"
;;                      <meth-exp>
;;                  ::= "for" <identificador> " = " <expresion> "to" <expresion> "do" <expresion> "end"
;;			<for-exp>
;;                  ::= "object" " { " { <identificador> " => " <expresion> }* " } "
;;			<obj-exp>
;;                  ::= "get" <identificador> " . " <identificador>
;;			<get-exp>
;;                  ::= "send" <identificador> " . " <identificador> " ( " { <identificador> }*(,) " ) "
;;			<send-exp>
;;                  ::= "update" <identificador> " . " <identificador> " := " <expresion>
;;			<update-exp>
;;                  ::= "clone" " ( " <identificador> { <identificador> }*(,) " ) "
;;			<clone-exp>
;;
;;  <bool-expresion>::= true
;;			<true-exp>
;;                  ::= false
;;			<false-exp>
;;		    ::= <bool-primitiva> " ( " { <expresion> }*(,) " ) "
;;                      <aplicar-bool>
;;		    ::= <bool-oper> " ( " { <bool-expresion> }*(,) " ) "
;;                      <bool-oper>
;;
;;  <primitiva>	    ::= +
;;			<prim-suma>
;;                  ::= -
;;			<prim-resta>
;;	            ::= *
;;			<prim-multiplicacion>
;;  	  	    ::= %
;;			<prim-modulo>
;;		    ::= &
;;			<prim-ampersand>
;;
;;  <bool-primitiva>::= <
;;			<prim-menor>
;;		    ::= >
;;			<prim-mayor>
;;		    ::= <=
;;			<prim-menor-igual>
;;		    ::= >=
;;			<prim-mayor-igual>
;;	      	    ::= is
;;			<prim-igual-igual>
;;
;;  <bool-oper>	    ::= not
;;			<oper-not>
;;		    ::= and
;;			<oper-and>
;;		    ::= or
;;			<oper-or>
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Especificación Léxica
;;
;******************************************************************************************
;;
(define obliq-especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario ( "(" "*" (arbno (not #\newline)) "*" ")") skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (caracter ( "\'" letter "\'" ) symbol)
    (cadena ( "\""(or letter whitespace digit) (arbno (or whitespace letter digit)) "\"") string)))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Especificación Sintáctica (gramática)
;;
;******************************************************************************************
;;
(define obliq-especificacion-gramatical
  '(
    (programa (expresion) un-programa)
    ;; CONSTANTES
        (expresion (bool-expresion) bool-exp)
	(expresion (identificador) id-exp)
	(expresion (numero) num-exp)
	(expresion (caracter) caracter-exp)
        (expresion (cadena) cadena-exp)
	(expresion ("ok") ok-exp)
    ;; OPERADORES
	(primitiva ("+") prim-suma)
        (primitiva ("-") prim-resta)
        (primitiva ("*") prim-multiplicacion)
        (primitiva ("/") prim-division)
        (primitiva ("%") prim-modulo)
        (primitiva ("&") prim-ampersand)
        (bool-primitiva ("<") prim-menor)
        (bool-primitiva (">") prim-mayor)
        (bool-primitiva ("<=") prim-menor-igual)
        (bool-primitiva (">=") prim-mayor-igual)
        (bool-primitiva ("is") prim-igual-igual)
        (bool-oper ("not") oper-not)
        (bool-oper ("and") oper-and)
        (bool-oper ("or") oper-or)
    ;; OBJETOS
	(expresion ("object" "{" (arbno identificador "=>" expresion) "}") obj-exp)
	(expresion ("meth" "(" (arbno identificador) " , " (separated-list identificador ",") ")" expresion "end") meth-exp)
	(expresion ("send" identificador "." identificador "(" (separated-list identificador ",") ")") send-exp)
	(expresion ("clone" "(" identificador (separated-list identificador ",") ")") clone-exp)
	(expresion ("update" identificador "." identificador ":=" expresion) update-exp)
	(expresion ("get" identificador "." identificador) get-exp)
    ;; ESTRUCTURAS DE CONTROL
	(expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)
        (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
        (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ") =" expresion) "in" expresion "end") letrec-exp)
	(expresion ("set" identificador ":=" expresion) set-exp)
        (expresion ("begin" expresion (separated-list expresion ";") "end") begin-exp)
	(expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
	(expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") cond-exp)
	(expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)
	(expresion (primitiva "(" (arbno expresion ) ")") primapp-exp)
        (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
        (bool-expresion ("true") true-exp)
	(bool-expresion ("false") false-exp)
	(bool-expresion (bool-primitiva "(" (separated-list expresion ",") ")") aplicar-bool)
        (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") oper-bool)
  ))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Definir datatypes (Construidos automáticamente)
;;
;******************************************************************************************
;;
(sllgen:make-define-datatypes obliq-especificacion-lexica obliq-especificacion-gramatical)
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Ver datatypes construidos
;;
;******************************************************************************************
;;
(define ver-datatypes
  (lambda () (sllgen:list-define-datatypes obliq-especificacion-lexica obliq-especificacion-gramatical)))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Escaner El Analizador Léxico (Scanner)
;;
;******************************************************************************************
;;
(define just-scan
  (sllgen:make-string-scanner obliq-especificacion-lexica obliq-especificacion-gramatical))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Parse El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;;
;******************************************************************************************
;;
(define scan&parse
  (sllgen:make-string-parser obliq-especificacion-lexica obliq-especificacion-gramatical))
;;
;******************************************************************************************




;******************************************************************************************
;;
;; Evaluador de Programa
;; Evalua un programa que es una expresion en un ambiente inicial vacio
;;
;******************************************************************************************
;;
(define evaluar-programa 
  (lambda (pgm)
    (cases programa pgm
      (un-programa(expresion)
        (evaluar-expresion expresion (empty-env))))))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; El Interpretador (FrontEnd + Evaluación + señal para lectura )
;;
;******************************************************************************************
;;
(define interpretador
  (sllgen:make-rep-loop  "$command-> " evaluar-programa
    (sllgen:make-stream-parser 
      obliq-especificacion-lexica
      obliq-especificacion-gramatical)))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Ambientes
;; Especificacion de los ambientes usados por el interpretador
;;
;******************************************************************************************
;; Definicion de tipo de dato ambiente
(define-datatype environment environment?
	; Ambiente vacio
	(empty-env-record)
	; Ambiente extendido
	(extended-env-record
		(syms (list-of symbol?))
		(vals vector?)
		(env environment?))
	; Ambiente recursivo extendido
	(recursively-ext-env-record
		(proc-names (list-of symbol?))
		(proc-ids (list-of (list-of symbol?)))
		(proc-bodies (list-of expresion?))
		(env environment?)))
;;
;; Funciones Auxiliares de Ambientes
;;
;; Crea un ambiente vacio
(define empty-env  
  (lambda ()
    (empty-env-record)))
;; Crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))
;; Crea un ambiente recursivo extendido (letrec-exp)
(define extend-env-recursively
  (lambda (proc-names proc-ids proc-bodies env)
    (recursively-ext-env-record proc-names proc-ids proc-bodies env)))
;; Aplica un ambiente a un símbolo retorna su valor asociado
(define apply-env
	(lambda (env sym)
		(cases environment env
			; Caso ambiente vacio
			(empty-env-record ()
				; Error cuando el ambiente es vacio
				(eopl:error 'apply-env "No binding for ~s" sym))
			; Caso ambiente extendido
			(extended-env-record (syms vals env)
				; Busca la posicion del simbolo en la lista
				(let ((position (rib-find-position sym syms)))
					(if (number? position)
						; Busca el valor del simbolo en la posicion
						(let ((val (vector-ref vals position)))
							;; si el valor encontrado es una celda utilizamos la funcion auxiliar extraer el valor
							(if (box? val) (unbox val)
								; en caso contrario se retorna lo que habia en el vector del ambiente
								val))
						;; llamado recursivo con el ambiente anterior si la posicion no es un numero
						(apply-env env sym))))
			; Caso ambiente recursivo extendido
			(recursively-ext-env-record (proc-names proc-ids proc-bodies old-env)
				; Busca la posicion del procedimiento en la lista
				(let ((pos (list-find-position sym proc-names)))
					(if (number? pos)
						; Construye la clausura con del procedimiento encontrado
						(clausura (list-ref proc-ids pos) (list-ref proc-bodies pos) env)
						; Llamado recursivo con el ambiente anterior
						(apply-env sym old-env)))))))
;; Asigna valor al simbolo para (set-exp) con el ambiente
(define apply-env-for-set!
	(lambda (env sym n-val)
		(cases environment env
		; Caso ambiente vacio
		(empty-env-record () 
			; Error cuando el ambiente es vacio
			(eopl:error 'apply-env "No binding for ~s" sym))
		; Caso ambiente extendido
		(extended-env-record (syms vals env)
			; Busca la posicion del simbolo en la lista
			(let ((pos (rib-find-position sym syms)))
				(if (number? pos)
					; Busca el valor del simbolo en la posicion
					(let ((val (vector-ref vals pos)))
						;; si el valor que corresponde a la posicion encontrada es una celda se procede a modificar el valor
						(if (box? val) (set-box! val n-val)
							;; en caso contrario se lanza un error
							(eopl:error 'set "No se pueden asignar valores a ~s" sym)))
					;; llamado recursivo con el ambiente padre
					(apply-env-for-set! env sym n-val))))
		; No aplica para el Caso ambiente recursivo extendido
		(else (eopl:error 'set "No es posible asignar para: ~s" sym )))))
;; Encontrar la posición de un símbolo en la lista de símbolos de un ambiente
(define rib-find-position 
	(lambda (sym los)
	(list-find-position sym los)))
;;
(define list-find-position
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
;; Evaluar Expresion
;; Evalua la expresion en el ambiente de entrada
;;
;******************************************************************************************
;;
(define evaluar-expresion
	(lambda (exp env)
		(cases expresion exp
                  (bool-exp (args) (evaluar-bool-exp args env))
                  ;; CONSTANTES
                  (id-exp (identificador) (apply-env env identificador))
                  (num-exp (numero) numero)
                  (caracter-exp (caracter) caracter)
                  (cadena-exp (cadena)
                              (let ((end (- (string-length cadena) 1)))
                                (substring cadena 1 end)))
                  (ok-exp () "ok")
                  ;; OBJETOS
                  (obj-exp (ids exps)
                           ;; se crea un estructura de tipo object
                           (un-objeto ids (list->vector exps)))
                  (meth-exp (id args-ids body-exp)
                            ;id
                            ;args-ids
                            ;body-exp
                            ;(append (list id) args-ids)
                            ;; se crea una estructura de tipo method
                            (un-metodo args-ids body-exp)
                            ;;(un-metodo (append (list id) args-ids) body-exp)
                            )
                  (send-exp (obj-id meth-id args)
                            ;obj-id
                            ;meth-id
                            ;(car args)
                            ;env
                            ;(evaluar-rands (car args) env)
				(let ((args (evaluar-rands args env))
					;; se evaluan en el ambiente el objeto y los argumentos de entrada del metodo
					(obj (apply-env env obj-id)))
					;; y se llama a la funcion aplicar-metodo con los resultados anteriores y el id del metodo
					(aplicar-metodo obj meth-id args env))
                  )
                  (clone-exp (id args) (let ((obj (apply-env env id)))
				;; comprobamos que lo que se retorno del ambiente sea un objeto y si lo es se retorna
				(if (objeto? obj) obj
					;; en caso contrario se lanza un error
					(eopl:error 'clone "Esto no es un objeto -> ~s" id))))
                  (update-exp (obj-id field-id new-val)
				(let ((obj (apply-env env obj-id)))
					;; se hace el llamado a actualizar-atributo con el objeto, el id del campo y el nuevo valor
					(actualizar-atributo obj field-id new-val)))
                  (get-exp (obj-id sym)
				;; se liga el resultado de aplicar el ambiente a obj-id
				(let ((obj (apply-env env obj-id)))
					;; se hace el llamado a valor-atributo 
					(valor-atributo sym obj env)))
                  ;; ESTRUCTURAS DE CONTROL
                  (var-exp (ids ids-xps body-exp)
				;;id-vals recibe las cajas con los ids-xps y el ambiente
				(let ((id-vals (crear-boxes ids-xps env)))
					;;Evalúa el body con el ambiente extendido de los ids y sus valores
					(evaluar-expresion body-exp (extend-env ids id-vals env))))
                  (let-exp (ids ids-xps body-exp)
				;;id-vals recibe los valores de las expresiones ya evaluadas
				(let ((id-vals (evaluar-rands ids-xps env)))
					;;se evalua el cuerpo en el ambiente extendido con los ids, los id-values y un ambiente
					(evaluar-expresion body-exp (extend-env ids id-vals env))))
                  (letrec-exp (proc-names proc-ids proc-bodies letrec-body)
				;;se evalua el cuerpo de la expresion en un ambiente extendido recursivamente con los procedimientos definidos
				(evaluar-expresion letrec-body (extend-env-recursively proc-names proc-ids proc-bodies env)))
                  (set-exp (id exp) 
				;;se llama a una funcion que aplica un ambiente al id y la expresion exp ya evaluada
				(apply-env-for-set! env id (evaluar-expresion exp env)))
                  ;;se usa una funcion que evalua las expresiones del begin
                  (begin-exp (exp1 exp2) (evaluar-begin exp1 exp2 env))
                  (proc-exp (ids body) (clausura ids body env))
                  (cond-exp (test-exp result-exp test-exps result-exps else-result)
				(if (evaluar-bool-exp test-exp env)(evaluar-expresion result-exp env)
					(evaluar-rest-exps test-exps result-exps else-result env)))
                  (for-exp (id ini-val final-val exp)
				(let ((ini-id (box (evaluar-expresion ini-val env))) (fin-id (evaluar-expresion final-val env)))
					(evaluar-for-exp id fin-id exp (extend-env (list id) (list ini-id) env))))
                  (primapp-exp (prim args)
				(let ((args (evaluar-rands args env)))
					(aplicar-primitiva prim args)))
                  (apply-exp (id args)
				;;se aplica el ambiente para conocer el valor del id y se liga a proced
				(let ((proced (apply-env env id))
					;;se evaluan los argumentos y se ligan a args
					(args (evaluar-rands args env)))
					;;si proced es un procedimiento, se usa la funcion para aplicar un proc con proced y args
					(if (procval? proced) (aplicar-procedimiento proced args)
						;;en el caso contrario se muestra un error
						(eopl:error 'evaluar-expresion "Esto no es un procedimiento -> ~s" id))))
		)))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Funciones Auxiliares para Evaluar Expresion
;;
;******************************************************************************************
;;
;; Aplica la funcion evaluar-expresion a una lista de expresiones
(define evaluar-rands
	(lambda (exps env)
		(map
			(lambda (exp)
				;; Se evalua cada expresion de la lista exps
				;; Si la exp es un objeto, se devuelve la misma expresion
				(if (objeto? exp) exp
					;; En caso contrario se llama a evaluar-expresion para que evalue la expresion
					(evaluar-expresion exp env)))
				exps))
)

;; Determina el resultado de aplicar una primitiva (operaciones aritmeticas) a un conjunto de expresions
(define aplicar-primitiva
	(lambda (prim args)
		;; usando el cases de primitiva, evalua el tipo
		(cases primitiva prim
			;; puede sumar los argumentos para prim-sum
			(prim-suma ()
                                   (if (null? args)
                                       0
                                       (+ (car args) (aplicar-primitiva prim (cdr args)))))
			;; puede restar los argumentos para prim-resta
			(prim-resta () (if (null? args)
                                           0
                                           (- (car args) (aplicar-primitiva prim (cdr args)))))
			;; puede multiplicar los argumentos para prim-mult
			(prim-multiplicacion () (if (null? args)
                                                    1
                                                    (* (car args) (aplicar-primitiva prim (cdr args)))))
			;; puede dividir los argumentos para prim-division
			(prim-division () (if (null? args)
                                              1
                                              (/ (car args) (aplicar-primitiva prim (cdr args)))))
			;; puede calcular el modulo entre los argumentos para prim-modulo
			(prim-modulo () (modulo (car args) (cadr args)))
			;; o puede concatenar dos cadenas para prim-amper
			(prim-ampersand () (if (null? args)
                                                   ""
                                                   (if (string? (car args))
                                                       (string-append (car args) (aplicar-primitiva prim (cdr args)))
                                                       (eopl:error 'aplicar-primitiva "Los argumentos deben ser tipo string -> ~s" args)))))))
;; Evalua las expresions definidas con elseif
(define evaluar-rest-exps
	(lambda (tests-exps result-exps else-result env)
		;; si no existen expresiones definidas con elseif se  evalua la expresion definida en el else
		(if (and (null? tests-exps)(null? result-exps)) (evaluar-expresion else-result env)
			;; en caso contrario se evalua la primera expresion de la lista con su respectiva expresion de respuesta
			(if (evaluar-bool-exp (car tests-exps) env) (evaluar-expresion(car result-exps) env)
				;; y se hace el llamado recursivo con el resto de expresiones
				(evaluar-rest-exps (cdr tests-exps) (cdr result-exps) else-result env)))))
;; Determinar el valor de verdad de las expresiones bool-expresion
(define evaluar-bool-exp
	(lambda (test-exp env)
		(cases bool-expresion test-exp
			(true-exp() #t)
			(false-exp() #f)
			(aplicar-bool (oper args)
				;; se evaluan los argumentos
				(let ((args (evaluar-rands args env)))
					;; y se llama a la funcion auxiliar que evaluara la expresion y retorna su valor de verdad
					(aplicar-prim-bool oper args)))
			(oper-bool (oper args)
				;; llamado a la funcion auxiliar con el operador, los argumentos y el ambiente actual
				(aplicar-bool-oper oper args env)))))
;; Evalua la aplicacion de una prim-bool
(define aplicar-prim-bool
	(lambda (oper args)
		(cases bool-primitiva oper
			; si el primer argumento es menor que el segundo, verdadero, sino falso
			(prim-menor() (if (< (car args)(cadr args)) #t #f))
			; si el primer argumento es mayor que el segundo, verdadero, sino falso
			(prim-mayor() (if (> (car args)(cadr args)) #t #f))
			; si el primer argumento es menor o igual que el segundo, verdadero, sino falso
			(prim-menor-igual() (if (<= (car args)(cadr args)) #t #f))
			; si el primer argumento es mayor o igual que el segundo, verdadero, sino falso
			(prim-mayor-igual() (if (>= (car args)(cadr args)) #t #f))
			; si el primer y segundo argumentos son iguales, verdadero, sino falso
			(prim-igual-igual() (if (equal? (car args)(cadr args)) #t #f)))))
;; Evalua las expresiones en las que se usa un operador logico (and, or, not)
(define aplicar-bool-oper
	(lambda (oper args env)
		(cases bool-oper oper
			;; se aplica el operador not a el primer elemento evaluado de args
			(oper-not () (not (evaluar-bool-exp (car args) env)))
			;; se aplica el operador and a el primer elemento evaluado de args
			(oper-and () (and (evaluar-bool-exp (car args) env) (evaluar-bool-exp (cadr args) env)))
			;; se aplica el operador or a el primer elemento evaluado de args
			(oper-or () (or (evaluar-bool-exp (car args) env) (evaluar-bool-exp (cadr args) env))))))
;; Determina el resultado de las iteraciones de una expresion for y las pone en una lista
(define evaluar-for-exp
	(lambda (id final-value exp env)
		(cond
			;; si el valor ligado a id es mayor que el valor final especificado se retorna la lista vacia
			[(> (apply-env env id) final-value) '()]
			[else
				(begin
					;; en caso contrario se incrementa el valor de id en el ambiente
					(apply-env-for-set! env id (+ 1 (apply-env env id)))
					;; y se va construyendo la lista con la primera evaluacion de la expresion y los llamados recursivos 
					(cons (evaluar-expresion exp env) (evaluar-for-exp id final-value exp env))
                                        ;(evaluar-for-exp id (evaluar-expresion exp env) exp env)
                                        )])))
;; Determina el resultado de una begin-exp
(define evaluar-begin
	(lambda (exp exps env)
		(cond
			;; si la lista de expresiones es vacia solo se ejecuta la primera exp del begin
			[(null? exps) (evaluar-expresion exp env)]
			[else
				(begin
					;; en caso contrario se evaluan las expresiones dentro de un begin para que se retorne 
					(evaluar-expresion exp  env)
					;; el resultado de la ultima
					(evaluar-begin (car exps) (cdr exps) env))])))
;******************************************************************************************
;;
;; BOX
;;
;; Ejemplos:
;; > (define b (box "apple"))
;; > b
;; '#&"apple"
;; > (unbox b)
;; "apple"
;; > (set-box! b '(banana boat))
;; > b
;; '#&(banana boat)
;;
;******************************************************************************************
;;
;; Permite crear boxes para los valores de las expresiones evaluadas
(define crear-boxes 
	(lambda (exps env)
		;;vals recibe las expresiones evaluadas
		(let ((vals (evaluar-rands exps env)))
			(map
				(lambda (val) (box val))
					vals))))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Procedimientos
;;
;******************************************************************************************
;;
;; Definicion del tipo de dato para almacenar la informacion de un procedimiento
(define-datatype procval procval?
	;; una clausura (representacion de procedimientos) es:
	(clausura
		;; una lista de simbolos que son los argumentos de entrada del procedimiento
		(ids (list-of symbol?))
		;; una expresion que es el cuerpo del procedimiento (expresion a evaluar)
		(body expresion?)
		;; y el ambiente en el que fue ligado el procedimiento
		(env environment?)))
;; Aplicar el procedimiento de entrada a la lista de argumentos args
(define aplicar-procedimiento
	(lambda (procd args)
		(cases procval procd
			(clausura (ids body env)
				;; se evalua el cuerpo de el procedimiento en un ambiente extendido con los valores de los argumentos
				(evaluar-expresion body (extend-env ids args env))))))
;;
;******************************************************************************************


;******************************************************************************************
;;
;; Objetos
;;
;******************************************************************************************
;;
;; Definicion del tipo de dato para la representacion de un objeto en el lenguaje
(define-datatype objeto objeto?
	(un-objeto
		;; una lista de simbolos con los identificadores de sus campos 
		(fields (list-of symbol?))
		;; un vector de expresiones que corresponden a los campos del objeto 
		(exps vector?)))
;;
(define-datatype metodo metodo?
	(un-metodo
		;; lista de simbolos que representan los argumentos de entrada del metodo
		(ids (list-of symbol?))
		;; exp que constituye el cuerpo del metodo (exp a evaluar)
		(body expresion?)))
;; Determina el valor de un atributo de un objeto dado (para get-exp)
(define valor-atributo
	(lambda (sym obj env)
		(cases objeto obj
			(un-objeto (fields exps)
				;; buscamos la posicion del simbolo en la lista fields del objeto
				(let ((pos (rib-find-position sym fields)))
					;; si la posicion corresponde a un number
					(if (number? pos)
						(let ((value (vector-ref exps pos)))
							;; extraemos el valor correspondiente al simbolo y se retorna su evaluacion
							(evaluar-expresion value env))
							;; en caso contrario se lanza un error
							(eopl:error 'objeto "El campo ~s no ha sido definido" sym)))))))
;; Actualiza el valor de un atributo en un objeto (para update-exp)
(define actualizar-atributo
	(lambda (obj id new-value)
		(cases objeto obj
			(un-objeto (fields exps)
				;; buscamos la posicion del simbolo en el objeto
				(let ((pos (rib-find-position id fields)))
					;; si la posicion es un number 
					(if (number? pos)
						;; cambiamos el valor en la posicion del vector del objeto
						(vector-set! exps pos new-value)
						;; si no es, se lanza un error
						(eopl:error 'update "El campo ~s no ha sido definido" id)))))))
;; Determina el resultado de la aplicacion de un metodo de un objeto dado a una lista de argumentos
(define aplicar-metodo
	(lambda (obj meth args env)
		(cases objeto obj
			(un-objeto (fields exps)
				;; buscamos la posicion del metodo en el objeto
				(let ((pos (rib-find-position meth fields)))
					;; si la posicion encontrada corresponde a un number,
					(if (number? pos)
						;; evaluamos lo que corresponde a la posicion en el vector
						(let ((met (evaluar-expresion (vector-ref exps pos) env)))
							(if (metodo? met)
								(cases metodo met
									(un-metodo (ids body)
										;; comprobamos si el number de argumentos concuerda
										(if (equal? (length args) (length ids))
											;; y evaluamos el cuerpo en un ambiente extendido
											(evaluar-expresion body (extend-env ids args env))
											(eopl:error 'metodo "El numero de argumentos no corresponde al numero de ids"))))
							(eopl:error 'metodo "No corresponde a un metodo ~s" meth)))
					(eopl:error 'metodo "No se encuentra la posicion del metodo en el objeto")))))))
;;
;******************************************************************************************





;******************************************************************************************
;;
;; Interpretador OBLIQ
;;
;******************************************************************************************
;;
(interpretador)
;;
;******************************************************************************************





;******************************************************************************************
;;
;; Pruebas OBLIQ
;;
;******************************************************************************************
;;
;;
;; CONSTANTES
;;
;; true -> #t
;; false -> #f
;; var x = 1 in x end -> 1
;; 3 -> 3
;; 'c' -> 'c'
;; "cadena" -> "cadena"
;; ok -> "ok"
;;
;;
;; OPERADORES DE TEXTO, BOOLEANOS Y ENTEROS
;;
;; &("Una" "Dos" "Tres""Cuatro") -> "UnaDosTresCuatro"
;; not(false) -> #t
;; and(false, true) -> #f
;; or(false, true) -> #t
;; +(1 2 3) -> 6
;; -(1 2 3) -> 2
;; *(1 2 3) -> 6
;; /(1 2 3) -> 3/2
;; %(1 2) -> 1
;; <(1,2) -> #t
;; >(1,2) -> #f
;; <=(1,1) -> #t
;; >=(1,1) -> #t
;; is('c',"c") -> #f
;; is(3,3) -> #t
;; is("cadena", "cadena") -> #t
;;
;;
;; ESTRUCTURAS DE CONTROL
;;
;; var x = 2 in +(x 3) end -> 5
;; let y = 5 in *(y 2) end -> 3
;; letrec Fact(n) = if is(n, 0) then 1 else *(n apply Fact(-(n 1))) end in apply Fact(5) end -> 120
;; let funcion = proc(x, y) *(x y) end in apply funcion(2, 3) end -> 6
;;
;;
;; OBJETOS
;; 
;; let animal = object { patas => 4 ojos => 3 orejas => meth(s) +(get s.patas get s.ojos) end } in get animal.patas end -> 4
;; let animal = object { patas => 4 ojos => 3 orejas => meth(s) +(get s.patas get s.ojos) end } in var perro = clone(animal) in get perro.patas end end -> 4
;;
;;
;; SECUENCIACION
;;
;; begin var a = 2 in a end *(5 3); ok end -> "ok"
;;
;;
;; CONDICIONALES
;;
;; if <(2,8) then /(10 3) else *(10 3) end -> 10/3
;;
;;
;; ITERADORES
;;
;; let x = 1 in for i = 2 to 5 do +(x 1) end end -> (2 2 2 2)
;;
;;
;******************************************************************************************

