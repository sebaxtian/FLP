#lang eopl
;(require racket/base)
(require (only-in racket/base box))
(require (only-in racket/base box?))
(require (only-in racket/base unbox))
(require (only-in racket/base set-box!))

;;
;; Jhonny Melgarejo 1310251
;; Sebastian Rios 1310105
;;

;; La definición BNF para las expresiones del lenguaje Obliq:
;;
;;                  ::= <bool-expresion>
;;  <programa>       ::= <expresion>
;;                      <un-programa (exp)>
;;  <expresion>    ::= <numero>
;;                      <lit-exp (datum)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <caracter>
;;                      <caracter-exp>
;;                  ::= <cadena>
;;                      <cadena-exp>
;;                  ::= <ok>
;;                  ::= var {<identificador> = <expresion>}* (,) in <expresion> end
;;                      <var-exp>
;;                  ::= let {<identificador> = <expresion>}* (,) in <expresion> end
;;                      <let-exp>
;;                  ::= letrec {<identificador> ({<identificador>}* (,)) = <expresion>}* in <expresion> end
;;                      <letrec-exp>
;;                  ::= set <identificador> := <expresion>
;;                      <set-exp>
;;                  ::= begin <expresion> ; {<expresion>}*(;) end
;;                      <begin-exp>
;;                  ::= <primitiva> ({<expresion>}*)
;;                      <primapp-exp (prim rands)>
;;                  ::= if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion>}* else <expresion> end
;;                  ::= proc({<identificador>}*(,)) <expresion> end
;;                  ::= apply <identificador> ({<identificador>}*(,)) 
;;                  ::= meth <identificador> ({<identificador>}*(,)) <expresion> end
;;                  ::= for <identificador> = <expresion> to <expresion> do <expresion> end
;;                  ::= object  {<identificador> => <expresion>}* end
;;                  ::= get <identificador>.<identificador>
;;                  ::= send <identificador>.<identificador> ({<identificador>}*(,)) 
;;                  ::= update <identificador>.<identificador> := <expresion>  
;;                  ::= clone (<identificador>)
;; <bool-expresion> ::= <bool-primitiva>({<expresion>}*)
;;                  ::= <bool-oper>({<expresion>}*)
;;                  ::= true
;;                  ::= false
;; <primitiva>      ::= + | - | * | / | % | &
;; <bool-primitiva> ::= < | > | <= | >= | is
;; <bool-oper>      ::= not | and | or


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
    ;; Constantes
    (expresion (identificador) id-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) caracter-exp)
    (expresion (cadena) cadena-exp)
    (expresion ("ok") ok-exp)
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)
    ;; Definiciones
    (expresion
     ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)
    (expresion
     ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
    (expresion
     ("letrec" (arbno identificador "(" (separated-list identificador ",") ") =" expresion) "in" expresion "end") letrec-exp)
    (expresion
     ("set" identificador ":=" expresion) set-exp)
    (expresion
     ("begin" expresion ";" (separated-list expresion ";") "end") begin-exp) ;;DUDA ENVIADA POR CORREO, DEBE FINALIZAR EN END
    (expresion
     (primitiva "(" (arbno expresion ) ")") primapp-exp)
    ;; Condicional
    (expresion
     ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then"  expresion) "else" expresion "end") cond-exp)
    ;; Procedimiento
    (expresion 
	 ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion
     ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    ;; Iterador
    (expresion
     ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)
    ;; Metodos y Objetos
    (expresion
     ("meth"  "(" identificador "," (separated-list identificador ",") ")" expresion "end") meth-exp)
    (expresion
     ("object" (arbno identificador "=>" expresion) "end") obj-exp)
    (expresion
     ("get" identificador "." identificador) get-exp)
    (expresion
     ("send" identificador "." identificador "(" (separated-list expresion ",") ")") send-exp)
    (expresion
     ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion
     ("clone" "(" identificador ")") clone-exp)
    ;; Expresiones Booleanas
    (bool-expresion
     (bool-primitiva "(" (arbno expresion) ")") apply-bool)
    (bool-expresion
     (bool-oper "(" (arbno bool-expresion) ")") oper-bool)
    ;; Operadores
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
    (bool-primitiva ("is") prim-igual)
    (bool-oper ("not") oper-not)
    (bool-oper ("and") oper-and)
    (bool-oper ("or") oper-or)
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
			;; Expresiones Constantes
			(num-exp (numero) numero)
			(id-exp (identificador) (apply-env env identificador))
			(caracter-exp (caracter) caracter)
			(cadena-exp (cadena)
				(let ((end (- (string-length cadena) 1)))
                      (substring cadena 1 end)))
			(ok-exp () "ok")
			;; Expresiones Definiciones
			(primapp-exp (prim args)
				(let ((args (evaluar-rands args env)))
					(aplicar-primitiva prim args)))
			;; Expresiones Condicional
			(cond-exp (test-exp result-exp test-exps result-exps else-result)
				(if (evaluar-bool-exp test-exp env)(evaluar-expresion result-exp env)
					(evaluar-rest-exps test-exps result-exps else-result env)))
			;; Expresiones Iterador
			(for-exp (id ini-val final-val exp)
				(let ((ini-id (box (evaluar-expresion ini-val env))) (fin-id (evaluar-expresion final-val env)))
					(evaluar-for-exp id fin-id exp (extend-env (list id) (list ini-id) env))))
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
			;; Expresiones Procedimiento
			(proc-exp (ids body) (clausura ids body env))
			(apply-exp (id args)
				;;se aplica el ambiente para conocer el valor del id y se liga a proced
				(let ((proced (apply-env env id))
					;;se evaluan los argumentos y se ligan a args
					(args (evaluar-rands args env)))
					;;si proced es un procedimiento, se usa la funcion para aplicar un proc con proced y args
					(if (procval? proced) (aplicar-procedimiento proced args)
						;;en el caso contrario se muestra un error
						(eopl:error 'evaluar-expresion "Esto no es un procedimiento -> ~s" id))))
			(set-exp (id exp) 
				;;se llama a una funcion que aplica un ambiente al id y la expresion exp ya evaluada
				(apply-env-for-set! env id (evaluar-expresion exp env)))
			;;se usa una funcion que evalua las expresiones del begin
			(begin-exp (exp1 exp2) (evaluar-begin exp1 exp2 env))
			(letrec-exp (proc-names proc-ids proc-bodies letrec-body)
				;;se evalua el cuerpo de la expresion en un ambiente extendido recursivamente con los procedimientos definidos
				(evaluar-expresion letrec-body (extend-env-recursively proc-names proc-ids proc-bodies env)))
			;; Expresiones Metodos y Objetos
			(meth-exp (id args-ids body-exp)
				;; se crea una estructura de tipo method
				(un-metodo args-ids body-exp))
			(obj-exp (ids exps)
				;; se crea un estructura de tipo object 
				(un-objeto ids (list->vector exps)))
			(get-exp (obj-id sym)
				;; se liga el resultado de aplicar el ambiente a obj-id
				(let ((obj (apply-env env obj-id)))
					;; se hace el llamado a valor-atributo 
					(valor-atributo sym obj env)))
			(update-exp (obj-id field-id new-val)
				(let ((obj (apply-env env obj-id)))
					;; se hace el llamado a actualizar-atributo con el objeto, el id del campo y el nuevo valor
					(actualizar-atributo obj field-id new-val)))
			(send-exp (obj-id meth-id args)
				(let ((args (evaluar-rands args env))
					;; se evaluan en el ambiente el objeto y los argumentos de entrada del metodo
					(obj (apply-env env obj-id)))
					;; y se llama a la funcion aplicar-metodo con los resultados anteriores y el id del metodo
					(aplicar-metodo obj meth-id args env)))
			(clone-exp (id) (let ((obj (apply-env env id)))
				;; comprobamos que lo que se retorno del ambiente sea un objeto y si lo es se retorna
				(if (objeto? obj) obj
					;; en caso contrario se lanza un error
					(eopl:error 'clone "Esto no es un objeto -> ~s" id))))
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
				exps)))
;; Determina el resultado de aplicar una primitiva (operaciones aritmeticas) a un conjunto de expresions
(define aplicar-primitiva
	(lambda (prim args)
		;; usando el cases de primitiva, evalua el tipo
		(cases primitiva prim
			;; puede sumar los argumentos para prim-sum
			(prim-suma () (+ (car args) (cadr args)))
			;; puede restar los argumentos para prim-resta
			(prim-resta () (- (car args) (cadr args)))
			;; puede multiplicar los argumentos para prim-mult
			(prim-multiplicacion () (* (car args) (cadr args)))
			;; puede dividir los argumentos para prim-division
			(prim-division () (/ (car args) (cadr args)))
			;; puede calcular el modulo entre los argumentos para prim-modulo
			(prim-modulo () (modulo (car args) (cadr args)))
			;; o puede concatenar dos cadenas para prim-amper
			(prim-ampersand () (string-append (car args) (cadr args))))))
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
			(apply-bool (oper args)
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
			(prim-igual() (if (equal? (car args)(cadr args)) #t #f)))))
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
					(cons (evaluar-expresion exp env) (evaluar-for-exp id final-value exp env)))])))
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
#|Función que crea una caja (BOX) con el valor|#
(define crear-boxes 
	(lambda (exps env)
		;;vals recibe las expresiones evaluadas
		(let ((vals (evaluar-rands exps env)))
			(map
				;;make-a-box recibe cada valor de vals
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
							(eopl:error 'objeto "Field ~s has not been defined" sym)))))))
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
;; Determinar el resultado de la aplicacion de un metodo de un objeto dado a una lista de argumentos
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
											(eopl:error 'metodo "falló :("))))
							(eopl:error 'metodo "No es un método ~s" meth)))
					(eopl:error 'metodo "¿ajá y tu que?")))))))
;;
;******************************************************************************************








#|==========================================Intérprete Obliq==========================================|#
(interpretador)







#|==========================================Pruebas Unitarias Obliq===================================|#
#|

letrec Fact (n) = if is (n 0) then 1 else *(n apply Fact(-(n 1))) end in apply Fact (5) end
letrec fact(n) = if is(n 0) then 1 else *(n apply fact(-(n 1))) end in apply fact(5) end

->  <caracter>
      'a'
->  <cadena>
      "cadenastring"
->  <ok>
      ok
->  var {<identificador> = <expresion>}* (,) in <expresion> end
      var f=10, l=20, p=30 in *(+(f l) p) end
->  let {<identificador> = <expresion>}* (,) <expresion> end
      let f=10, l=50, p=4 in *(-(f l) p) end
->  letrec {<identificador> ({<identificador>}* (,)) = <expresion>}* <expresion> end
      letrec fact(n) = if is(n 0) then 1 else *(n apply fact(-(n 1))) end in apply fact(5) end
->  set&begin 
      var x=10 in begin set x:=20; x end end
->  <primitiva> ({<expresion>}*)
      +(-(40 30) *(2 2) /(10 2) %(5 2))
      &("aacsca" "bcasca")

->  if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion>}* else <expresion> end
      var x=10 in if is(10 0) then 100 else 200 end end

->  proc & apply
      let sumaResta=proc(x,y,z) +(-(x y) z) end in apply sumaResta(20,10,50) end
      
->  object & meth & get <identificador> ({<identificador>}*(,)) <expresion> end
      let o=object atr1 => 10 atr2 => 50 atr3 => meth(s) +(get s.atr1 get s.atr2) end end in get o.atr1 end

->  for <identificador> = <expresion> to <expresion> do <expresion> end
      let x=20 in for incr=0 to x do begin "a"; set incr:=+(incr 1); incr end end

->  send <identificador>.<identificador> ({<identificador>}*(,))

->  update <identificador>.<identificador> := <expresion>
       let o=object atr1 => 10 atr2 => 50 atr3 => meth(s) +(get s.atr1 get s.atr2) end end in let a=get o.atr1 in begin update o.atr1:=100; +(a get o.atr1) end end end
->  clone (<identificador>))
       let o=object atr1 => 10 atr2 => 50 atr3 => meth(s) +(get s.atr1 get s.atr2) end end in var x=clone(o) in get x.atr1 end end
|#
