;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repaso1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; Como Diseñar Programas
;;
;; (https://goo.gl/rmmde3)
;; (https://goo.gl/TZHFG3)
;;
;;
;; Estos son los pasos basicos a seguir en el diseño de programas:
;;
;;
;; 1. Del analisis del problema a la definicion de datos
;;
;; Identificar la informacion que debe ser representada y como
;; se representa en el lenguaje de programacion escogido.
;; Formule las definiciones de los datos e ilustre con ejemplos.
;;
;;
;; 2. Declaracion del Proposito, Encabezado, Firma
;;
;; Exprese de forma concisa lo que la funcion o programa calcula.
;; Indique los datos que la funcion o programa deseada consume y
;; produce.
;; Defina una firma del autor de la funcion o programa.
;;
;;
;; 3. Ejemplos Funcionales
;;
;; Trabaje con ejemplos que ilustran el proposito de las funciones
;; o del programa.
;;
;;
;; 4. Plantilla de la Funcion
;;
;; Convierta las definiciones de datos en un esquema de funciones.
;;
;;
;; 5. Definicion de la Funcion
;;
;; Rellene los espacios en la plantilla de la funcion aprovechando
;; la declaracion del proposito y los ejemplos.
;;
;;
;; 6. Pruebas
;;
;; Articule los ejemplos como pruebas y asegure que las funciones
;; pasan todas las pruebas.
;; Hacer pruebas permite descubrir errores y tambien ayuda a otros
;; a entender la definicion del proposito de la funcion de
;; cualquier programa.
;;
;;


;;
;; EJEMPLO
;;
;; Contrato del Programa:
;;
;; Ejemplo de diseño de un programa que calcula el factorial
;; de un numero entero positivo que toma como entrada de la funcion.
;;
;; Por definicion el factorial de un numero entero positivo n
;; es la multiplicacion de todos los enteros positivos desde
;; 1 hasta el numero n.
;;
;; Ejemplos:
;;
;; 0! = 1
;; 1! = 1
;; 3! = 6
;; 5! = 120
;; 6! = 720
;;
;; Plantilla de la Funcion:
;;
;; factorial : input1 -> output
;;
;; [input1] -> Numero entero positivo
;; [output] -> Factorial del numero entero positivo
;;
;; Autor: Sebastian Rios Sabogal
;;

;; Definicion del Programa
(define (factorial n)
  (cond
    [(= n 0) 1]
    [else (* (factorial (- n 1)) n)]))

;; Pruebas
(factorial 0) "=" 1
(factorial 1) "=" 1
(factorial 3) "=" 6
(factorial 5) "=" 120
(factorial 6) "=" 720
