;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repaso4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; EJEMPLO - Funciones
;;
;; Ejercicios de funciones
;;

;;
;; Desarrolle una funcion que calcule el area de un
;; cuadrado de lado L.
;;
(define (areaCuadrado l)
  (* l l))
;; Pruebas
"areaCuadrado"
(areaCuadrado 2)
(areaCuadrado 3)
(areaCuadrado 12)
(areaCuadrado 7)

;;
;; Desarrolle una funcion que determine si un numero
;; es impar o no.
;;
(define (espar n)
  (cond
    [(= (remainder n 2) 0) 'par]
    [else 'impar]))
;; Pruebas
"espar"
(espar 0)
(espar 2)
(espar 3)
(espar 7)
(espar 16)
(espar 21)

;;
;; Desarrolle una funcion que retorne "Eureka" si
;; la entrada es el numero 08323, si no debe
;; retornar "La policia te va atrapar".
;;
(define (lapoli numero)
  (cond
    [(equal? numero 08323) "Eureka"]
    [else "La policia te va atrapar"]))
;; Pruebas
"lapoli"
(lapoli 08323)
(lapoli 1234)

;;
;; Desarrolle una funcion que reciba un numero y
;; retorne la lista de los pares desde 0 hasta
;; ese numero.
;;
(define (listaPares n)
  (cond
    [(= n 0) (cons 0 empty)]
    [(equal? (espar n) 'par) (cons n (listaPares (- n 1)))]
    [else (listaPares (- n 1))]))
;; Pruebas
"listaPares"
(reverse (listaPares 10))
(reverse (listaPares 25))
(reverse (listaPares 30))
