;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repaso3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; EJEMPLO - Condicionales
;;
;; Los condicionales tiene la siguiente estructura
;;

;;
;; (cond
;;    [<Pregunta> <Respuesta>]
;;    [<Pregunta> <Respuesta>]
;;    ...
;;    [else <Respuesta>]
;; )
;;

;;
;; Ejemplo de una funcion que recibe un numero y verifica
;; si ese numero es par.
;;
(define (verificarpar n)
  (cond
    [(even? n) #true]
    [else #false]))
;; Pruebas
(verificarpar 3)
(verificarpar 8)
(verificarpar 345)

;;
;; ¿Que hace esta funcion?
;;
(define (funcionRara n)
  (cond
    [(odd? (- n 1))
     (cond
       [(< n 10) (* 2 n)]
       [(> n 15) (* 3 n)]
       [else n])]
    [(> n 115) (* 4 n)]
    [else (* 5 n)]))
;; Pruebas
(funcionRara 3)
(funcionRara 2)
(funcionRara 18)
(funcionRara 7)
(funcionRara 59)
(funcionRara 10)
(funcionRara 29)
