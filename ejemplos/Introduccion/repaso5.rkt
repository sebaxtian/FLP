;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repaso5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; EJEMPLO - Funciones
;;
;; Funciones como Ciudadanos de Primera Clase
;;

;;
;; Las Funciones pueden ingresar Como Parametros
;;
(define (operador a b f)
  (f a b))
;; Pruebas
"operador"
(operador 1 2 +)
(operador 2 3 -)
(operador 4 5 /)
(operador 2 2 *)

;;
;; Las Funciones pueden Retornar Funciones
;;
(define (funcionLoca a b)
  (lambda (x y) (+ x y a b)))
;; Pruebas
"funcionLoca"
(funcionLoca 1 2)
((funcionLoca 1 2) 3 4)
