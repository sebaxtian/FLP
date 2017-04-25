;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repaso2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; EJEMPLO - Preliminares
;;
;; Se usa la palabra reservada define para la definicion de variables.
;;

(define numeroA 5)
(define numeroB (* 2 numeroA))

;; Se definen las funciones de la siguiente forma
;;
;; (define (nombreFuncion argumentosEntrada) <operaciones>)
;;

(define (multiplique a b) (* a b))


;; Ejercicios
(define operacion1 (+ (* 2 2) (* 3 5) (expt (/ 1 4) 2)))
operacion1

(define operacion2 (+ (* 2 (+ 1 (expt 3 2) (/ 4 4))) (* 3 (- 5 3)) (- (/ 12 4) 3) (* 4 (expt 5 3))))
operacion2

(define operacion3 (+ (* 2 (+ 1 (/ 7 4))) (* (- 1 2) (- 5 3)) (- (/ (+ 1 34) 4) (/ 8 9)) (* 4 (expt 5 3))))
operacion3

(define operacion4 (/ (+ (* 2 (+ 1 (expt 3 2) (/ 7 4))) (* 3 (- 5 3)) (expt (+ (- (/ 16 4) 3) (* 4 5)) 2)) (+ (* 2 2) (* 3 5) (expt (/ 1 4) 2))))
operacion4