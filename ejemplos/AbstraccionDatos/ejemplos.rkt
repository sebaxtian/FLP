;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ejemplos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

;;uso de lambda
(define caso1 (lambda (x y) (+ x y)))
(define caso2 (lambda (x y z w) (and (>= x y) (>= z w))))


;;como valor
(define caso3 ((lambda (x) x) 10))

(define caso4 ((lambda (y) (+ 2 y)) 10))

;;Definición recursiva para multiplos de 3
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))
;;Definición recursiva para numero multiplo de 5

(define in-SM5?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 5) 0)
            (in-SM5? (- n 5))
            #f))))