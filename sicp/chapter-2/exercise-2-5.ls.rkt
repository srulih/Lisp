#lang sicp

;Exercise 2.5: Show that we can represent pairs of nonnegative integers 
;using only numbers and arithmetic operations if we represent the pair 
;a and b as the integer that is the product 2^a3^b.
;Give the corresponding definitions of the procedures cons, car, and cdr.

; find exponent of a prime in prime decomposition of a number

(define (prime-exp n p)
  (if (= 0 (modulo n p))
	(+ 1 (prime-exp (/ n p) p))
	0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (prime-exp p 2))

(define (cdr p)
  (prime-exp p 3))

; test

(define pair (cons 23 56))

(display (car pair))
(newline)

(display (cdr pair))
(newline)
