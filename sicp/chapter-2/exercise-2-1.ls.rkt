#lang sicp

(define (gcd a b)
  (if (= b 0)
	a
	(gcd b (modulo a b)))
  )

(define (make-rat n d)
  (let ((g (gcd n d)))
	(cons (/ n g) (/ d g))))

; check if a rational number is positive
(define (is-positive n d)
  (cond ((and (> n 0) (> d 0)) #t)
		((and (< n 0) (< d 0)) #t)
		(else #f))
  )

(define (make-rat2 n d)
  (if (is-positive n d)
	(make-rat (abs n) (abs d))
	(make-rat (* (abs n) -1) (abs d)))
  )

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define one-half (make-rat2 1 -2))

(define one-third (make-rat2 1 3))
(define two-third (make-rat2 2 3))

(display (add-rat one-half one-third))
