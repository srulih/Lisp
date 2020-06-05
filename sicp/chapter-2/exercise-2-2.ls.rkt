#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((st (start-segment s))
		(en (end-segment s)))
	(let ((stx (x-point st))
		  (sty (y-point st))
		  (enx (x-point en))
		  (eny (y-point en)))
	  (let ((mx (/ (+ stx enx) 2))
			(my (/ (+ sty eny) 2)))
		(make-point mx my)))))


; some sanity tests

(define point-1 (make-point 2 3))
(define point-2 (make-point 4 7))

(define seg (make-segment point-1 point-2))

(define mid-point (midpoint-segment seg))

(print-point mid-point)
