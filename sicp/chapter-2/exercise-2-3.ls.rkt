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

; make a rectange from two points
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (rectangle-height r)
  (- (y-point (cdr r)) (y-point (car r))))

(define (rectangle-width r)
  (- (x-point (cdr r)) (x-point (car r))))

; define a rectangle using single point height and width
(define (make-rectangle2 p h w)
  (cons p (cons h w)))

(define (rectangle-height2 r)
  (cadr r))

(define (rectangle-width2 r)
  (cddr r))

(define (rectangle-perim2 r)
  (+ (* 2 (rectangle-width2 r)) (* 2 (rectangle-height2 r))))

(define (rectangle-area2 r)
  (* (rectangle-width2 r) (rectangle-height2 r)))

; test area

(define p1 (make-point 2 1))

(define rec (make-rectangle2 p1 4 7))

(define perim (rectangle-perim2 rec))

(define area (rectangle-area2 rec))

(display perim)
(newline)

(display area)
(newline)

