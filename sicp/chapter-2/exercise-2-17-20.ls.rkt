#lang sicp

(define (print o)
  (display o)
  (newline))

(define (test-func f p l)
  (if (equal? (apply f p) l)
	(print #t)
	(print #f)))

; exercise 2-17

(define (last-pair l)
  (if (null? (cdr l))
	l
	(last-pair (cdr l))))

; test
(print "exercise 2-17 tests")


(define l (last-pair (list 1 2 3 4)))

(define ok (equal? (list 4) l))
(print ok)

(define l2 (last-pair (list 8)))

(define ok2 (equal? (list 8) l2))
(print ok2)

; exercise 2-18

(define (reverse l)
  (define (rev-iter l il)
	(if (null? l)
	  il
	  (rev-iter (cdr l) (cons (car l) il))))
  (rev-iter l '()))

; test
(newline)
(print "exercise 2-18 tests")

(test-func reverse (list (list 1 2 3)) (list 3 2 1))
(test-func reverse (list (list 1)) (list 1))
(test-func reverse (list '()) '())


; exercise 2-20

(define (same-parity . l)
  (let ((p (modulo (car l) 2)))
	(define (sp il)
	  (cond ((null? il) '()) 
			((= (modulo (car il) 2) p)
			 (cons (car il) (sp (cdr il))))
			(else (sp (cdr il)))))
  	(sp l)))

; test
(newline)
(print "exercise 2-20 tests")

(test-func same-parity (list 1 2 3 4 5) (list 1 3 5))
(test-func same-parity (list 4 6 5 3 2) (list 4 6 2))

; exercise 2-21


(define (square-list items)
  (if (null? items)
	nil
	(cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

; test
(newline)
(print "exercise 2-21 tests")

(test-func square-list (list (list 1 2 3)) (list 1 4 9))
(test-func square-list2 (list (list 1 2 3)) (list 1 4 9))

; exercise 2-22

(define (for-each2 f items)
  (if (null? items)
	#t
	(begin (f (car items)) (for-each2 f (cdr items)))))

; test

; expect this to print all the numbers in the list

(newline)
(print "exercise 2-22 tests")
(for-each2 (lambda (x) (print x)) (list 1 2 4 5))
