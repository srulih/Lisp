#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


(define (make-frame variables values)
  (if (null? variables)
       '()
       (cons
          (cons (car variables) (car values))
          (make-frame (cdr variables) (cdr values))))
)

(define (frame-variables frame)
  (if (null? frame)
      '()
      (cons
       (caar frame) (frame-variables (cdr frame))))
  )

(define (frame-values frame)
  (if (null? frame)
      '()
      (cons
       (cdar frame) (frame-values (cdr frame))))
  )


(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val))
  )
   
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
       (error "Too many arguments supplied" vars vals)
       (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
       (cond ((null? frame) (env-loop (enclosing-environment env)))
             ((eq? var (caar frame)) (cdar frame))
             (else (scan (cdr frame)))))
   (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan frame))))
   (env-loop env))

(define (set-variable-value! var val env)
   (define (env-loop env)
      (define (scan frame)
         (cond ((null? frame) (env-loop (enclosing-environment env)))
               ((eq? var (caar frame)) (set-car! frame (cons var val)))
               (else (scan (cdr frame)))))
      (if (eq? env the-empty-environment)
         (error "Unbound variable: SET!" var)
         (let ((frame (first-frame env)))
              (scan frame))))
(env-loop env))

(define (define-variable! var val env)
   (let ((frame (first-frame env)))
       (define (scan f)
          (cond ((null? f) (add-binding-to-frame! var val frame))
                ((eq? var (caar f)) (set-car! frame (cons var val)))
                (else (scan (cdr f)))))
       (scan frame)))



;; examples

(define env the-empty-environment)

(define env-first (extend-environment '(a b c) '(2 3 4) env))

(display env)
(newline)
(define var (lookup-variable-value 'a env-first))

(display var)
(newline)
(define-variable! 'd 2 env-first)

(display env-first)
(newline)

(set-variable-value! 'c 55 env-first)

(define val (lookup-variable-value 'c env-first))

(display val)
(newline)
