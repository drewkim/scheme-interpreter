(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) 
    nil
    (cons (proc (car items)) (map proc (cdr items)))
    )
  )

(define (cons-all first rests)
  (cond
    ((null? (cdr rests)) (cons (cons first (car rests)) nil))
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
    )
  )

(define (zip pairs)
    (define (firsts p)
        (if (null? p)
            nil
            (cons (car (car p))(firsts (cdr p)))
        )
    )
    (define (seconds p)
        (if (null? p)
            nil
            (cons (car (cdr (car p))) (seconds (cdr p)))
        )
    )
    (cons (firsts pairs) (cons (seconds pairs) nil))
)

;; Problem 17

(define (len s)
  (if (null? s)
    0
    (+ 1 (len (cdr s)))
    )
  )
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define x (len s))
  (define (inner s)
    (if (null? s)
      nil
      (cons (cons (- x (len s)) (cons (car s) nil)) (inner (cdr s)))
      )
    )
  (inner s)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond 
      ((null? denoms) nil)
      ((= 0 total) (cons nil nil))
      ((< total (car denoms)) (list-change total (cdr denoms)))
      (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms)))))))

  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
        ((atom? expr) expr)
        ((quoted? expr) expr)
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) (map let-to-lambda (Cadr (zip values))))
           ))
        (else
         (cons (car expr) (map let-to-lambda (cdr expr)))
         )))
