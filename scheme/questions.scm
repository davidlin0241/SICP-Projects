
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (define (add_to_front lst)
    (append (cons first nil) lst)
  )
  (map add_to_front rests)
)

;map returns a well-formed list!!
;(list '(1)) is ((1))
;(append '() '(()) ) is still (())
; (()) is not null

(define (zip pairs)
  (cond
    ( (null? pairs) '(() ()) )
    ( (null? (car pairs)) '() )
    ( else (append (list (map car pairs)) (zip (map cdr pairs))))
  )
)

;(let ((a 1) (b 2)) (+ a b))
; (let-to-lambda '(let ((a 1) (b 2)) (+ a b)))
; ((lambda (a b) (+ a b)) 1 2)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s index)
    (cond
      ( (null? s) nil )
      (else (cons (cons index (cons (car s) nil)) (helper (cdr s) (+ 1 index)) ))
    )
   )
   (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
    (cond
      ( (= total 0) '(()) )
      ( (or (null? denoms) (< total 0)) '() )
      ( else (append
                (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                ( list-change total (cdr denoms) )
      ))))
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

; (let-to-lambda '(+ 1 2))
; Eval let-to-lambda: check that it exists.
; Eval '(+ 1 2) : it's (+ 1 2)
; Apply let-to-lambda on (+ 1 2): goes into else case because there's no quote there.

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list 'lambda params (let-to-lambda (car body))) (let-to-lambda (cdr body)))
           ; END PROBLEM 19
        ))
        ((let? expr)
         (let ((values (cadr expr))   ;((a 1) (b 2))
               (body   (cddr expr)))  ;((+ a b))
           ; BEGIN PROBLEM 19
              ( cons (list 'lambda (car (zip values)) (let-to-lambda (car body))) (let-to-lambda (cadr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)       ; how does + return itself?????? what about a (in one of the tests)???
         ; END PROBLEM 19
         )))
