(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
    (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
        (cond
           ((null? pairs) (append (cons nil nil) (cons nil nil)))
           ((not (null? (filter (lambda(x) (null? x)) pairs))) nil)
           (else (cons (map (lambda(x) (car x)) pairs) (zip (map (lambda(x) (cdr x)) pairs))))
          )
)


;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s i)
     (if (equal? s nil)
         nil
         (cons(cons i(cons (car s) nil)) (helper (cdr s) (+ i 1)))
      )
  )
  (helper s 0)
)

  ; END PROBLEM 17
(define (remove-redundancy old-list)
    (define (helper old-list new-list)
        (cond ((null? old-list) new-list)
              ((null? (filter (lambda(x) (equal? x (car old-list))) new-list)) (helper (cdr old-list) (append new-list (cons (car old-list) nil))))
              (else (helper (cdr old-list) new-list)))
       )
       (helper old-list nil)
     )

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (cond
      ((null? denoms) nil)
      ((= (car denoms) total)  (cons (cons total nil) (list-change total (cdr denoms))))
      ((> (car denoms) total) (list-change total (cdr denoms)))
      (else (remove-redundancy (append (cons-all (car denoms) (append (list-change (- total (car denoms)) denoms) (list-change (- total (car denoms)) (cdr denoms)))) (list-change total (cdr denoms)))))
   )
)

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
           (append (list form params) (map  (lambda (x) (let-to-lambda x)) body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (if (not (list? values))
              (cons (car expr) (let-to-lambda (cdr expr)))
              (cons (list 'lambda (let-to-lambda (car (zip values))) (let-to-lambda (car body))) (let-to-lambda (cadr (zip values)))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
          (cons (let-to-lambda (car expr)) (let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         ))))
