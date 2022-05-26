#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))


; 10 points
(define -- (lambda args (list 'let args)))


; 10 points
(define @ (lambda (parsed_binding expr)
    (if (null? parsed_binding) expr (cons (car parsed_binding) (@ (cdr parsed_binding) expr)))))


; 20 points

(define split_at_delim (lambda (delim args) 
    (if (null? args) '()
        (if (equal? (car args) delim)
            (list (split_at_delim delim (cdr args)))
            (cons (car args) (split_at_delim delim (cdr args)))
        )
    )
))

(define (split-by args delim)
  (foldr (lambda (element next)
           (if (eqv? element delim)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) args))

; 30 points

(define split-and-prefix 
    (lambda (expr operand) (cons operand (split-by expr operand)))
)

(define parse_binding_list (lambda (binding_list)
    
    (let* ((splitted (split-by binding_list '--)))
        (map parse_assignment splitted)
    )
    
))

(define parse_assignment (lambda (assignment)
    (list ':= (cadr (car assignment)) 
        (if (number? (caddr assignment)) 
            (caddr assignment) 
            (cadr(caddr assignment))
        )
    )
))


(define parse_1binding_list (lambda (binding_list)
    
    (-- (:= (cadr (car binding_list)) (caddr binding_list)))
    
))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define parse_expr (lambda (expr) 
    (if (null? expr) '() 

    (cond ((member '+ expr) (list '+  parse_expr))
        ((member '* expr) (parse_expr (split-and-prefix expr '*)))
        ((member '@ expr) (list))
        ((number? (car expr)) (car expr)) 
        ((list? (car expr) ) (parse_expr (car expr)))
        ((atom? (car expr)) (car expr))

        (else (list 1))
    )

    )

))
; 20 points
(define eval_expr (lambda (expr) 0))
