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
        (map parse_assignment (reverse (cdr (reverse splitted))))
    )
    
))

(define parse_assignment (lambda (assignment)
    (:= (cadr (car assignment)) 
        (if (number? (caddr assignment)) 
            (caddr assignment) 
            (cadr(caddr assignment))
        ))
    )
)


(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

 

(define member? (lambda (x list)
 (if (null? list) #f
    (if (eq? x (car list)) #t
        (member? x (cdr list))))))


(define parse_expr (lambda (expr) 
    
    (if (null? expr) '() 
    (if (list? expr)
        (let ((splitted_multiply (split-by expr '*))
            (splitted_addition (split-by expr '+)))
            (if (list? (car expr) ) (parse_expr (car expr))
            (if (member? '* expr) (list '* (parse_expr (car splitted_multiply)) (parse_expr (cdr splitted_multiply)))
                (if (member? '+ expr) (list '+ (parse_expr (car splitted_addition)) (parse_expr (cdr splitted_addition)))
                    (if (member? '@ expr) 
                    (let ((splitted_assign (split-by expr '@)))
                        (@ (-- (parse_binding_list (caar splitted_assign))) (parse_expr (cadr splitted_assign))))
                        (if (number? (car expr)) (list (car expr)) 
                            (if (atom? (car expr)) (list (car expr))
                            '() 
                            )
                        )
                    )
                )
            )
        ))
        (if (number? expr) (list expr) 
                            (if (atom? expr) (list expr)
                            '() 
                            ))
    )
    )
))

;(define parse_expr (lambda (expr) 
;    
;    (if (null? expr) '() 
;    (if (list? expr)
;        (if (list? (car expr) ) (parse_expr (car expr))
;            (if (member? '* expr) (list '* (parse_expr (car expr)) (parse_expr (cddr expr)))
;                (if (member? '+ expr) (list '+ (parse_expr (car expr)) (parse_expr (cddr expr)))
;                    (if (member? '@ expr) 
;                    (let ((splitted_assign (split-by expr '@)))
;                        (@ (-- (parse_binding_list (caar splitted_assign))) (parse_expr (cadr splitted_assign))))
;                        (if (number? (car expr)) (cons '() (car expr)) 
;                            (if (atom? (car expr)) (cons '() (car expr))
;                            '() 
;                            )
;                        )
;                    )
;                )
;            )
;        )
;        (if (number? expr) (list expr) 
;                            (if (atom? expr) (list expr)
;                            '() 
;                            ))
;    )
;    )
;))

; 20 points
(define eval_expr (lambda (expr) 0))
