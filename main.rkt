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
    (lambda (expr operand) (list operand (split-by expr operand)))
)

(define parse_binding_list (lambda (binding_list)
    
    (if (null? binding_list) ()
    (if (member '-- binding_list)
        ((let assignment (split-by binding_list '--) ) (let var (split-by assignment ':=)))
            (-- (:= (car assignment) (cdr assignment)) ) )
        (cons parse_binding_list (car binding_list) (cdr binding_list)))

)) 

(define parse_expr (lambda (expr) 

    (cond (member '+ expr) (parse_expr (split-and-prefix expr '+))
        (member '* expr) (parse_expr (split-and-prefix expr '*))
        (member '@ expr) '()
        (number? expr) (expr) 
        (atom? expr) (expr) 

        (else expr)
    )



))
; 20 points
(define eval_expr (lambda (expr) 0))
