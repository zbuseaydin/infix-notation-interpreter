#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))


; 10 points
(define -- (lambda args (list 'let args)))

(define modified-- (lambda lst (cons 'let lst)))


; 10 points
(define @ (lambda (parsed_binding expr)
    (if (null? parsed_binding) expr (cons (car parsed_binding) (@ (cdr parsed_binding) expr)))))


; 20 points

(define m_split_at_delim (lambda (delim args) 
    (foldr (lambda (e n)
           (if (eqv? e delim)
               (cons '() n)
               (cons (cons e (car n)) (cdr n))))
         (list '()) args)
))


(define split_at_delim (lambda (delim args)
    (if (member? delim args) (let ((index (index-of args delim)))
        (cons (take args index)
            (split_at_delim delim (drop args (+ index 1)) ))
    )(list args))            
))

; 30 points

(define parse_binding_list (lambda (binding_list)
    
    (let* ((splitted (split_at_delim '-- binding_list)))
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
        
            (if (member? '+ expr) (cons '+ (map parse_expr (split_at_delim '+ expr)))
                (if (member? '* expr) (cons '* (map parse_expr (split_at_delim '* expr)))
                    (if (member? '@ expr) 
                    (let ((splitted_assign (split_at_delim '@ expr)))
                        (@ (modified-- (parse_binding_list (caar splitted_assign))) (list (parse_expr (cadr splitted_assign)))))
                         (if (list? (car expr) ) (parse_expr (car expr))
                        (if (number? (car expr)) (car expr) 
                            (if (atom? (car expr)) (car expr)
                            '() 
                            )
                        )
                    )
                )
            )
        )
        (if (number? expr) (list expr) 
                            (if (atom? expr) (list expr)
                            '() 
                            ))
    )
    )
))

        
; 20 points
(define eval_expr (lambda (expr) 0))
