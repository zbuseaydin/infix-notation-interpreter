#lang racket

(provide (all-defined-out))

(define := (lambda (var value) (list var value)))


(define -- (lambda args (list 'let args)))


(define @ (lambda (parsed_binding expr)
    (if (null? parsed_binding) expr (cons (car parsed_binding) (@ (cdr parsed_binding) expr)))))


(define split_at_delim (lambda (delim args)
    (if (member? delim args) (let ((index (index-of args delim)))
        (cons (take args index)
            (split_at_delim delim (drop args (+ index 1)) ))
    )(list args))            
))


(define modified-- (lambda lst (cons 'let lst)))

; splits the binding list by -- and parses each of the bindings
(define parse_binding_list (lambda (binding_list)
    (let* ((splitted (split_at_delim '-- binding_list)))
        (map parse_assignment (reverse (cdr (reverse splitted))))
    )
))

(define parse_assignment (lambda (assignment)
    (:= (cadr (car assignment))  ; get the second element from the assignment 
        (if (number? (caddr assignment))  ; if the last element of the assignment is a number
            (caddr assignment)            ; return the number
            (cadr(caddr assignment))      ; else it is 'variable, so return the variable
        ))
    )
)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; checks if x is a member of list
(define member? (lambda (x list)
 (if (null? list) #f
    (if (eq? x (car list)) #t
        (member? x (cdr list))))))


(define parse_expr (lambda (expr) 
    (if (null? expr) '()  ; check if expr is null
        (if (list? expr)  ; else if expr is a list then do the corresponding parsing according to the expr type

            ; if expr+expr, return (+ parsed_expr1 parsed_expr2 ...)
            (if (member? '+ expr) (cons '+ (map parse_expr (split_at_delim '+ expr))) 

                ; else if expr*expr, return (+ parsed_expr1 parsed_expr2 ...)
                (if (member? '* expr) (cons '* (map parse_expr (split_at_delim '* expr))) 

                    ; else if (binding list)@(expr) then parse the binding list, apply modified--, parse expr and apply @ func to them
                    (if (member? '@ expr) (let ((splitted_assign (split_at_delim '@ expr)))
                            (@ (modified-- (parse_binding_list (caar splitted_assign))) (list (parse_expr (cadr splitted_assign)))))

                        ; else if (expr) return expr
                        (if (list? (car expr) ) (parse_expr (car expr))

                            ; else if number or variable return number or variable
                            (if (number? (car expr)) (car expr) 
                                (if (atom? (car expr)) (car expr)
                                '() 
                            ))
                        ))))
          
        '() ; else return empty list 
        ))
))

        
(define eval_expr (lambda (expr) 
    (eval (parse_expr expr))))
