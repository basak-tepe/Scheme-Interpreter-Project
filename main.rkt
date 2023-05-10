;needed updates:
;dont print procedure for last 2 inputs of function 2.
;continue working from function 7. 
;work on evaluating function.
;revise and test 4 5 6.
;lambda needs rework in 9.
;symbol needs rewok in 9.


; Basak Tepe
; 2020400117
; compiling: no
; complete: no
#lang racket


(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here
; 1. empty-state (5 points)
;(define empty-state 0)
(define empty-state (hash))


; 2. get (5 points)
;(define get 0)

;state is a hash. 
(define (get state var)
    (let ((val (hash-ref state var #f))) ;#f is the default value if the key is not found
    (if (not (eq? val #f)) ;if the key is found, val is not false.
        val
    (eval var)))) ;else (if key not found) evaluate the key.



; 3. put (5 points)
;(define put 0)
(define (put state var val)
    (let ((newstate (hash-set state var val))) ;update the value or create a new key-value pair
    newstate));return the new state


; 9. eval-expr (20 points)
;This function takes an expression expr and a state state, and evaluates the expression with the
;given state. It puts the resulting value of the expression to the -r variable in the resulting state,
;and returns the new state. The expression can be a literal, a variable, a function call, a conditional
;expression, a while loop, or a function definition. You can assume that the expression is syntactically
;correct, and you do not need to check for errors. You can use map-eval function in this function.
;Hint: Given the expression,
;• If it is a list, you should check its first element to determine the operation. If the operation
;is one of :=, if:, while:, or func, you can basically append the state into the list and call
;eval function on the list.
;• If the operation is lambda, just eval the list and put the resulting value into the -r variable
;in the resulting state.
;• If the operation is a symbol (e.g., '+, '*, or some other symbol that), map-eval all of the
;elements in the list and apply the operation to the resulting values.
;• Else, if the operation is not a list, then it is either a literal or a variable. If it is a literal, just
;put the literal into the -r variable in the resulting state. If it is a variable, get the value of
;the variable from the state and put it into the -r variable in the resulting state.
;(define eval-expr 0)

;apperently cons appends and returns a new list so we have to use it within the eval call.
(define (eval-expr expr state)
    (if (list? expr) ;if it is a list
    ;we will go into a switch-like structure.
    (cond   ((eq? (car expr) ':=) 
                (eval-expr (cons ':= expr) state))            
            ((eq? (car expr) 'if:) 
                (eval-expr (cons 'if: expr) state))
            ((eq? (car expr) 'while:) 
                (eval-expr (cons 'while: expr) state))            
            ((eq? (car expr) 'func)
                    (eval-expr (cons 'func expr) state))
            ;lambda needs rework
            ;((eq? (car expr) 'lambda) 
                    ;(put state '-r (eval expr)))
            ;symbol needs rework
            ;((symbol? (car expr)) 
                ;skip the first element and evaluate the numbers. first element is the operator
                ;(put state '-r (apply (car expr) (map-eval (cdr expr) state)))))
                ;(map-eval expr state) ;map-eval all of the elements in the list and apply the operation to the resulting values.
                ;(put state '-r (eval expr)))             
    )
    
    ;else it is variable or literal
    (cond   ((number? expr) 
            (put state '-r expr)) ;a literal: put the number into -r.
            ((boolean? expr) 
            (put state '-r expr)) ;a literal:  put the boolean into -r.
            ((string? expr) 
            (put state '-r expr)) ;a literal:  put the string into -r.
            ((number? (get state expr)) ;a variable: if the variable with a value is found in the state     
            (put state '-r (get state expr))) ; a variable: put the var into -r
    ))
                state); return the state


; 4. := (15 points)
;This function takes a variable name var, a value expression val-expr, and a state state, and
;returns a new state that is the same as the old state except that the variable is bound to the value
;of the value expression. You should evaluate the value expression with eval-expr function that
;you will implement since it may be a complex expression. The result of the value expression should
;be stored in the variable -r.
;Tip: for starters, you can assume that the value expression is a number, a string, a boolean.
;Then, you can extend your implementation to handle function calls and function definitions. Especially the last examples bel


;(define (:= var val-expr state)
;   (let ((val (eval-expr val-expr state)))
;   (let ((newstate (put state var val)))
;    (put newstate '-r val))))
;
;val = (eval-expr val-expr state)
;newstate = (put state var val)
;above was what i tried to implement. it caused an error of bad syntax. so i had to make
;a nested call which is below and has poor readability. above code does not work yer is clearer.

(define (:= var val-expr state)
    (let ((val (eval-expr val-expr state)))
    (let ((newstate (put state var val)))
        (put newstate '-r val))))



(define while: 0)
(define func 0)
(define eval-exprs 0)