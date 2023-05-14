;needed updates:
;dont print procedure for last 2 inputs of function 2.
;continue working from function 7. 
;work on evaluating function.
;revise and test 5 6.
;function 4 - last 2 input examples.
;lambda needs rework in 8.
;symbol needs rewok in 8.
;read map-eval written by co-pilot.
;work on 5 and 9.
; we cannot evaluate false on 5.

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
(define empty-state 
    (let ((state (hash)))
    (hash-set state '-r 0))) ;initialize -r to 0



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



;extra function map-eval
;Given a list of expressions lst and a state state, this function evaluates each expression in lst
;with eval-expr function and returns results together with the resulting state after
;evaluating the last expression in lst. This function is not mandatory, but it is highly suggested to
;use this function in eval-expr function.
;(define map-eval 0)
;(+ 2 b)

(define (map-eval lst state)
    (printf "THE LIST IS  ~a\n" lst);
    ;first element is the operator
    (let* ((operator (car lst))
            (operands (cdr lst))
            (results (map (lambda (operand) (get (eval-expr operand state) '-r)) operands))) ;map each element of list (exprs) to values. resut is a list of mappings.
            ;results is a pure numeric exp list. for example, ( 2 b) gets to the form of ( 2 6)
    (let ((new-expr (cons operator results))) ;add the operator back to the list.
    (printf "THE NEW EXPR IS  ~a\n" new-expr);
    (let ((newstate (put state '-r (eval new-expr)))) ;evaluate the new expression and put it to the state.
    (printf "THE NEW STATE IS  ~a\n" newstate);
    newstate)))) ;return the new state. with -r variable updated.


; 8. eval-expr (20 points)
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
                (let ((val (eval (cons ':= expr))))
                (put state '-r val)));put returns the newstate            
            ((eq? (car expr) 'if:) 
            printf("we are in eval-expr and if section \n")
                (let ((val (eval (cons 'if: expr))))
                (put state '-r val)));put returns the newstate 
    
            ((eq? (car expr) 'while:) 
                (let ((val (eval (cons 'while: expr))))
                (put state '-r val)));put returns the newstate 
            
            ((eq? (car expr) 'func)
                (let ((val (eval (cons 'func: expr))))
                (put state '-r val)));put returns the newstate 

            ;lambda needs rework
            ;((eq? (car expr) 'lambda) 
                    ;(put state '-r (eval expr)))
            ;symbol needs rework
            ((symbol? (car expr))  ;operation cases
            (printf   "SYMBOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
            (let ((val  (eval (get (map-eval expr state) '-r))))
            (put state '-r val)));put returns the newstate 

            ;(map-eval expr state) ;map-eval all of the elements in the list and apply the operation to the resulting values.
            ;(put state '-r (eval expr)))  
    )
    
    ;else it is variable or literal
    (cond   ((number? expr) 
            (printf   "NUMBER CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((boolean? expr) 
            (printf   "BOOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (printf "the value is ~a \n" val)
                (cond   ((eq? val #t)
                        (printf "WE ARE ADDING TRUE TO THE STATE ~a \n" val)
                        (put state '-r '#t))
                        ((eq? val #f)
                        (printf "WE ARE ADDING FALSE  TO THE STATE ~a \n" val)
                        (put state '-r '#f))
                )));put returns the newstate

            ((symbol? expr) 
            (printf   "SINGLE SYMBOL CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (get state expr)))
                (put state '-r val)));put returns the newstate 

            ((string? expr) 
            (printf   "STRING CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((number? (get state expr)) ;a variable: if the variable with a value is found in the state     
            (printf   "VARIABLE CASE IN EVAL-EXPR AND EXPR IS ~a \n" expr)
                (let ((val (eval (get state expr))))
                (put state '-r val)));put returns the newstate 
    )));


; 4. := (15 points)
;This function takes a variable name var, a value expression val-expr, and a state state, and
;returns a new state that is the same as the old state except that the variable is bound to the value
;of the value expression. You should evaluate the value expression with eval-expr function that
;you will implement since it may be a complex expression. The result of the value expression should
;be stored in the variable -r.
;Tip: for starters, you can assume that the value expression is a number, a string, a boolean.
;Then, you can extend your implementation to handle function calls and function definitions. Especially the last examples bel

;temporary. only handles numbers right now.

(define (:= var val-expr state)
    (let ((new-state (eval-expr val-expr state)))
        (put new-state var (get new-state '-r))))



;5. if: (15 points)
;(if: test-expr then-exprs else-exprs state) (15 points)
;This function takes a test expression test-expr, a list of then expressions then-exprs, a list of
;else expressions else-exprs, and a state state, and returns the result of then expressions if the
;test expression evaluates to true, and the result of else expressions otherwise. You will evaluate


;the test expression with eval-expr function and evaluate then-exprs if -r is true in the resulting
;state after evaluating test-expr, and evaluate else-exprs otherwise.
;Tip: It is again highly suggested that you first implement this function for primitive types, and
;then extend it to handle function calls and function definitions
;(define if: 0)
(define (if: test-expr then-exprs else-exprs state)
  (printf "we are in if: \n")
  (let ((test-state (eval-expr test-expr state)))
  (printf "we are in if: and test-state is ~a \n" test-state) 
    (cond ((eq? (hash-ref test-state '-r) #t)
           (printf "EXPR IS TRUE AND EXPR IS ~a \n" test-expr)
          (eval-exprs then-exprs state))
          (else
           (printf "EXPR IS FALSE AND EXPR IS ~a \n" test-expr)
           (eval-exprs else-exprs state))
         )))


; 9 eval-exprs (5 points)
;Given a list of expressions exprs, this function evaluates each expression in exprs with eval-expr
;function and returns the resulting state after evaluating the last expression in exprs. You need to
;call each expression with the resulting state of the previous expression.
;Hint: foldl.
;(define eval-exprs 0)
(define (eval-exprs exprs state)
(display "we are in eval-exprs \n")
  (foldl (lambda (expr acc-state)
           (eval-expr expr acc-state))
           state
           exprs))


; 6. while: (15 points)
;This function takes a test expression test-expr, a list of body expressions body-exprs, and a state
;state. It evaluates the test expression with eval-expr function and evaluates the body expressions
;if -r is true in the resulting state after evaluating test-expr. It repeats this process until -r is false
;in the resulting state after evaluating test-expr. It returns the resulting state after evaluating the
;last test expression (since it will be the last expression to be evaluated).
;(define while: 0)



(define while: 0)
(define func 0)


