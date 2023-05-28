; Basak Tepe
; 2020400117
; compiling: yes
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
(define empty-state
        (hash))



; 2. get (5 points) 
(define (get state var)
    (let ((val (hash-ref state var #f))) ;#f is the default value if the key is not found
    (if (not (eq? val #f)) ;if the key is found, val is not false.
        val
    (eval var)))) ;else (if key not found) evaluate the key.



; 3. put (5 points)
(define (put state var val)
    (let ((newstate (hash-set state var val))) ;update the value or create a new key-value pair
    newstate));return the new state



;extra function
(define (map-eval lst state)
    ;first element is the operator, rest are the operands. (+ 2 3)
    (let* ((operator (car lst))
            (operands (cdr lst))
            (results (map (lambda (operand) (get (eval-expr operand state) '-r)) operands))) ;map each element of list (operand) to values. resut is a revealed list of values.
            ;results is a pure numeric exp list. for example, ( 2 b) gets to the form of ( 2 6)
    (let ((new-expr (cons operator results))) ;add the operator back to the list.
    (let ((newstate (put state '-r (eval new-expr)))) ;evaluate the new expression and put it to the state.
    newstate)))) 




;4. eval-expr
(define (eval-expr expr state)
    (if (and (list? expr) (> (length expr) 1)) ;if it is a list

    ;we will go into a switch-like structure.
    (cond   ((eq? (car expr) ':=) 

                ;removing := from exp
                (let ((rem-exp (cdr expr)))
                ;calling the := function I implemented with proper bits of the expression
                (let (( return-state (:= (car rem-exp) (car (cdr rem-exp)) state)))
                (let ((val (get return-state '-r)))
                (put return-state '-r val))))

            );put returns the newstate   


            ((eq? (car expr) 'if:) 
                (let ((rem-exp (cdr expr)))
                ;calling the if function I implemented with proper bits of the expression
                (let ((return-state (if: (car rem-exp) (car (cdr rem-exp)) (car (cdr (cdr rem-exp))) state)))
                (let ((val (get return-state '-r)))
                (put return-state '-r val)))))

    
            ((eq? (car expr) 'while:) 
                (let ((rem-exp (cdr expr)))
                ;calling my while function with proper bits of the expression
                (let ((return-state (while: (car rem-exp) (car (cdr rem-exp)) state)))
                (let ((val (get return-state '-r)))
                (put return-state '-r val)))))
            
            ((eq? (car expr) 'func)
                (let ((rem-exp (cdr expr)))
                ;calling my func with proper bits of the expression
                (let ((return-state (func (car rem-exp) (car (cdr rem-exp)) state)))
                (let ((val (get return-state '-r)))
                (put return-state '-r val)))))

            ;lambda
            ((eq? (car expr) 'lambda) 
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate

            ;symbol cases
            ((symbol? (car expr))  ;operation cases
            (let ((val (eval (hash-ref (map-eval expr state) '-r )))) ; map (+ 2 a) to (+ 2 3) and evaluate it.
            (put state '-r val)));put returns the newstate 
    )
    
    ;if not a list then it is variable or literal
    (cond   ((number? expr) 
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((boolean? expr) 
                (let ((val (eval expr)))
                (cond   ((eq? val #t)
                        (put state '-r '#t))
                        ((eq? val #f)
                        (put state '-r '#f))
                )));put returns the newstate

            ((symbol? expr) 
                (let ((val (get state expr))) ;looking it up
                (put state '-r val)));put returns the newstate 

            ((string? expr) 
                (let ((val (eval expr)))
                (put state '-r val)));put returns the newstate 

            ((number? (get state expr)) 
                (let ((val (eval (get state expr))))
                (put state '-r val)));put returns the newstate 
    )));


;5 (:= var val-expr state)
(define (:= var val-expr state)
    (let ((new-state (eval-expr val-expr state))) ;evaluate right hand side. Then put it into both the '-r and the var in the state.
    (put new-state var (get new-state '-r))))


;6. (if: test-expr then-exprs else-exprs state)
(define (if: test-expr then-exprs else-exprs state)
    ;we are in if
    (let ((test-state (eval-expr test-expr state)))
    ;we tested the test expr and got a state where '-r is either true or false.
    (cond ((eq? (hash-ref test-state '-r) #t)
            ;if the test expr is true; we will evaluate the then exprs.
            (eval-exprs then-exprs state))
            (else
            ;if the test expr is false; we will evaluate the else exprs.
            (eval-exprs else-exprs state))
            )))


;7. (eval-exprs exprs state)
(define (eval-exprs exprs state) ;evaluate all expressions and gather the results.
        (foldl eval-expr state exprs))



;8. (while: test-expr body-exprs state)
(define (while: test-expr body-exprs state)
    (let ((test-state (eval-expr test-expr state))) ;a state where '-r is either true or false, it is the value of test-expr.

    ;switch structure
    (cond 
            ((eq? (hash-ref test-state '-r) #t) ;if the test expr is true; we will evaluate the body exprs and then evaluate the test expr again.
            (let ((new-state (eval-exprs body-exprs state)))
            (let ((new-test-state (eval-expr test-expr new-state))) ;evaluate the test expr in the new state.
            ;if it is true, we will evaluate the body exprs by calling while again.
            (cond ((eq? (hash-ref new-test-state '-r) #t)
                (while: test-expr body-exprs new-state)) ;evaluate the body exprs again.
                ;else we will return the new state.
                (else
                (let ((new-state (put new-state '-r #f))) ;latest evaluated exp - test exp is false. make it false.
                new-state))
            )))))));return the new state.



;9. (func params body-exprs state)
(define (func params body-exprs state)
    ;creates a lambda procedure
    ;puts this procedure into the state in '-r.
    ;lamda procedure is responsible for evaluating the body-exprs with the given params, then returning the result.
    (let ((new-state (put state '-r (lambda params (get (eval-exprs body-exprs state) '-r)))))
    new-state))







