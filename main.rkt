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
(define empty-state (hash))


; 2. get (5 points)
(define get 0)


; 3. put (5 points)
(define put 0)


; 4. := (15 points)
(define := 0)


; 5. if: (15 points)
(define if: 0)


; 6. while: (15 points)
(define while: 0)


; 7. func (15 points)
(define func 0)


; 8. eval-expr (20 points)
(define eval-expr 0)


; 9 eval-exprs (5 points)
(define eval-exprs 0)
