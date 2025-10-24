#lang racket

(define (occurs-free? x exp)
    (match exp
        (`(lambda (,var) ,body)
            (if (eq? x var) 
                #f 
                (occurs-free? x body)))

        (`(,ator ,rand) 
            (or (occurs-free? x ator) 
                (occurs-free? x rand)))

        (s (symbol? s) (eq? s x))
        (else (error "invalid expression!" exp))))