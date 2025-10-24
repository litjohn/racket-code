#lang racket

(define (product a b)
    (define (base s lst)
        (cond
            ((null? lst) '())
            (else 
                (cons (list s (car lst)) 
                    (base s (cdr lst))))))
            
    (cond
        ((null? a) '())
        (else 
            (let ([cur (base (car a) b)]
                  [next (product (cdr a) b)])
                        
                (append cur next)))))