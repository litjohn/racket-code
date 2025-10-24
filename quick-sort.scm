(define (quick-sort a)
    (cond 
        ((or (null? a) (null? (cdr a))) a)
        (else
            (let* ((povit (car a))
                    (rest (cdr a)))
                (append (quick-sort (filter (lambda (x) (<= x povit)) rest))
                        (cons povit '())
                      (quick-sort (filter (lambda (x) (> x povit)) rest)))
            )
        )
    )
)