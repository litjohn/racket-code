(define (split l)
    (cond ((null? l) (cons '() '()))
            ((null? (cdr l)) (cons l '()))
            (else 
                (let*  ((x (car l))
                        (y (cadr l))

                        (tmp (split (cddr l)))
                        (a (car tmp))
                        (b (cdr tmp)))
                    (cons (cons x a) (cons y b))
                )
            )
    )
)

(define (merge a b)
    (cond
        ((null? a) b)
        ((null? b) a)
        (else 
            (let ((x (car a))
                    (y (car b)))

                (if (< x y)
                    (cons x (merge (cdr a) b))
                    (cons y (merge a (cdr b)))
                )
            )
        )
    )
)

(define (merge-sort l)
    (cond 
        ((or (null? l) (null? (cdr l))) l)
        (else 
            (let* ((tmp (split l))
                (a (car tmp))
                (b (cdr tmp)))
            (set! a (merge-sort a))
            (set! b (merge-sort b))
            (merge a b))
        )
    )
)