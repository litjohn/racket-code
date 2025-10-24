(define n (read))
(define a (make-vector (+ n 2) 0))
(define dp (make-vector (+ n 2) 2000000000))
(vector-set! dp 0 0)

(let loop ((i 1))
    (when (<= i n)
        (vector-set! a i (read))
        (loop (+ i 1))
    )
)

(define ans 0)

(define (find x)
    (let loop  ((l 0)
                (r ans))
        
        (if (< l r)
            (let ((mid (div (+ l r 1) 2)))
                (if (< (vector-ref dp mid) x)
                    (loop mid r)
                    (loop l (- mid 1))
                )
            )

            l
        )
    )
)

(let query ((i 1))
    (when (<= i n)
        (let ((x (find (vector-ref a i))))
            (vector-set! dp (+ x 1) 
                (min
                    (vector-ref dp (+ x 1))
                    (vector-ref a i)))
            
            (set! ans (max ans (+ x 1)))
        )

        (query (+ i 1))
    )
)

(display ans)