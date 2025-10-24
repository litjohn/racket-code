(define (counter-maker x)
    (let ((num x))
        (define (get-count) num)
        (define (increase) (set! num (+ num 1)))
        (define (decrease) (set! num (- num 1)))

        (define (message str)
            (cond ((string=? str "get-count") (get-count))
                ((string=? str "increase") (increase))
                ((string=? str "decrease") (decrease))
                (else 'err)
            )
        )

        message
    )
)