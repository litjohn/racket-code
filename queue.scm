(define (make-queue)
    (define size 0)
    (define head (cons 'void '()))
    (define tail head)

    (define (empty?)
        (= size 0)
    )

    (define (push x)
        (set-car! tail x)
        (set-cdr! tail (cons 'void '()))
        (set! tail (cdr tail))
        (set! size (+ size 1))
    )

    (define (pop)
        (if (empty?)
            (error "pop" "cannot pop empty queue!")
            (begin
                (set! head (cdr head))
                (set! size (- size 1))
            )
        )
    )

    (define (front)
        (if (empty?)
            (error "front" "cannot get front of empty queue!")
            (car head)
        )
    )

    (lambda (x)
        (cond 
            ((eq? x 'empty?)
                (empty?)
            )

            ((eq? x 'front)
                (front)
            )

            ((eq? x 'pop)
                (pop)
            )

            ((eq? x 'push)
                (lambda (y)
                    (push y)
                )
            )

            ((eq? x 'size)
                size
            )
        )
    )
)