(define n (read))

(define ans (make-vector (+ n 1) '()))
(define (calc i)
  (when (<= i n)
    (let ((pre (vector-ref ans (- i 1))))  
        (vector-set! ans i                    
            (cons pre pre)))
    (calc (+ i 1))))

(calc 1)

(let output ((idx 0))
    (when (<= idx n)
        (display (vector-ref ans idx))
        (newline)
        (output (+ idx 1))
    )
)