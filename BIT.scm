(define (low-bit x)
  (bitwise-and x (- x)))

(define (query-sum x prefix)
  (define res 0)

  (let loop ([i x])
    (if (> i 0)
      (begin
        (set! res 
              (+ res 
                 (vector-ref prefix i)))
        (loop (- i (low-bit i))))

      res)))

(define (add x c n prefix)
  (let loop ([i x])
    (when (<= i n)
      (vector-set! prefix i
                   (+ (vector-ref prefix i) c))
      (loop (+ i (low-bit i))))))

(define a (make-vector 100 0))