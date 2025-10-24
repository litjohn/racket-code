(define x (read))

(define true #t)
(define false #f)

(define (abs x) (if (< 0 x) x (- x)))

(define (sqrt-iter x guess)
    (define new-guess (/ (+ guess (/ x guess)) 2.0))
    (if (< (abs (- guess new-guess)) 0.001) guess (sqrt-iter x new-guess)))

(define (sqrt x)
    (sqrt-iter x 1.0))

(display (sqrt x))

(newline)