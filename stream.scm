; (optimize-level 3)

(define-syntax delay
  (syntax-rules ()
    ((delay x) (lambda () x))))

(define (lazy-variable thunk)
  (cons #f thunk))

(define (is-calculated? variable)
  (car variable))

(define (force variable)
  (if (is-calculated? variable)
    (cdr variable)
    (begin
      (set-car! variable #t)
      (set-cdr! variable ((cdr variable)))
      (cdr variable))))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a b)
     (cons
       a
       (lazy-variable
         (delay b))))))

(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))

(define (int-from n)
  (stream-cons
    n
    (int-from
      (+ n 1))))

(define nature-numbers (int-from 1))

(define (get a pos)
  (if (= pos 0)
    (stream-car a)
    (get (stream-cdr a) (- pos 1))))

(define (take a len)
 (if (= len 0)
   '()
   (cons
     (stream-car a)
     (take (stream-cdr a) (- len 1)))))

(define (transfer-to-stream operator a)
  (if (null? a)
    '()
    (stream-cons
      (operator (car a))
      (transfer-to-stream operator (cdr a)))))

(define (stream-map operator a)
  (if (null? a)
    '()
    (stream-cons
      (operator (stream-car a))
      (stream-map operator (stream-cdr a)))))

(define (stream-filter is-valid? a)
  (cond 
    ((null? a) '())
    ((is-valid? (stream-car a)) 
     (stream-cons 
       (stream-car a) 
       (stream-filter 
         is-valid? 
         (stream-cdr a))))

    (else (stream-filter is-valid? (stream-cdr a)))))

(define (sieve a)
  (define (divisible? x y)
    (= (mod x y) 0))
    
  (stream-cons
    (stream-car a)
    (sieve 
      (stream-filter 
        (lambda (x)
          (not (divisible? x (stream-car a))))
        (stream-cdr a)))))

(define prime (sieve (int-from 2)))

(define (stream-merge s1 s2)
  (let ((h1 (stream-car s1))
        (h2 (stream-car s2)))
    (cond ((< h1 h2)
           (stream-cons h1 (stream-merge (stream-cdr s1) s2)))
          ((> h1 h2)
           (stream-cons h2 (stream-merge s1 (stream-cdr s2))))
          (else ; h1 = h2, 去重
           (stream-cons h1 (stream-merge (stream-cdr s1) (stream-cdr s2)))))))

(define ans (stream-cons 1 
                         (stream-merge (stream-map (lambda (x) (* 2 x)) ans) 
                                       (stream-merge (stream-map (lambda (x) (* 3 x)) ans) 
                                                     (stream-map (lambda (x) (* 5 x)) ans)))))