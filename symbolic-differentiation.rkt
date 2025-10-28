#lang racket/base

(require racket/match)

(define (D exp x)
  (match exp
    [(? (lambda (v) (not (pair? v))))
     (cond
       [(number? exp) 0]
       [(eq? x exp) 1]
       [else 0])]

    [`(+ . ,rators)
     (cons '+ (map (lambda (p) (D p x)) rators))]

    [`(* ,e) (D e x)]

    [`(* ,first . ,rators)
     (let ([rest (cons '* rators)])
       `(+ (* ,first ,(D rest x)) (* ,rest ,(D first x))))]))

(define (simplify exp)
  (match exp
    [(? (lambda (v) (not (pair? v))))
     exp]

    [`(+ . ,rest)
     (let ([res (map simplify rest)]
           [numbers 0]
           [l '()])

       (for ([i (in-list res)])
         (if (number? i)
             (set! numbers (+ numbers i))
             (set! l (cons i l))))

       (cond
         [(null? l) numbers]
         [(and (zero? numbers) (null? (cdr l))) (car l)]
         [else (cons '+
                     (if (zero? numbers)
                         l
                         (cons numbers l)))]))]

    [`(* . ,rest)
     (let ([res (map simplify rest)]
           [numbers 1]
           [l '()])

       (if (member 0 res)
           0
           (begin
             (for ([i (in-list res)])
               (if (number? i)
                   (set! numbers (* numbers i))
                   (set! l (cons i l))))

             (cond
               [(null? l) numbers]
               [(and (= numbers 1) (null? (cdr l))) (car l)]
               [else (cons '*
                           (if (= 1 numbers)
                               l
                               (cons numbers l)))]))))]))

(define (diff exp x)
  (simplify (D exp x)))
