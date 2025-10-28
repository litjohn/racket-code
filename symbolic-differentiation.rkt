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
           [l '()])

       (for ([i (in-list res)])
         (when (not (eqv? 0 i))
           (set! l (cons i l))))

       (cond
         [(null? l) 0]
         [(null? (cdr l)) (car l)]
         [else (cons '+ l)]))]

    [`(* . ,rest)
     (let ([res (map simplify rest)]
           [l '()])

       (if (member 0 res)
           0
           (begin
             (for ([i (in-list res)])
               (when (not (eqv? 1 i))
                 (set! l (cons i l))))

             (cond
               [(null? l) 1]
               [(null? (cdr l)) (car l)]
               [else (cons '* l)]))))]))

(define (diff exp x)
  (simplify (D exp x)))
