#lang racket/base

;; 一个测试，经典的八皇后问题，用于试手

(define n 0)

(define vis-col 0)
(define vis-skew1 0)
(define vis-skew2 0)

(define all-solutions '())

(define (solve i sol)
  (cond
    [(> i n) (set! all-solutions (cons sol all-solutions))]
    [else
     (let loop ([j 1])
       (when (<= j n)
         (when (not (or (vector-ref vis-col j) (vector-ref vis-skew1 (+ i j)) (vector-ref vis-skew2 (+ n (- j i)))))
           (vector-set! vis-col j #t)
           (vector-set! vis-skew1 (+ i j) #t)
           (vector-set! vis-skew2 (+ n (- j i)) #t)
           (solve (add1 i) (cons j sol))
           (vector-set! vis-col j #f)
           (vector-set! vis-skew1 (+ i j) #f)
           (vector-set! vis-skew2 (+ n (- j i)) #f))
         (loop (add1 j))))]))

(define (main)
  (set! n (read))
  (set! vis-col (make-vector (+ n 2) #f))
  (set! vis-skew1 (make-vector (+ 2 (* n 2)) #f))
  (set! vis-skew2 (make-vector (+ 2 (* n 2)) #f))
  (solve 1 '())
  (for ([sol (in-list all-solutions)])
    (for ([pos (in-list sol)])
      (printf "~a " pos))
    (newline))
    (exit))

(main)
