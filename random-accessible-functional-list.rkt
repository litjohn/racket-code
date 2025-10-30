#lang racket/base

(define (length-geq? l n)
  (let loop ([cur l]
             [len 0])

    (cond
      [(>= len n) #t]
      [(null? cur) #f]
      [else (loop (cdr cur) (add1 len))])))

(struct alist (roots) #:transparent)
(struct single-tree (root size) #:transparent)
(struct tree-node (val l r) #:transparent)

(define (alist-cons head l)
  (let ([rt (alist-roots l)])
    (if (length-geq? rt 2)
        (let ([a (car rt)]
              [b (cadr rt)]
              [rest (cddr rt)])

          (if (= (single-tree-size a) (single-tree-size b))
              (alist
               (cons (single-tree
                      (tree-node head (single-tree-root a) (single-tree-root b))
                      (add1 (* 2 (single-tree-size b))))
                     rest))

              (alist (cons (single-tree (tree-node head #f #f) 1) rt))))

        (alist (cons (single-tree (tree-node head #f #f) 1) rt)))))

(define (alist-car l)
  (let ([head (car (alist-roots l))])
    (tree-node-val
     (single-tree-root head))))

(define (alist-cdr l)
  (let ([head (car (alist-roots l))]
        [rest (cdr (alist-roots l))])

    (if (= (single-tree-size head) 1)
        (alist rest)
        (let* ([root (single-tree-root head)]
               [lson (tree-node-l root)]
               [rson (tree-node-r root)]
               [size (arithmetic-shift (single-tree-size head) -1)])

          (alist (cons (single-tree lson size) (cons (single-tree rson size) rest)))))))

(define (get-binary x)
  (define width (integer-length x))
  (define res (make-vector width #f))

  (let loop ([i (sub1 width)])
    (when (>= i 0)
      (when (not (zero? (bitwise-and 1 (arithmetic-shift x (- i)))))
        (vector-set! res i #t))

      (loop (sub1 i))))

  res)

(define (access-in-single-tree p idx)
  (define width (integer-length idx))
  (define bin (get-binary idx))

  (let loop ([i (- width 2)]
             [cur p])
    (if (>= i 0)
        (if (vector-ref bin i)
            (loop (sub1 i) (tree-node-r cur))
            (loop (sub1 i) (tree-node-l cur)))
        (tree-node-val cur))))

(define (random-access l pos)
  (set! pos (add1 pos)) ;; 0-base => 1-base

  (let ([roots (alist-roots l)])
    (let loop ([cur roots]
               [i pos])

      (let ([rt (car cur)])
        (if (> i (single-tree-size rt))
            (loop (cdr cur) (- i (single-tree-size rt)))
            (access-in-single-tree (single-tree-root rt) i))))))

(define (set-in-single-tree p idx v)
  (define width (integer-length idx))
  (define bin (get-binary idx))

  (let loop ([i (- width 2)]
             [cur p])

    (let ([org (tree-node-val cur)]
          [lson (tree-node-l cur)]
          [rson (tree-node-r cur)])

      (if (< i 0)
          (tree-node v lson rson)
          (if (vector-ref bin i)
              (tree-node org lson (loop (sub1 i) rson))
              (tree-node org (loop (sub1 i) lson) rson))))))

(define (random-set l pos v)
  (set! pos (add1 pos))

  (define res
    (let ([roots (alist-roots l)])
      (let loop ([cur roots]
                 [i pos])

        (let ([rt (car cur)])
          (if (> i (single-tree-size rt))
              (cons rt (loop (cdr cur) (- i (single-tree-size rt))))
              (cons (single-tree (set-in-single-tree (single-tree-root rt) i v) (single-tree-size rt)) (cdr cur)))))))
  (alist res))

;; tests

(define a (alist '()))
(set! a (alist-cons 3 a))
(set! a (alist-cons 5 a))

(displayln (random-access a 0))

(set! a (alist-cons 7 a))
(set! a (alist-cons 9 a))

(displayln (random-access a 2))

(set! a (random-set a 2 'a))

(displayln (random-access a 2))
(displayln (random-access (alist-cdr a) 1))
