#lang racket
(require racket/control)

(define c (void))

(displayln (reset (* 3 (shift k (set! c k) 3))))

(displayln (c 3))