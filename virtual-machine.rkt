#lang racket/base

(require racket/contract)
(require racket/match)

;; 让我们来实现一个虚拟机。
;; 基于栈的虚拟机。其中含有的一切值的类型都是整数。

(struct val-type (val) #:transparent)
(define/contract (contracted-make-val x)
  (-> integer? val-type?)
  (val-type x))

;; 我们可以让 make-val 被定义为 val 以消除契约的开销，或者在平时将它定义为 contracted-make-val 以获得类型安全
(define make-val contracted-make-val)

(define get-val val-type-val)

;; 虚拟机主体
;; 设计一下指令集。
;; `#(store ,pos) 将栈顶存储至指定位置
;; `#(load ,pos) 将指定位置存入栈顶
;; `#(imm ,val) 将立即数压入栈顶
;; `#(call ,line) 跳转至指定行并进行函数压栈
;; `#(ret) 返回并弹出函数栈
;; 为了实现这些，我们需要一个 dump 来存储栈以及 PC
;; 我们需要一些算术操作。暂时加上 add，sub，mul，div，mod。
;; 自然需要 `#(jmp ,line) 无条件跳转，以及对应的一些有条件跳转
;; 使用 `#(jcond ,line) 进行条件跳转。如果栈顶不为零则跳转到 line。
;; 用 read 和 print 指令进行 io
;; 用 drop 指令弹栈

(define (run ins-vec [memory (make-vector 4096 (make-val 0))] [dump '()] [stack '()] [last-pc 0])
  (define/contract (push x)
    (-> val-type? void?)
    (set! stack (cons x stack)))

  (define (top)
    (car stack))

  (define (pop)
    (set! stack (cdr stack)))

  (define (call pc)
    (set! dump (cons pc dump)))

  (define (return)
    (let ([old-pc (car dump)])

      (set! dump (cdr dump))
      (add1 old-pc)))

  (define end (vector-length ins-vec))

  (let loop ([pc last-pc])
    (when (< pc end)
      (let ([ins (vector-ref ins-vec pc)])
        (match ins
          [`#(store ,pos) (vector-set! memory pos (top)) (loop (add1 pc))]
          [`#(load ,pos) (push (vector-ref memory pos)) (loop (add1 pc))]
          [`#(imm ,val) (push (make-val val)) (loop (add1 pc))]
          [`#(drop) (pop) (loop (add1 pc))]
          [`#(call ,line) (call pc) (loop line)]
          [`#(ret) (loop (return))]
          [`#(jmp ,line) (loop line)]
          [`#(jcond ,line)

           (let ([flag (top)])
             (pop)
             (if (zero? (get-val flag))
                 (loop (add1 pc))
                 (loop line)))]

          [`#(add)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (+ (get-val a) (get-val b))))))

           (loop (add1 pc))]

          [`#(sub)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (- (get-val b) (get-val a))))))

           (loop (add1 pc))]

          [`#(mul)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (* (get-val a) (get-val b))))))

           (loop (add1 pc))]

          [`#(div)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (quotient (get-val b) (get-val a))))))

           (loop (add1 pc))]

          [`#(mod)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (remainder (get-val b) (get-val a))))))

           (loop (add1 pc))]

          [`#(print) (displayln (get-val (top))) (loop (add1 pc))]
          [`#(read) (push (make-val (read))) (loop (add1 pc))]
          [else (error "Invalid instruction!")])))))

;;; test
(define a1
  `#(#(jmp 5)
     #(read)
     #(read)

     #(mul)
     #(ret)
     #(call 1)
     #(print)))

(run a1)
