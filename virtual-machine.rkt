#lang racket/base

(require racket/contract)
(require racket/match)

;; 让我们来构建一个抢占式多任务的操作系统
;; 首先，我们来实现一个虚拟机。我们操作系统运行的所有任务都是用这个虚拟机的指令写的。
;; 基于栈的虚拟机。其中含有的一切值的类型都是整数。

(struct val-type (val) #:transparent)
(define/contract (make-val x)
  (-> integer? val-type?)
  (val-type x))

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

(define (run ins-vec [dump '()] [stack '()] [memory (make-vector 4096 (make-val 0))] [last-pc 0])
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
          [`#(exhause) (pop) (loop (add1 pc))]
          [`#(call ,line) (call pc) (loop line)]
          [`#(ret) (loop (return))]
          [`#(jmp ,line) (loop line)]

          [`#(add)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (+ (get-val a) (get-val b))))))

           (loop (add1 pc))]

          [`#(mul)
           (let ([a (top)])
             (pop)
             (let ([b (top)])
               (pop)
               (push (make-val (* (get-val a) (get-val b))))))

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
