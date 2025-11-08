#lang racket/base

(require racket/contract)
(require racket/match)
(require racket/bool)

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
;; 加上 and/or/xor/not 布尔运算，以及 lt 比较运算
;; lt 是一个一元算符，如果栈顶大于 0 则将栈顶改为非零值（1），否则将栈顶赋为 0
;; 行号跳转还是太痛苦了。加一个 label 系统
;; `#(label ,sym)

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
  (define labels (make-hash))

  (let loop ([i 0])
    (when (< i end)
      (match (vector-ref ins-vec i)
        [`#(label ,sym)
         (hash-set! labels sym i)]
        [else (void)])

      (loop (add1 i))))

  (let loop ([pc last-pc])
    (when (< pc end)
      (let ([ins (vector-ref ins-vec pc)])
        (match ins
          [`#(label ,sym) (loop (add1 pc))]
          [`#(store ,pos) (vector-set! memory pos (top)) (loop (add1 pc))]
          [`#(load ,pos) (push (vector-ref memory pos)) (loop (add1 pc))]
          [`#(imm ,val) (push (make-val val)) (loop (add1 pc))]
          [`#(drop) (pop) (loop (add1 pc))]
          [`#(call ,line)
           (call pc)

           (when (symbol? line)
             (set! line (hash-ref labels line)))

           (loop line)]

          [`#(ret) (loop (return))]

          [`#(jmp ,line)
           (when (symbol? line)
             (set! line (hash-ref labels line)))

           (loop line)]

          [`#(jcond ,line)
           (when (symbol? line)
             (set! line (hash-ref labels line)))

           (let ([flag (get-val (top))])
             (pop)
             (if (zero? flag)
                 (loop (add1 pc))
                 (loop line)))]

          [`#(add)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push (make-val (+ a b)))))

           (loop (add1 pc))]

          [`#(sub)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push (make-val (- b a)))))

           (loop (add1 pc))]

          [`#(mul)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push (make-val (* a b)))))

           (loop (add1 pc))]

          [`#(div)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push (make-val (quotient b a)))))

           (loop (add1 pc))]

          [`#(mod)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push (make-val (remainder b a)))))

           (loop (add1 pc))]

          [`#(lt)

           (let ([v (get-val (top))])
             (pop)
             (if (< 0 v)
                 (push (make-val 1))
                 (push (make-val 0))))

           (loop (add1 pc))]

          [`#(and)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push
                (make-val
                 (if (or (zero? a) (zero? b))
                     0
                     1)))))

           (loop (add1 pc))]

          [`#(or)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push
                (make-val
                 (if (and (zero? a) (zero? b))
                     0
                     1)))))

           (loop (add1 pc))]

          [`#(xor)
           (let ([a (get-val (top))])
             (pop)
             (let ([b (get-val (top))])
               (pop)
               (push
                (make-val
                 (if (xor (zero? a) (zero? b))
                     1
                     0)))))

           (loop (add1 pc))]

          [`#(not)

           (let ([v (get-val (top))])
             (pop)
             (if (zero? v)
                 (push (make-val 1))
                 (push (make-val 0))))

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

;; not 5 (非零) -> 0
(define test-not-1
  '#(#(imm 5)
     #(not)
     #(print))) ; 应该打印 0

;; not 0 -> 1
(define test-not-2
  '#(#(imm 0)
     #(not)
     #(print))) ; 应该打印 1

;; 3 < 5 -> 1 (true)
(define test-lt-1
  '#(#(imm 3)
     #(imm 5)
     #(sub)
     #(lt)
     #(print))) ; 应该打印 1

;; 5 < 3 -> 0 (false)
(define test-lt-2
  '#(#(imm -1)
     #(lt)
     #(print))) ; 应该打印 0

;; 1 and 1 (true and true) -> 1
(define test-and
  '#(#(imm 1)
     #(imm 1)
     #(and)
     #(print) ; -> 1
     #(imm 1)
     #(imm 0)
     #(and)
     #(print) ; -> 0
     #(imm 0)
     #(imm 0)
     #(and)
     #(print))) ; -> 0

;; (修正版) 读入一个数 x，如果 x > 10 则打印 100，否则打印 200
(define test-if-else-fixed
  '#(#(read)       ; 0: 读入 x
     #(imm 10)     ; 1: 压入 10
     #(sub)        ; 2: 相减
     #(lt)         ; 3: 比较 x 和 10。如果 x>10, 栈顶为1, 否则为0
     #(jcond 7)     ; 3: 如果栈顶非零 (x > 10)，跳转到 "then" 分支
     ;; "else" 分支 (如果栈顶是 0)
     #(imm 200)    ; 4: 压入 200
     #(jmp 8)       ; 5: 跳转到结尾
     ;; "then" 分支
     #(imm 100)    ; 6: 压入 100
     ;; 结尾
     #(print)))    ; 7: 打印栈顶的值

(define test-while
  '#(#(read)
     #(label while)
     #(store 0)
     #(drop)
     #(imm 5)
     #(load 0)
     #(sub)
     #(lt)
     #(not)
     #(jcond end)
     #(imm 1)
     #(print)
     #(load 0)
     #(add)
     #(jmp while)
     #(label end)))

;; (run a1)

(provide run)
(provide make-val)
(provide get-val)
