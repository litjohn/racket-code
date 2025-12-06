#lang racket/base

(require racket/match)

;; a.k.a https://github.com/litjohn/racket-code/virtual-machine.rkt
(require (file "virtual-machine.rkt"))

;; 在有了虚拟机之后，我们来实现一门简单的语言
;; 命名为 while
;; 让我们来想想它支持哪些语言特性
;; 一个 while 程序由若干个子程序构成
;; 入口是名为 main 的子程序
;; 一个子程序/函数会接受一些参数
;; 通过压栈传递
;; 这里涉及到变量的定义和取值、赋值等等。这是一个非常重要的问题。
;; 对于一个变量，我们在内存中给它分配一个地址
;; 一个非常重要的操作大概是“对于一个表达式，将它求值并将求值结果放在栈顶”
;; 感觉依然是经典的递归下降解析。
;; 虽然我们决定让代码以 S 表达式形式书写，依然需要使用一些语法分析和语法树节点
;; 开始吧

(struct immediate-node (val) #:transparent)
(struct varible-node (name) #:transparent)
(struct add-node (op1 op2) #:transparent)
(struct mul-node (op1 op2) #:transparent)
(struct sub-node (op1 op2) #:transparent)
(struct div-node (op1 op2) #:transparent)
(struct mod-node (op1 op2) #:transparent)

(define (expression? e)
  (or (varible-node? e) (add-node e) (sub-node e) (mul-node e) (div-node e) (mod-node e)))

(define (compile-while ast)
  (define available 0) ; 空的，可用于新的变量的地址
  (define var-table (make-hash)) ; 变量符号表，维护变量名到内存地址的映射

  (define (new-varible name)
    (hash-set! var-table name available)
    (set! available (add1 available)))

  ;; 我们的第一部分是一个表达式求值器。接受一个表达式，求出它的值

  (define (process-expression exp)
    (match exp
      [`(varible-node ,name) `(#(load ,(hash-ref var-table name)))]
      [`(immediate-node ,val) `(#(imm ,val))]
      [else (raise-argument-error "Exp is not an valid expression!")]))

  (void))
