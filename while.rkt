#lang racket/base

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

(define (compile-while code)
  (define available 0) ; 空的，可用于新的变量的地址
  (define var-table (make-hash)) ; 变量符号表，维护变量名到内存地址的映射

  (define (new-varible name)
    (hash-set! var-table name available)
    (set! available (add1 available))))
