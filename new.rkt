#lang racket

;; 1. 定义变量
;; 我们用 vector 来表示逻辑变量，方便和普通的 symbol 区分
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (eq? x1 x2))

;; 2. 替换表 (Substitution)
;; 本质是一个关联列表 (Association List): '((var1 . val1) (var2 . val2))
(define empty-s '())

;; extend-s: 扩展替换表，把 x 绑定到 v
(define (ext-s x v s)
  (cons `(,x . ,v) s))

;; 3. Walk (寻值)
;; 这是核心。在一个替换表中查找变量的最终值。
;; 如果 x 绑定了 y，y 绑定了 5，那么 (walk x s) 应该得到 5。
(define (walk u s)
  (let ((pr (and (var? u) (assq u s))))
    (if pr
        (walk (cdr pr) s) ;; 继续递归查找
        u)))              ;; 找不到或者不是变量，就返回自己

;; 4. Unify (合一)
;; 尝试让 u 和 v 相等，返回新的替换表；如果无法合一，返回 #f
(define (unify u v s)
  (let ((u (walk u s))   ;; 先把 u, v 解析到最底层的值
        (v (walk v s)))
    (cond
      ((eqv? u v) s)     ;; 已经相等，无需操作，返回原表
      ((var? u) (ext-s u v s)) ;; u 是变量，把 u->v 加入表
      ((var? v) (ext-s v u s)) ;; v 是变量，把 v->u 加入表
      ((and (pair? u) (pair? v)) ;; 都是 pair，递归合一 car 和 cdr
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else #f))))       ;; 无法合一（比如 5 和 6），失败

;; 5. 基础 Goal 构造器

;; == 关系：让 u 和 v 相等
(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s `(,s) '())))) ;; 成功返回包含 s 的列表，失败返回空表

;; 成功与失败
(define succeed (lambda (s) `(,s)))
(define fail    (lambda (s) '()))

;; 6. 流的操作 (简易版 Monad)

;; mplus (Monad Plus): 合并两个流 (相当于逻辑 OR / disj)
;; 这里的实现比较 naive，真正的 miniKanren 会在这里做 interleaving (交错)
;; 以防止深度优先搜索陷入无限分支。
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $1 ($2)))) ;; 处理惰性流
    (else (cons (car $1) (mplus (cdr $1) $2)))))

;; bind (Monad Bind): 将目标 g 应用到流 $ 中的每个元素 (相当于逻辑 AND / conj)
(define (bind $ g)
  (cond
    ((null? $) '())
    ((procedure? $) (lambda () (bind ($) g)))     ;; 处理惰性流
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;; 7. 逻辑连接词
(define (disj g1 g2) (lambda (s) (mplus (g1 s) (g2 s))))
(define (conj g1 g2) (lambda (s) (bind (g1 s) g2)))

;; 8. 引入新变量 (call/fresh)
;; f 是一个函数，接受一个新变量，返回一个 Goal
(define (call/fresh f)
  (lambda (s)
    (let ((c (length s))) ;; 用当前 s 的长度作为新变量的名字(ID)，简单粗暴
      ((f (var c)) s))))

;; 9. 运行接口 (Run)
;; q 是查询变量
(define (run goal)
  (let ((q (var 'q)))
    ;; 我们把 q 包装进 goal，最后只把 q 的值 walk 出来
    (map (lambda (s) (reify-name (walk* q s)))
         ((call/fresh (lambda (x) (conj (== x q) (goal x)))) empty-s))))

;; 辅助函数：递归彻底 walk (把结构里所有的变量都换成值)
(define (walk* v s)
  (let ((v (walk v s)))
    (if (pair? v)
        (cons (walk* (car v) s)
              (walk* (cdr v) s))
        v)))

;; 辅助函数：把剩下的丑陋变量 #(0) #(1) 换成好看的 symbols _.0 _.1
(define (reify-name v)
  (if (var? v)
      (string->symbol (format "_.~a" (vector-ref v 0)))
      (if (pair? v)
          (cons (reify-name (car v)) (reify-name (cdr v)))
          v)))
