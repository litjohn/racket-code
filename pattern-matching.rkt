#lang racket

(define-syntax pmatch-if
  (lambda (stx)
    (syntax-case stx (quote any/p)
      [(_ val any/p t f) #'t]
      [(_ val x t f)
       (identifier? #'x)
       #'(let ([x val])
           t)]

      [(_ val (quote x) t f)
       #'(if (equal? val 'x)
             t
             f)]

      [(_ val x t f)
       (not (pair? (syntax->datum #'x)))
       #'(if (equal? val 'x)
             t
             f)]

      [(_ val (pat1 . pat2) t f)
       #'(if (pair? val)
             (let ([x (car val)]
                   [y (cdr val)])
               (pmatch-if x pat1 (pmatch-if y pat2 t f) f))

             f)])))

(define-syntax pmatch
  (syntax-rules ()
    [(_ v) (error "pmatch: match failed:" v)]
    [(_ v [pat body ...] rest ...)
     (let* ([tmp v]
            [failure (lambda () (pmatch tmp rest ...))])
       (pmatch-if tmp pat (begin body ...) (failure)))]))

;; ==========================================
;; 单元测试集
;; ==========================================

(module+ test
  (require rackunit)

  (test-case "基础字面量匹配"
    (check-equal? (pmatch 1 [1 'one] [_ 'fail]) 'one)
    (check-equal? (pmatch "hello" ["hello" 'hi] [_ 'fail]) 'hi)
    (check-equal? (pmatch #t [#t 'true] [_ 'fail]) 'true)
    ;; 关键测试：空列表
    (check-equal? (pmatch '() [() 'empty] [_ 'fail]) 'empty))

  (test-case "变量绑定与通配符"
    (check-equal? (pmatch 10 [x (+ x 1)]) 11)
    (check-equal? (pmatch 10 [any/p 'ok]) 'ok)
    ;; 变量遮蔽测试
    (check-equal? (pmatch 10 [x (pmatch 20 [x x])]) 20))

  (test-case "列表解构"
    (check-equal? (pmatch '(1 2) [(a b) (+ a b)]) 3)
    (check-equal? (pmatch '(1 2 3) [(a . rest) rest]) '(2 3))
    ;; 嵌套匹配
    (check-equal? (pmatch '((1 2) 3) [((a b) c) (+ a b c)]) 6))

  (test-case "引用符号匹配 (Quote)"
    (check-equal? (pmatch 'apple ['apple 'is-apple] [_ 'no]) 'is-apple)
    ;; 区分变量和符号
    (check-equal? (pmatch 'apple [x 'variable] ['apple 'symbol]) 'variable) ;; x 优先捕获
    (check-equal? (pmatch 'apple ['apple 'symbol] [x 'variable]) 'symbol))

  (test-case "递归计算器"
    ;; 匹配结构：(op arg1 arg2)
    (define (calc expr)
      (pmatch expr
              [('add e1 e2) (+ (calc e1) (calc e2))]
              [('mul e1 e2) (* (calc e1) (calc e2))]
              [x x]))

    (check-equal? (calc '(add (mul 2 3) 4)) 10))

  (test-case "向量测试 (Edge case)"
    ;; 你的 (not (pair? ...)) 逻辑把向量也视为原子，
    ;; 所以 (equal? val '#(1 2)) 应该能工作
    (check-equal? (pmatch '#(1 2) [#(1 2) 'vec] [_ 'fail]) 'vec))

  #; (displayln "所有测试通过！"))
