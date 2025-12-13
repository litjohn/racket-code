#lang racket

(require rackunit)

;; ==========================================
;; 你的 CPS 核心代码 (已整合 Join Point 优化)
;; ==========================================

(define (atom? x) (not (pair? x)))
(define intrinsics (hash '+ #t '- #t '* #t '/ #t 'cons #t 'car #t 'cdr #t 'null? #t '= #t '< #t '> #t '<= #t '>= #t))
(define (is-intrinsic? op) (hash-has-key? intrinsics op))
(define keywords '(lambda define begin quote))

(define (wrap k exp)
  (if (eq? k 'identity) exp (list k exp)))

(define (is-app-exp? exp)
  (not (or (atom? exp) (member (car exp) keywords) (is-intrinsic? (car exp)))))

(define (cps1 exp k arg-acc #:intr? [flag-intr #f])
  (match exp
    [`(define ,id ,exp)
     `(begin (define ,id ,(cps1 exp 'identity '())) (,k #f))]

    [`(lambda ,bind . ,body)
     (let ([res (let ([c (gensym 'k)])
                  `(lambda ,(append bind (list c)) ,(cps1 (cons 'begin body) c '())))])
       (wrap k res))]

    [`(begin ,exp)
     (cps1 exp k '())]

    [`(begin ,exp . ,rest) #:when (not (null? rest))
                           (let ([c1 (gensym 'v)])
                             (cps1 exp `(lambda (,c1) ,(cps1 (cons 'begin rest) k '())) '()))]

    ;; === 重点修改：Join Point 优化 ===
    [`(if ,condition ,then-c ,else-c)
     (let ([c (gensym 'v)])
       ;; 定义生成器
       (define (build-if k-val)
         (if (is-app-exp? condition)
             (cps1 condition `(lambda (,c) (if ,c ,(cps1 then-c k-val '()) ,(cps1 else-c k-val '()))) '())
             `(if ,(cps1 condition 'identity '()) ,(cps1 then-c k-val '()) ,(cps1 else-c k-val '()))))

       ;; 优化逻辑：如果 k 是复杂 lambda，就用 let 绑起来
       (if (or (atom? k) (eq? k 'identity))
           (build-if k)
           (let ([k-tmp (gensym 'k)])
             `(let ([,k-tmp ,k]) ,(build-if k-tmp)))))]
    ;; ===============================

    [`(,intrin . ,rest) #:when (is-intrinsic? intrin)
                        (cps1 rest k '() #:intr? intrin)]

    [`(,exp) #:when flag-intr
             (let ([c1 (gensym 'v)])
               (if (is-app-exp? exp)
                   (cps1 exp `(lambda (,c1) ,(wrap k (cons flag-intr (reverse (cons c1 arg-acc))))) '())
                   (wrap k (cons flag-intr (reverse (cons (cps1 exp 'identity '()) arg-acc))))))]

    [`(,exp1 . ,rest) #:when (and flag-intr (not (null? rest)))
                      (let ([c1 (gensym 'v)])
                        (if (is-app-exp? exp1)
                            (cps1 exp1 `(lambda (,c1) ,(cps1 rest k (cons c1 arg-acc) #:intr? flag-intr)) '())
                            (cps1 rest k (cons (cps1 exp1 'identity '()) arg-acc) #:intr? flag-intr)))]

    [`(,exp) #:when (is-app-exp? (list exp))
             (let ([c1 (gensym 'v)])
               (if (is-app-exp? exp)
                   (cps1 exp `(lambda (,c1) ,(reverse (cons k (cons c1 arg-acc)))) '())
                   (reverse (cons k (cons (cps1 exp 'identity '()) arg-acc)))))]

    [`(,exp1 . ,rest) #:when (and (is-app-exp? exp) (not (null? rest)))
                      (let ([c1 (gensym 'v)])
                        (if (is-app-exp? exp1)
                            (cps1 exp1 `(lambda (,c1) ,(cps1 rest k (cons c1 arg-acc))) '())
                            (cps1 rest k (cons (cps1 exp1 'identity '()) arg-acc))))]

    [x #:when (or (atom? x) (eq? (car x) 'quote)) (wrap k x)]))

(define (cps exp) (cps1 exp 'ctx0 '()))

;; ==========================================
;; 单元测试集
;; ==========================================

(test-begin
  "CPS Transform Tests"

  ;; 1. 基本原子值
  (test-case "Atomic values"
    (check-equal? (cps '1) '(ctx0 1))
    (check-equal? (cps ''foo) '(ctx0 'foo))
    (check-equal? (cps 'x) '(ctx0 x)))

  ;; 2. 内置原语 (Primitives)
  (test-case "Primitives"
    (check-equal? (cps '(+ 1 2)) '(ctx0 (+ 1 2)))
    ;; 嵌套原语：先算内部，再算外部
    (check-equal? (cps '(+ 1 (* 2 3))) '(ctx0 (+ 1 (* 2 3)))))

  ;; 3. Lambda 定义
  (test-case "Lambda expression"
    ;; (lambda (x) x) => (ctx0 (lambda (x k) (k x)))
    (check-match (cps '(lambda (x) x))
                 `(ctx0 (lambda (x ,k) (,k x)))))

  ;; 4. 函数调用 (Application)
  (test-case "Function Application"
    ;; (f x) => (f x ctx0)
    (check-equal? (cps '(f x)) '(f x ctx0))
    ;; (f g) => (f g ctx0)  <-- 注意这里 g 被视为变量传递
    (check-equal? (cps '(f g)) '(f g ctx0)))

  ;; 5. 简单 IF (条件是原语)
  (test-case "Simple If (Atomic Condition)"
    ;; (if (null? x) 1 2)
    (check-match (cps '(if (null? x) 1 2))
                 `(if (null? x) (ctx0 1) (ctx0 2))))

  ;; 6. 复杂 IF (条件是函数调用)
  (test-case "Complex If (App Condition)"
    ;; (if (p x) 1 2)
    ;; 应该生成: (p x (lambda (v) (if v (ctx0 1) (ctx0 2))))
    (check-match (cps '(if (p x) 1 2))
                 `(p x (lambda (,v) (if ,v (ctx0 1) (ctx0 2))))))

  ;; 7. 序列 (Begin)
  (test-case "Begin Sequence"
    ;; (begin (f x) (g y))
    ;; 应该生成: (f x (lambda (_) (g y ctx0)))
    (check-match (cps '(begin (f x) (g y)))
                 `(f x (lambda (,_v) (g y ctx0)))))

  ;; 8. 嵌套原语与顺序 (Evaluation Order)
  (test-case "Evaluation Order"
    ;; (+ (f x) (g y))
    ;; 应该先算 f，再算 g，最后 +
    (check-match (cps '(+ (f x) (g y)))
                 `(f x (lambda (,r1)
                         (g y (lambda (,r2)
                                (ctx0 (+ ,r1 ,r2))))))))

  ;; 9. === 关键测试：Join Point Optimization (修正版) ===
  (test-case "Join Point Optimization (Code Explosion Avoidance)"

    (define complex-k '(lambda (res) (super-long-computation res)))

    ;; 目标: (let ((k complex-k)) (if (null? x) (k 1) (k 2)))
    ;; 注意: pattern 里的 let 绑定部分需要双层括号 ((,k-var ,val))

    (check-match (cps1 '(if (null? x) 1 2) complex-k '())
                 `(let ((,k-var ,val))
                    (if (null? ,x) (,k-var 1) (,k-var 2)))
                 "Should wrap complex continuation in a let"))

  ;; 10. 验证 Identity 优化
  (test-case "Identity Optimization"
    ;; 在 define 中会用到 'identity
    ;; (define x 1) -> (begin (define x 1) (ctx0 #f))
    ;; 内部递归调用 (cps1 1 'identity) 应该返回 1 而不是 ('identity 1)
    (check-match (cps '(define x 1))
                 `(begin (define x 1) (ctx0 #f))))

  )
