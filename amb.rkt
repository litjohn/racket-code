#lang racket

;; 全局的失败续延栈
(define failure-k #f)

;; amb 操作符，从一组 choices 中不确定地选择一个
(define (amb . choices)
  (if (null? choices)
      (fail) ; 如果没有选项，就失败
      (call/cc
       (lambda (k)
         ;; (k (car choices)) 会立即返回第一个选项
         ;; 但在返回之前，我们先设置好“如果这个选项不行，该怎么办”
         ;; 怎么办？就是尝试剩下的选项 (cdr choices)
         (set! failure-k
               (let ([old-failure-k failure-k])
                 (lambda ()
                   (set! failure-k old-failure-k) ; 恢复上一层的 failure-k
                   (k (apply amb (cdr choices)))))) ; 调用续延 k，让 amb 返回下一个选项
         (car choices)))))

;; 失败函数，调用它会回溯到上一个 amb 选择点
(define (fail)
  (if failure-k
      (failure-k)
      (error "amb: no more choices")))

;; 一个顶层函数来运行带 amb 的代码
(define (run-amb-search thunk)
  (let ([old-failure-k failure-k])
    (set! failure-k (lambda () (error "amb: search failed")))
    (let ([result (thunk)])
      (set! failure-k old-failure-k)
      result)))

;; 一个辅助函数，用于表达“必须满足这个条件”
(define (req condition)
  (when (not condition) (fail)))

;; tests

  (define (find-pythagorean-triple-amb)
  (run-amb-search
   (lambda ()
     ;; 1. 不确定地选择一个 1-10 的整数 i
     (define i (apply amb (range 1 11)))
     ;; 2. 不确定地选择一个 i-10 的整数 j
     (define j (apply amb (range i 11)))
     ;; 3. 不确定地选择一个 j-10 的整数 k
     (define k (apply amb (range j 11)))

     ;; 4. 要求必须满足勾股定理
     (req (= (+ (* i i) (* j j)) (* k k)))

     ;; 5. 如果代码能执行到这里，说明所有要求都满足了，返回结果
     (list i j k))))

(displayln (find-pythagorean-triple-amb)) ; 输出 '(3 4 5)