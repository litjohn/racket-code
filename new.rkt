#lang typed/racket

(define-type Tree (U Integer (Listof Tree)))

(: sum-tree (-> Tree Integer))
(define (sum-tree t)
  (cond
    ;; 分支 1：编译器自动推断此处 t 为 Integer
    [(integer? t) t]
    ;; 分支 2：因为排除了 Integer，编译器推断此处 t 必然是 (Listof Tree)
    ;; 所以 map 和 apply + 可以安全使用
    [else (apply + (map sum-tree t))]))

(sum-tree '(1 (2 3) (4 (5)))) ; -> 15
