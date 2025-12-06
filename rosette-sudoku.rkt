#lang rosette

; 0. 初始时等待完善的数独。0 代表空位。
(define org
  #(#(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)
    #(0 0 0 0 0 0 0 0 0)))

; 1. 定义 9x9 的符号变量矩阵
(define a
  (for/vector ([i (in-range 9)])
    (for/vector ([j (in-range 9)])
      (let ([x (vector-ref (vector-ref org i) j)])
        (if (zero? x)
           (begin
             (define-symbolic* sym integer?)
             sym)
           x)))))

; 2. 定义约束并求解
(define solution
  (solve
   (begin
     ; 约束 A: 所有数字必须在 1 到 9 之间
     (for* ([row a]
            [x row])
       (assert (and (>= x 1) (<= x 9))))

     ; 约束 B: 每一行必须互不相同 (Row restriction)
     (for ([row a])
       ; distinct? 接受变长参数，所以我们要把 vector 转为 list 并 apply
       (assert (apply distinct? (vector->list row))))

     ; 约束 C: 每一列必须互不相同
     (for ([j (in-range 9)])
       (define col
         (for/list ([i (in-range 9)])
           (vector-ref (vector-ref a i) j)))
       (assert (apply distinct? col)))

     ; 约束 D: 每个宫格不能有相同数字
     (for* ([i '(0 3 6)]
            [j '(0 3 6)])
       (assert
        (apply distinct?
               (for*/list ([k (in-range 3)]
                           [l (in-range 3)])
                 (vector-ref (vector-ref a (+ i k)) (+ j l)))))))))

; 3. 输出结果
(if (sat? solution)
    (let ([concrete-a (evaluate a solution)])
      (displayln "Found a solution:")
      (displayln concrete-a))
    (displayln "No solution!"))
