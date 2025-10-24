;; 使用闭包模拟的线段树节点对象
(define (make-node l-son r-son initial-sum)
  (let ((sum initial-sum)      ; 区间和
        (tag 0)                ; 懒标记 (lazy tag)
        (children (cons l-son r-son))) ; 子节点

    ;; 方法分派器
    (lambda (command)
      (cond
        ((eq? command 'ls) (car children))
        ((eq? command 'rs) (cdr children))
        ((eq? command 'get-sum) sum)

        ((eq? command 'apply-add)
         (lambda (l r c)
           (set! sum (+ sum (* c (- r l -1))))
           (set! tag (+ tag c))))

        ((eq? command 'pushdown)
         (lambda (l r)
           (when (not (= tag 0))
             (let ((mid (quotient (+ l r) 2))
                   (l-son (car children))
                   (r-son (cdr children)))
               (when l-son ((l-son 'apply-add) l mid tag))
               (when r-son ((r-son 'apply-add) (+ mid 1) r tag))
               (set! tag 0)))))

        ((eq? command 'maintain)
         (let ((l-son (car children)))
           (when l-son
             (let ((r-son (cdr children)))
               (set! sum (+ (l-son 'get-sum) (r-son 'get-sum)))))))
        ))))

;; 全局变量
(define rt)
(define a)

;; [辅助] 递归建树 (修正版)
(define (build s t)
  (if (= s t)
      ;; 基本情况：叶子节点，sum 直接来自初始数组
      (make-node '() '() (vector-ref a s))
      ;; 递归情况：内部节点
      (let* ((mid (quotient (+ s t) 2))
             (l-son (build s mid))
             (r-son (build (+ mid 1) t)))

        ;; 直接用子节点的和来创建父节点，一步到位
        (make-node l-son r-son (+ (l-son 'get-sum) (r-son 'get-sum))))))

;; [核心] 区间更新
(define (update node s t L R val)
  (if (and (<= L s) (<= t R))
      ((node 'apply-add) s t val)
      (begin
        (let ((mid (quotient (+ s t) 2)))
          ((node 'pushdown) s t)
          (when (<= L mid)
            (update (node 'ls) s mid L R val))
          (when (> R mid)
            (update (node 'rs) (+ mid 1) t L R val))
          ;; 在更新后，需要维护当前节点的状态
          (node 'maintain)))))

;; [核心] 区间查询
(define (query node s t L R)
  (if (and (<= L s) (<= t R))
      (node 'get-sum)
      (let ((mid (quotient (+ s t) 2))
            (res 0))
        ((node 'pushdown) s t)
        (when (<= L mid)
          (set! res (+ res (query (node 'ls) s mid L R))))
        (when (> R mid)
          (set! res (+ res (query (node 'rs) (+ mid 1) t L R))))
        res)))

;; 主函数
(define (main)
  (define n (read))
  (define q (read))

  (set! a (make-vector (+ n 1) 0))
  (let input-loop ((i 1))
    (when (<= i n)
      (vector-set! a i (read))
      (input-loop (+ i 1))))

  (set! rt (build 1 n))

  (let query-loop ((i 1))
    (when (<= i q)
      (let ((op (read)))
        (cond
          ((= op 1)
           (let* ((l (read)) (r (read)) (x (read))) ; 坑点：注意用 let* 而不是 let，因为 let 的绑定顺序未定义，可能会把 l 和 r 读反
             (update rt 1 n l r x)))
          ((= op 2)
           (let* ((l (read)) (r (read)))
             (display (query rt 1 n l r))
             (newline)))))
      (query-loop (+ i 1)))))

;; 程序入口
(main)