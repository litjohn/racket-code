; 原始全局向量，用于标记是否为素数候选
(define is_prime (make-vector 100001 #t))

; 增量修改：显式将0和1标记为非素数
; 这是一个好的实践，确保is_prime数组的意义更清晰
(vector-set! is_prime 0 #f)
(vector-set! is_prime 1 #f)

; 原始 set-loop! 函数，用于将x的倍数标记为非素数
; 这个函数本身不需要改变，因为它接收起始点i
(define (set-loop! i n x)
    (when (<= i n)
        (vector-set! is_prime i #f)
        (set-loop! (+ i x) n x)))

; 修改后的 solve 函数
(define (solve x n)
    ; 当x仍然被认为是素数时 (vector-ref is_prime x)
    ; 并且 x 在我们考虑的范围内 (隐式地，因为x从2开始，到n结束)
    (when (vector-ref is_prime x)
        ; 性能优化：从 x*x 开始标记x的倍数
        ; set-loop! 内部会检查 i 是否超出 n
        (set-loop! (* x x) n x)
        ;移除了 (display x) 和 (newline)
    )

    ; 递归处理下一个数，直到 x 达到 n
    (when (< (* x x) n)
        (solve (+ x 1) n)))

; --- 执行筛选 ---
; 定义筛选上限
(define N_LIMIT 100) ; 使用 N_LIMIT 替代硬编码的 100，更清晰

; 调用 solve 函数执行筛选，这会修改全局的 is_prime 向量
(solve 2 N_LIMIT)

; --- 将筛选结果收集到新的向量中 ---
; 辅助函数：收集在 is_prime 向量中标记为 #t (素数) 的数
(define (collect-sieved-primes top-n prime-sieve-vector)
    ; 内部辅助函数：首先计算有多少素数，以确定结果向量的大小
    (define (count-primes-recursive current-num end-num count)
        (if (> current-num end-num)
            count
            (count-primes-recursive
                (+ current-num 1)
                end-num
                (if (vector-ref prime-sieve-vector current-num)
                    (+ count 1)
                    count))))

    (let ((num-found-primes (count-primes-recursive 2 top-n 0))) ; 素数从2开始计数
        (let ((result-vector (make-vector num-found-primes)))
            ; 内部辅助函数：填充结果向量
            (define (populate-primes-recursive sieve-idx result-idx)
                (cond ((>= result-idx num-found-primes) ; 如果结果向量已填满
                       result-vector)
                      ((> sieve-idx top-n) ; 如果已检查完所有候选数 (理论上不应先于上一个条件)
                       result-vector)
                      ((vector-ref prime-sieve-vector sieve-idx) ; 如果当前sieve-idx是素数
                       (vector-set! result-vector result-idx sieve-idx)
                       (populate-primes-recursive (+ sieve-idx 1) (+ result-idx 1)))
                      (else ; 如果当前sieve-idx不是素数，跳过
                       (populate-primes-recursive (+ sieve-idx 1) result-idx))))
            ; 从数字2开始检查，填充到结果向量的索引0开始
            (populate-primes-recursive 2 0))))

; 定义一个新的向量 result_primes_vector 来存储找到的素数
(define result_primes_vector (collect-sieved-primes N_LIMIT is_prime))

; (可选) 打印结果向量以验证 (这部分是新增的，用于查看结果)
; (display result_primes_vector)
; (newline)
; (display "Number of primes found: ")
; (display (vector-length result_primes_vector))
; (newline)