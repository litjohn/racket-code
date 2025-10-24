(define (make-heap less?)
  ;; "私有" 节点操作辅助函数 (无内部错误检查)
  (define (make-node element children)
    (cons element children))

  (define (get-element node)
    (car node)) ; 依赖 (car '()) 或 (car non-pair) 抛出运行时错误

  (define (get-children node)
    (cdr node)) ; 依赖 (cdr '()) 或 (cdr non-pair) 抛出运行时错误

  (define (set-children! node new-children)
    (set-cdr! node new-children)) ; 依赖 set-cdr! 对非cons对操作抛出运行时错误

  ;; "成员变量" - 堆本身，初始化为空
  (let ((heap-root '()))

    ;; "私有成员函数" - 实现配对堆的核心逻辑
    (define (internal-merge heap1 heap2)
      (cond
        ((null? heap1) heap2)
        ((null? heap2) heap1)
        ((less? (get-element heap1) (get-element heap2))
         (set-children! heap1 (cons heap2 (get-children heap1)))
         heap1)
        (else
         (set-children! heap2 (cons heap1 (get-children heap2)))
         heap2)))

    (define (internal-pre-process children-list)
      (cond ((null? children-list) '())
            ((null? (cdr children-list)) children-list)
            (else
             (cons (internal-merge (car children-list) (cadr children-list))
                   (internal-pre-process (cddr children-list))))))

    (define (internal-second-pass children-list)
      (cond
        ((null? children-list) '())
        (else
         (internal-merge (car children-list) (internal-second-pass (cdr children-list))))))

    ;; "公共成员函数"的内部实现
    (define (do-insert x)
      (set! heap-root (internal-merge heap-root (make-node x '()))))
      ;; 'insert' 操作不返回值 (或者说返回未指定值)

    (define (do-get-top)
      ;; 如果 heap-root 为 '(), (get-element heap-root) 即 (car '()) 会导致运行时错误
      (get-element heap-root))

    (define (do-pop)
      ;; 如果 heap-root 为 '(), (get-children heap-root) 即 (cdr '())
      ;; 会导致运行时错误 (在多数Scheme实现中)
      ;; 这符合"让运行时错误暴露问题"的原则
      (let ((children (get-children heap-root)))
        (set! heap-root (internal-second-pass (internal-pre-process children))))
      ;; 'pop' 操作不返回值 (或者说返回未指定值)
      )

    ;; 调度过程，模拟方法调用
    (lambda (message)
      (cond ((eq? message 'insert!) do-insert)
            ((eq? message 'get-top) (do-get-top))
            ((eq? message 'pop!) (do-pop))
            ((eq? message 'empty?) (null? heap-root))
            (else (error "PairingHeap: Unknown method --" message))))))