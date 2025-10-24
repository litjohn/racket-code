;; 在环境中查找变量 v 的值
(define (lookup v env)
  (let loop ((e env))
    (if (null? e)
        (error "Unbound variable:" v)
        (let ((frame (car e)))
          (let ((binding (assoc v frame)))
            (if binding
                (cdr binding)
                (loop (cdr e))))))))

(define (secd-run s e c d)
  ;; 当控制列表 C 为空时，机器停止，结果在栈 S 顶
  (if (null? c)
      (if (null? s)
          '()
          (car s))
      ;; 取出当前指令和剩余指令
      (let ((instr (car c))
            (rest-c (cdr c)))
        (let ((op (car instr)))
          ;; 根据指令类型进行分发
          (case op
            ;; --- 核心指令实现 ---

            ((LDC) ; Load Constant: (LDC k)
             (secd-run (cons (cadr instr) s) e rest-c d))

            ((LD)  ; Load Variable: (LD v)
             (secd-run (cons (lookup (cadr instr) e) s) e rest-c d))

            ((ADD) ; Add
             (let ((v1 (car s))
                   (v2 (cadr s))
                   (rest-s (cddr s)))
               (secd-run (cons (+ v1 v2) rest-s) e rest-c d)))

            ((SUB) ; Subtract
             (let ((v1 (car s))
                   (v2 (cadr s))
                   (rest-s (cddr s)))
               (secd-run (cons (- v2 v1) rest-s) e rest-c d))) ; 注意顺序 v2 - v1

            ((MUL) ; Multiply
             (let ((v1 (car s))
                   (v2 (cadr s))
                   (rest-s (cddr s)))
               (secd-run (cons (* v1 v2) rest-s) e rest-c d)))

            ((SEL) ; Select (if/then/else): (SEL then-code else-code)
             (let ((test (car s))
                   (rest-s (cdr s))
                   (then-branch (cadr instr))
                   (else-branch (caddr instr)))
               ;; 保存 C 的剩余部分 (continuation)，以便分支结束后继续
               (let ((new-d (cons rest-c d)))
                 (if test
                     (secd-run rest-s e then-branch new-d)
                     (secd-run rest-s e else-branch new-d)))))

            ((JOIN) ; Join from a branch
             ;; 从 Dump 中恢复 continuation
             (let ((saved-c (car d))
                   (rest-d (cdr d)))
               (secd-run s e saved-c rest-d)))

            ((LDF) ; Load Function: (LDF (params . body-code))
             ;; 创建闭包：(closure (params . body) env)
             (let ((closure (list 'closure (cadr instr) e)))
               (secd-run (cons closure s) e rest-c d)))

            ((AP)  ; Apply Function
             (let ((closure (car s))
                   (arg (cadr s))
                   (rest-s (cddr s)))
               (let ((code (cadr closure))
                     (def-env (caddr closure)))
                 (let ((params (car code))
                       (body (cdr code)))
                   ;; 保存现场
                   (let ((new-d (cons (list rest-s e rest-c) d)))
                     ;; 创建新环境：在函数定义时的环境上扩展
                     (let ((new-env (cons (list (cons (car params) arg)) def-env)))
                       ;; 进入函数体
                       (secd-run '() new-env body new-d)))))))

            ((RTN) ; Return from function
             (let ((return-val (car s))
                   (saved-state (car d))
                   (rest-d (cdr d)))
               (let ((saved-s (car saved-state))
                     (saved-e (cadr saved-state))
                     (saved-c (caddr saved-state)))
                 ;; 恢复现场，并将返回值压入恢复后的栈
                 (secd-run (cons return-val saved-s) saved-e saved-c rest-d))))

            (else (error "Unknown instruction:" op)))))))

(define (compile exp)
  (cond
    ;; 常量
    ((or (number? exp) (boolean? exp) (string? exp) (vector? exp)) `((LDC ,exp)))
    ;; 变量
    ((symbol? exp) `((LD ,exp)))
    ;; 列表（函数调用或其他形式）
    ((list? exp)
     (case (car exp)
       ((lambda) ; (lambda (param) body)
        (let ((params (cadr exp))
              (body (caddr exp)))
          ;; 函数体编译后需要一个 RTN 指令
          `((LDF ,(cons params (append (compile body) '((RTN))))))))

       ((if) ; (if test then else)
        (let ((test-exp (cadr exp))
              (then-exp (caddr exp))
              (else-exp (cadddr exp)))
          ;; 每个分支结束后都需要一个 JOIN 指令
          (append (compile test-exp)
                  `((SEL ,(append (compile then-exp) '((JOIN)))
                         ,(append (compile else-exp) '((JOIN))))))))

       ((+) `(,@(compile (caddr exp)) ,@(compile (cadr exp)) (ADD)))
       ((-) `(,@(compile (caddr exp)) ,@(compile (cadr exp)) (SUB)))
       ((*) `(,@(compile (caddr exp)) ,@(compile (cadr exp)) (MUL)))

       ;; 函数调用 (f arg)
       (else
        (let ((func (car exp))
              (arg (cadr exp)))
          ;; 先计算参数，再计算函数，最后调用
          (append (compile arg)
                  (compile func)
                  '((AP)))))))
    (else (error "Cannot compile:" exp))))

;; 定义一个顶层函数来运行代码
(define (run exp)
  (let ((code (compile exp)))
    (secd-run '() '() code '())))

;; --- 示例 ---

;; 示例 1: 简单算术
(display "Running (* (+ 2 3) 4)...") (newline)
(display (run '(* (+ 2 3) 4))) (newline) ; 应该输出 20

;; 示例 2: 简单函数调用
(display "Running ((lambda (x) (+ x 10)) 5)...") (newline)
(display (run '((lambda (x) (+ x 10)) 5))) (newline) ; 应该输出 15

;; 示例 3: 闭包和词法作用域
(display "Running (((lambda (x) (lambda (y) (+ x y))) 10) 20)...") (newline)
(display (run '(((lambda (x) (lambda (y) (+ x y))) 10) 20))) (newline) ; 应该输出 30

;; 示例 4: 条件表达式
(display "Running (if #t ((lambda (x) x) 100) 200)...") (newline)
(let ((code `(if #t ((lambda (x) x) 100) 200)))
  (display (run code)) (newline)) ; 应该输出 100