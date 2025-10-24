; (make-closure '(p1 p2) '(body) env)
(define (make-closure params body env)
  (vector 'closure params body env))

(define (make-closure-with-name params body env name)
  (vector 'closure params body env name))    

(define (closure? obj)
  (and (vector? obj) (eq? (vector-ref obj 0) 'closure)))

(define (closure-param closure) (vector-ref closure 1))
(define (closure-body closure) (vector-ref closure 2))
(define (closure-env closure) (vector-ref closure 3))
(define (closure-name closure) (vector-ref closure 4))  

(define global-env (list '()))
(define intrinsics (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /) (cons '= =) (cons '< <) (cons '> >) (cons '>= >=) (cons '<= <=) (cons 'not not) (cons 'cons cons) (cons 'car car) (cons 'cdr cdr) (cons 'map map) (cons 'filter filter)
  (cons 'for-each for-each) (cons 'vector-ref vector-ref) (cons 'vector-set! vector-set!) (cons 'vector vector) (cons 'list list))) ; 内建函数。用于支持对于数字的操作等等。可以自行添加。

(define (new-environment bounds base-env)
  (cons bounds base-env))

(define (lookup-variable-value var env)
  (let loop ((e env))
    (if (null? e)
        (error var "Unbound variable")
        (let ((frame (car e)))
          (cond
            ((assq var frame) => cdr) ; R6RS 的 => 很有用
            (else (loop (cdr e))))))))

(define (modify var val env)
  (let loop ((e env))
    (if (null? e)
        (error var "Unbound variable")
        (let ((frame (car e)))
          (cond
            ((assq var frame) => (lambda (v) (set-cdr! v val)))
            (else (loop (cdr e))))))))

(define (define-variable! var val)
  ; 合法性检查：变量名必须是符号
  (when (not (symbol? var))
    (error var "Variable name must be a symbol"))

  ; define 总是修改最外层（全局）环境
  (set-car! global-env
    (cons (cons var val) (car global-env))))

(define (calc exp env)
  (cond
    ((or (number? exp) (string? exp) (vector? exp) (boolean? exp)) exp) ; 在这个解释器中，数字等原始数据结构也可以视为一种 intrinsics.
    ((and (list? exp) (assq (car exp) intrinsics)) => 
     (lambda (x)
      (let ([f (cdr x)])
        (apply f 
          (map 
            (lambda (v) (calc v env)) 
            (cdr exp)))))) ; 注意，操作数数量不匹配会由 scheme 自己报错。所以为了省事略去了。

    ((symbol? exp) (lookup-variable-value exp env))

    ((list? exp)
      (let ((op (car exp)))
        (cond 
          ((eq? op 'lambda)
            (let ((param (cadr exp))
                  (body (cons 'begin (cddr exp))))

              (make-closure param body env)))

          ((eq? op 'define)
            (let* ((var (cadr exp))
                  (val-exp (caddr exp))
                  (value (calc val-exp env)))

              (if (closure? value)
                (let ([param (closure-param value)]
                      [body (closure-body value)]
                      [env (closure-env value)])
                  (define-variable! var (make-closure-with-name param body env var)))
                (define-variable! var value))))

          ((eq? op 'let)
            (calc (cons 'begin (cddr exp))
                  (new-environment 
                    (map 
                      (lambda (x) (cons (car x) (calc (cadr x) env))) 
                      (cadr exp)) 
                      env)))

          ((eq? op 'set!)
            (if (not (symbol? (cadr exp))) 
              (error exp "Invalid syntax set!")
              (modify (cadr exp) (calc (caddr exp) env) env)))

          ((eq? op 'begin)
            (let loop ([x (cdr exp)])
              (cond
                ((null? x) (error exp "Invalid syntax begin"))
                ((null? (cdr x)) (calc (car x) env))
                (else (calc (car x) env) (loop (cdr x))))))

          ((eq? op 'quote)
            (if (or (null? (cdr exp)) (not (null? (cddr exp))))
              (error exp "Invalid syntax quote")
              (cadr exp)))
          
          ((eq? op 'if)
           (cond
            ((or (null? (cdr exp)) (null? (cddr exp))) (error exp "Invalid syntax if"))
            ((null? (cdddr exp)) (if (calc (cadr exp) env) (calc (caddr exp) env)))
            ((null? (cddddr exp))
              (if (calc (cadr exp) env) 
                (calc (caddr exp) env)
                (calc (cadddr exp) env)))
                
            (else (error exp "Invalid syntax if"))))

          (else
            (let ((proc (calc (car exp) env)) ; 求值函数部分 -> 必须是个闭包
                  (arg (map (lambda (x) (calc x env)) (cdr exp))))  ; 求值参数部分 -> 也是个值(闭包)
              (apply-proc proc arg))))))

    (else (error exp "Invalid expression"))))

(define (apply-proc proc arg)
  (if (closure? proc)
      (let ((param (closure-param proc))
            (body (closure-body proc))
            (env (closure-env proc)))
        ; 关键步骤：在闭包 *捕获的环境* 上扩展新绑定
        (let ((new-env 
              (new-environment
                (map cons param arg) 
                env)))

          (calc body new-env)))
      (error proc "Not a procedure")))

(define (run exp)
  (calc exp global-env))

(define (pretty-print obj)
  (cond
    ; 我们的主角：闭包
    ((closure? obj)
     (if (= (vector-length obj) 4) (display "#<procedure>") (begin (display "#<procedure ") (display (closure-name obj)) (display ">"))))

    ; 其他所有类型（符号、布尔值等）用默认方式打印
    (else (display obj))))

(define (repl)
  (display "Lambda> ")
  (let ((exp (read)))
    (if (eq? exp 'exit)
        (display "Goodbye!")
        (begin
          (let ((result (run exp))) ; <-- 使用 run 而不是 calc
            (pretty-print result)
            (newline))
          (repl)))))