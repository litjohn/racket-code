; (make-closure '(p1 p2) '(body) env)
(define (make-closure params body env)
  (vector 'closure params body env))

(define (closure? obj)
  (and (vector? obj) (eq? (vector-ref obj 0) 'closure)))

(define (closure-param closure) (vector-ref closure 1))
(define (closure-body closure) (vector-ref closure 2))
(define (closure-env closure) (vector-ref closure 3))

(define global-env (list '()))

; 接受一个参数名，一个参数值(会是个闭包)，和父环境
(define (extend-environment param arg base-env)
  (let ((frame (list (cons param arg)))) ; 创建只有一个绑定的新帧
    (cons frame base-env)))

(define (lookup-variable-value var env)
  (let loop ((e env))
    (if (null? e)
        (error "Unbound variable" var)
        (let ((frame (car e)))
          (cond
            ((assoc var frame) => cdr) ; R6RS 的 => 很有用
            (else (loop (cdr e))))))))

(define (define-variable! var val)
  ; define 总是修改最外层（全局）环境
  (set-car! global-env
    (cons (cons var val) (car global-env))))

(define (calc exp env)
  (cond
    ((symbol? exp)  (lookup-variable-value exp env))

    ((list? exp)
      (let ((op (car exp)))
        (cond 
          ((eq? op 'lambda)
            (let ((param (caadr exp))
                  (body (caddr exp)))

              (make-closure param body env)
            )
          )

          ((eq? op 'define)
          (let* ((var (cadr exp))
                 (val-exp (caddr exp))
                 (value (calc val-exp env)))
            (define-variable! var value)

            'ok))

          (else
            (let ((proc (calc (car exp) env)) ; 求值函数部分 -> 必须是个闭包
                  (arg (calc (cadr exp) env)))  ; 求值参数部分 -> 也是个值(闭包)
              (apply-proc proc arg)
            )
          )
        )
      )
    )

    (else (error "Invalid expression" exp))
  )
)

(define (apply-proc proc arg)
  (if (closure? proc)
      (let ((param (closure-param proc))
            (body (closure-body proc))
            (env (closure-env proc)))
        ; 关键步骤：在闭包 *捕获的环境* 上扩展新绑定
        (let ((new-env (extend-environment param arg env)))
          (calc body new-env)))
      (error "Not a procedure" proc)))

(define (run exp)
  (calc exp global-env))

(define (pretty-print obj)
  (cond
    ; 我们的主角：闭包
    ((closure? obj)
     (display "#<closure: (lambda (")
     (write (closure-param obj))
     (display ") ")
     (pretty-print (closure-body obj)) ; 递归地美化打印函数体
     (display ")>"))

    ; 对列表/S表达式进行美化
    ((pair? obj)
     (display "(")
     (pretty-print (car obj))
     (let loop ((rest (cdr obj)))
       (cond
         ((null? rest) (display ")"))
         ((pair? rest)
          (display " ")
          (pretty-print (car rest))
          (loop (cdr rest)))
         ; 处理点对列表，如 (a . b)
         (else
          (display " . ")
          (pretty-print rest)
          (display ")")))))

    ; 其他所有类型（符号、布尔值等）用默认方式打印
    (else (write obj))))

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