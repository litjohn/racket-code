(define-syntax define-dynamic
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name param) body)
       ;; 在这里，我们将 body 语法对象转换为普通的 datum (符号)。
       ;; 这样宏 'name' 的定义就会捕获这个 datum，而不是一个有词法上下文的语法对象。
         #'(define-syntax name
             (lambda (use-stx)
               (syntax-case use-stx ()
                 ((_ arg)
                 (let ((body-datum (syntax->datum #'body)))
                  #`(let ((#,(datum->syntax #'use-stx (syntax->datum #'param)) arg))
                      ;; 在这里，我们使用捕获的 datum 和 use-stx 的上下文
                      ;; 来创建一个新的、不卫生的标识符。
                      ;; 这个新创建的标识符将正确地查找到调用时的词法绑定。
                      #,(datum->syntax #'use-stx body-datum)))))))))))

(define-dynamic (f x) y)