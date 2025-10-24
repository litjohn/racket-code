(define-syntax type-val
    (syntax-rules ()
        ((_ type val) (cons type val))))

(define-syntax is-this-type?
    (syntax-rules ()
        ((_ type val) (and (pair? val) (symbol? type) (eq? type (car val))))))