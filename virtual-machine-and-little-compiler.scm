(define (lookup name env)
  (cond
    ((null? env) (error 'lookup "Unbound variable" name))
    ((assq name (car env)) => cdr)
    (else (lookup name (cdr env)))))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch v cs ...)))
    ((_ v) (error 'pmatch "failed: ~s" v))
    ((_ v (else e0 e ...)) (begin e0 e ...))
    ((_ v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (* quote unquote)
    ((_ v * kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
	 (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))

(define (secd-run s e c d)
  (if (null? c) (car s)

    ; else
    (let ([ins (car c)]
          [rest-c (cdr c)])
      
      (pmatch ins
        ((LDC ,val) (secd-run (cons val s) e rest-c d))
        ((LD ,var) (secd-run (cons (lookup var e) s) e rest-c d))
        ((SEL ,t ,f) (if (car s) (secd-run (cdr s) e t (cons rest-c d)) (secd-run (cdr s) e f (cons rest-c d))))
        ((JOIN) (secd-run s e (car d) (cdr d)))
        ((LDF ,param ,body) (secd-run (cons (list 'closure param body e) s) e rest-c d))
        ((AP) (let ([func (car s)]
                    [arg (cadr s)]
                    [rest-s (cddr s)])
                
                (let ([param (cadr func)]
                      [body (caddr func)]
                      [env (cadddr func)])
                      
                  (let ([new-d (cons (list rest-s e rest-c) d)]
                        [new-env (cons (list (cons param arg)) env)])
                  
                    (secd-run '() new-env body new-d)))))

        ((RTN) (let* ([res (car s)]
                      [saved-state (car d)]
                      [rest-d (cdr d)]
                      [saved-s (car saved-state)]
                      [saved-e (cadr saved-state)]
                      [saved-c (caddr saved-state)])
                      
                  (secd-run (cons res saved-s) saved-e saved-c rest-d)))

        ((ADD) (secd-run (cons (+ (car s) (cadr s)) (cddr s)) e rest-c d))
        ((SUB) (secd-run (cons (- (cadr s) (car s)) (cddr s)) e rest-c d))
        ((MUL) (secd-run (cons (* (car s) (cadr s)) (cddr s)) e rest-c d))
        ((DIV) (secd-run (cons (/ (cadr s) (car s)) (cddr s)) e rest-c d))

        (else (error 'secd-run "Unknown instruction" ins))))))

(define (exec code)
  (secd-run '() '() code '()))

(define (compile-to-secd code)
    (pmatch code
      (,x (guard (or (number? x) (boolean? x))) (list (list 'LDC x)))
      (,x (guard (symbol? x)) (list (list 'LD x)))
      ((+ ,x ,y) (append (compile-to-secd x) (compile-to-secd y) '((ADD))))
      ((- ,x ,y) (append (compile-to-secd x) (compile-to-secd y) '((SUB))))
      (('* ,x ,y) (append (compile-to-secd x) (compile-to-secd y) '((MUL)))) ; 注意，改造之后 pmatch 将 * 作为了通配符
      ((/ ,x ,y) (append (compile-to-secd x) (compile-to-secd y) '((DIV))))
      ((lambda (,param) ,body) ((list 'LDF param (append (compile-to-secd body) '((RTN))))))
      ((,f ,arg) (append (compile-to-secd f) (compile-to-secd arg) '((AP))))
      ((if ,test ,t ,f) (append (compile-to-secd test) `((SEL ,(append (compile-to-secd t) '((JOIN))) ,(append (compile-to-secd f) '((JOIN)))))))
      (else (error 'compile-to-secd "Compile error" code))))

(define (all-compile code)
  (cond 
    ((null? code) '())
    (else (append (compile-to-secd (car code)) (all-compile (cdr code))))))

(define (run code)
  (exec (all-compile code)))    