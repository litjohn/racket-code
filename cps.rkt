#lang racket

(define (atom? x) (not (pair? x)))

(define intrinsics (hash '+ #t '- #t '* #t '/ #t 'cons #t 'car #t 'cdr #t 'null? #t '= #t '< #t '> #t '<= #t '>= #t))
(define (is-intrinsic? op) (hash-has-key? intrinsics op))

(define (wrap k exp)
    (if (eq? k 'identity)
        exp
        (list k exp)))

(define keywords '(lambda define begin quote))

(define (is-app-exp? exp)
    (not (or (atom? exp) (member (car exp) keywords) (is-intrinsic? (car exp)))))

(define (cps1 exp k arg-acc #:intr? [flag-intr #f])
    (match exp
        [`(define ,id ,exp)
          `(begin (define ,id ,(cps1 exp 'identity '())) (,k #f))]
    
        [`(lambda ,bind . ,body) 
            (let ([res (let ([c (gensym 'k)])
                            `(lambda ,(append bind (list c)) ,(cps1 (cons 'begin body) c '())))])
                        
                (wrap k res))]

        [`(begin ,exp)
          (cps1 exp k '())]

        [`(begin ,exp . ,rest) #:when (not (null? rest))
          (let ([c1 (gensym 'v)])
            (cps1 exp `(lambda (,c1) ,(cps1 (cons 'begin rest) k '())) '()))]

        [`(if ,condition ,then-c ,else-c)
            (let ([c (gensym 'v)])
                (if (is-app-exp? condition)
                    (cps1 condition `(lambda (,c) (if ,c ,(cps1 then-c k '()) ,(cps1 else-c k '()))) '())
                    `(if ,(cps1 condition 'identity '()) ,(cps1 then-c k '()) ,(cps1 else-c k '()))))]

        [`(,intrin . ,rest) #:when (is-intrinsic? intrin)
          (cps1 rest k '() #:intr? intrin)]

        [`(,exp) #:when flag-intr
            (let ([c1 (gensym 'v)])
                (if (is-app-exp? exp)
                    (cps1 exp `(lambda (,c1) ,(wrap k (cons flag-intr (reverse (cons c1 arg-acc))))) '())
                    (wrap k (cons flag-intr (reverse (cons (cps1 exp 'identity '()) arg-acc))))))]

        [`(,exp1 . ,rest) #:when (and flag-intr (not (null? rest)))
            (let ([c1 (gensym 'v)])
                (if (is-app-exp? exp1)
                    (cps1 exp1 `(lambda (,c1) ,(cps1 rest k (cons c1 arg-acc) #:intr? flag-intr)) '())
                    (cps1 rest k (cons (cps1 exp1 'identity '()) arg-acc) #:intr? flag-intr)))]

        [`(,exp) #:when (is-app-exp? (list exp))
            (let ([c1 (gensym 'v)])
                (if (is-app-exp? exp)
                    (cps1 exp `(lambda (,c1) ,(reverse (cons k (cons c1 arg-acc)))) '())
                    (reverse (cons k (cons (cps1 exp 'identity '()) arg-acc)))))]

        [`(,exp1 . ,rest) #:when (and (is-app-exp? exp) (not (null? rest)))
            (let ([c1 (gensym 'v)])
                (if (is-app-exp? exp1)
                    (cps1 exp1 `(lambda (,c1) ,(cps1 rest k (cons c1 arg-acc))) '())
                    (cps1 rest k (cons (cps1 exp1 'identity '()) arg-acc))))]

        [x #:when (or (atom? x) (eq? (car x) 'quote)) (wrap k x)]))

(define (cps exp) (cps1 exp 'ctx0 '()))

(provide cps)