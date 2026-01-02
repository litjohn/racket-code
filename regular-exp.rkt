#lang racket

;; ---------------------------------------------------------
;; 1. 定义正则表达式的 AST (抽象语法树)
;; ---------------------------------------------------------
(struct Eps () #:transparent)          ;; 空串 ε
(struct Void () #:transparent)         ;; 空集 φ (匹配失败)
(struct Chr (c) #:transparent)         ;; 字符 c
(struct Cat (e1 e2) #:transparent)     ;; 连接 e1 e2
(struct Alt (e1 e2) #:transparent)     ;; 选择 e1 | e2
(struct Star (e) #:transparent)        ;; 克林闭包 e*

(define (make-cat a b)
  (match* (a b)
    [((Void) _) (Void)]
    [(_ (Void)) (Void)]
    [((Eps) r) r]
    [(r (Eps)) r]
    [(_ _) (Cat a b)]))

(define (make-alt a b)
  (match* (a b)
    [((Void) _) b]
    [(_ (Void)) a]
    [(r r) r]
    [(_ _) (Alt a b)]))

(define (nullable? re)
  (match re
    [(Eps) #t]
    [(Void) #f]
    [(Chr _) #f]
    [(Star _) #t]
    [(Alt a b) (or (nullable? a) (nullable? b))]
    [(Cat a b) (and (nullable? a) (nullable? b))]))

(define (derive re c)
  (match re
    [(Void) (Void)]
    [(Eps) (Void)]
    [(Chr ch) (if (char=? ch c) (Eps) (Void))]
    [(Alt e1 e2)
     (make-alt (derive e1 c) (derive e2 c))]
    [(Cat e1 e2)
     (if (nullable? e1)
         (make-alt (make-cat (derive e1 c) e2) (derive e2 c))
         (make-cat (derive e1 c) e2))]
    [(Star e)
     (make-cat (derive e c) (Star e))]))

(define (make-matcher re)
  (define cache (make-hash))

  (define (match-loop cur s)
    (cond
      [(null? s) (nullable? cur)]
      [else
       (let ([c (car s)])
         (define next
           (hash-ref!
            (hash-ref! cache cur (lambda () (make-hash)))
            c
            (lambda ()
              (derive cur c))))

         (if (Void? next)
             #f
             (match-loop next (cdr s))))]))

  (lambda (str)
    (match-loop re (string->list str))))

;; ---------------------------------------------------------
;; 测试
;; ---------------------------------------------------------

;; 正则: a(b|c)*d
(define my-re
  (make-cat (Chr #\a)
          (make-cat (Star (make-alt (Chr #\b) (Chr #\c)))
                  (Chr #\d))))

(define matches? (make-matcher my-re))

(displayln "Test Cases:")
(displayln (matches? "ad"))       ;; #t
(displayln (matches? "abd"))      ;; #t
(displayln (matches? "abccbd"))   ;; #t
(displayln (matches? "add"))      ;; #f
(displayln (matches? "a"))        ;; #f

;; 压力测试：状态缓存机制
;; 第一次运行会构建缓存，第二次运行直接查表
(time (for ([i 10000]) (matches? "abcbcd")))
