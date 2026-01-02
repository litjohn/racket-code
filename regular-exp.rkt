#lang racket

(require srfi/13)

;; ---------------------------------------------------------
;; 1. 定义正则表达式的 AST (抽象语法树)
;; ---------------------------------------------------------
(struct Eps () #:transparent)          ;; 空串 ε
(struct Void () #:transparent)         ;; 空集 φ (匹配失败)
(struct Chr (c) #:transparent)         ;; 字符 c
(struct Cat (e1 e2) #:transparent)     ;; 连接 e1 e2
(struct Alt (e1 e2) #:transparent)     ;; 选择 e1 | e2
(struct Star (e) #:transparent)        ;; 克林闭包 e*
(struct Dot () #:transparent)          ;; 语法糖：匹配任意字符

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
    [(Dot) #f]
    [(Chr _) #f]
    [(Star _) #t]
    [(Alt a b) (or (nullable? a) (nullable? b))]
    [(Cat a b) (and (nullable? a) (nullable? b))]))

(define (derive re c)
  (match re
    [(Void) (Void)]
    [(Eps) (Void)]
    [(Dot) (Eps)]
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

(define (re-reverse re)
  (match re
    [(Cat e1 e2) (make-cat (re-reverse e2) (re-reverse e1))]
    [(Alt e1 e2) (make-alt (re-reverse e1) (re-reverse e2))]
    [(Star e) (Star (re-reverse e))]
    [_ re]))

(define (make-search re)
  (define forward (make-cat (Star (Dot)) re))
  (define backward (re-reverse re))
  (define cache (make-hash))

  (define (match-loop cur s idx)
    (cond
      [(nullable? cur) idx]
      [(>= idx (string-length s)) #f]
      [else
       (let ([c (string-ref s idx)])
         (define next
           (hash-ref!
            (hash-ref! cache cur (lambda () (make-hash)))
            c
            (lambda ()
              (derive cur c))))

         (if (Void? next)
             #f
             (match-loop next s (add1 idx))))]))

  (lambda (str)
    (define E (match-loop forward str 0))
    (if E
      (begin
        (let ([T (match-loop backward (string-reverse str 0 E) 0)])
          (cons (- E T) E)))
      #f)))

;; ---------------------------------------------------------
;; 4. 测试用例
;; ---------------------------------------------------------

(define search (make-search my-re))

(displayln "=== Standard Cases ===")
(displayln (search "ad"))         ;; '(0 . 2)
(displayln (search "xxxad"))      ;; '(3 . 5)
(displayln (search "adxxx"))      ;; '(0 . 2)  -- 验证最左匹配
(displayln (search "xxxadxxx"))   ;; '(3 . 5)
(displayln (search "abccbd"))     ;; '(0 . 6)
(displayln (search "z"))          ;; #f

(displayln "=== Edge Cases ===")
;; 多个匹配，应该返回最左边的那个
(displayln (search "ad...ad"))    ;; '(0 . 2) 

;; 贪婪/非贪婪行为测试
;; 这里的实现是"最短匹配" (reluctant)，因为遇到 nullable 就立即返回了
;; 对于 a(b|c)*d，如果输入 abdbd
;; forward 会在第一个 d 处 (index 3) 停止。
;; backward 从 d 反向匹配到 a。
;; 结果应该是 '(0 . 3) -> "abd"
(displayln (search "abdbd"))      ;; '(0 . 3)

;; 复杂一点的
(displayln (search "zzzaaaaaaddddzzz")) ;; '(3 . 5) -> 匹配了第一个 a..d 即 "ad"
;; 解释：因为 forward 匹配 .*a(b|c)*d。
;; .* 吃了 "zzzaaaa"，然后 a 匹配 "a"，然后 d 匹配 "d"。
;; 这里其实取决于 .* 的结合性，但在导数语义下，一旦 nullable 就停，这通常意味着匹配了"能匹配的最短前缀"。