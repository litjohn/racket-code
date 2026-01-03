#lang racket

(require srfi/13)

;; 1. 定义 AST，通过 gen:equal+hash 强制 O(1) 比较
;; 只要对象是驻留的，equal? 逻辑就等同于 eq?
(struct Eps () #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Void () #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Chr (c) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Cat (e1 e2) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Alt (e-list) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Star (e) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

(struct Dot () #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recur) (eq? a b))
   (define (hash-proc a recur) (eq-hash-code a))
   (define (hash2-proc a recur) (eq-hash-code a))])

;; 2. 驻留池 (Weak Hash Table)
;; 注意：池的 Key 必须使用 vector，因为 vector 的 equal? 会递归检查其内容
;; 而内容（子节点）已经是驻留的，所以比较子节点是 O(1) 的 eq?
(define pool (make-weak-hash))

(define (intern key constructor)
  (hash-ref! pool key constructor))

;; 基础常量
(define phi (intern (vector 'Void) (λ () (Void))))
(define eps (intern (vector 'Eps) (λ () (Eps))))
(define dot (intern (vector 'Dot) (λ () (Dot))))

;; 3. 辅助排序函数：利用 eq-hash-code 实现常数时间全序
(define (re-type-rank r)
  (cond [(Void? r) 0] [(Eps? r) 1] [(Dot? r) 2] [(Chr? r) 3]
        [(Star? r) 4] [(Cat? r) 5] [(Alt? r) 6]))

(define (re-compare r1 r2)
  (let ([t1 (re-type-rank r1)] [t2 (re-type-rank r2)])
    (if (= t1 t2)
        (< (eq-hash-code r1) (eq-hash-code r2))
        (< t1 t2))))

;; 4. 智能构造函数
(define (make-chr c)
  (intern (vector 'Chr c) (λ () (Chr c))))

(define (make-star e)
  (cond [(Void? e) eps]
        [(Eps? e) eps]
        [(Star? e) e]
        [else (intern (vector 'Star e) (λ () (Star e)))]))

(define (make-cat e1 e2)
  (cond [(or (Void? e1) (Void? e2)) phi]
        [(Eps? e1) e2]
        [(Eps? e2) e1]
        [else (intern (vector 'Cat e1 e2) (λ () (Cat e1 e2)))]))

(define (make-alt l)
  (define (flatten es)
    (append-map (λ (x) (if (Alt? x) (Alt-e-list x) (list x))) es))

  (let* ([flat (flatten l)]
         [no-phi (filter-not Void? flat)]
         ;; 使用 eq-hash-code 排序，确保 ACI 规范化
         [sorted (sort no-phi re-compare)]
         [unique (remove-duplicates sorted eq?)])
    (cond
      [(null? unique) phi]
      [(null? (cdr unique)) (car unique)]
      [else (intern (vector 'Alt unique) (λ () (Alt unique)))])))

;; 5. 导数与判空
(define (nullable? re)
  (match re
    [(Eps) #t]
    [(Star _) #t]
    [(Alt es) (ormap nullable? es)]
    [(Cat e1 e2) (and (nullable? e1) (nullable? e2))]
    [_ #f]))

(define (derive re c)
  (match re
    [(Void) phi]
    [(Eps) phi]
    [(Dot) eps]
    [(Chr ch) (if (char=? ch c) eps phi)]
    [(Alt es) (make-alt (map (λ (e) (derive e c)) es))]
    [(Cat e1 e2)
     (if (nullable? e1)
         (make-alt (list (make-cat (derive e1 c) e2) (derive e2 c)))
         (make-cat (derive e1 c) e2))]
    [(Star e) (make-cat (derive e c) re)]))

;; ---------------------------------------------------------
;; 1. 匹配器构造 (完全匹配)
;; ---------------------------------------------------------
(define (make-matcher re)
  (define cache (make-hash)) ;; 状态转移缓存: cur -> char -> next
  (lambda (str)
    (let loop ([cur re] [s (string->list str)])
      (if (null? s)
          (nullable? cur)
          (let ([next (hash-ref! (hash-ref! cache cur (λ () (make-hash)))
                                 (car s)
                                 (λ () (derive cur (car s))))])
            (if (Void? next)
                #f
                (loop next (cdr s))))))))

;; ---------------------------------------------------------
;; 2. 正则表达式反转
;; ---------------------------------------------------------
(define (re-reverse re)
  (match re
    [(Cat e1 e2) (make-cat (re-reverse e2) (re-reverse e1))]
    [(Alt es)    (make-alt (map re-reverse es))]
    [(Star e)    (make-star (re-reverse e))]
    [_             re]))

;; ---------------------------------------------------------
;; 3. 搜索器构造 (子串匹配: 最左最短)
;; ---------------------------------------------------------
(define (make-search re)
  ;; 前向搜索: 匹配 .*RE
  (define forward-re (make-cat (make-star dot) re))
  ;; 后向搜索: 匹配 RE_reversed
  (define backward-re (re-reverse re))

  (define f-cache (make-hash))
  (define b-cache (make-hash))

  (define (step cache cur c)
    (hash-ref! (hash-ref! cache cur (λ () (make-hash)))
               c
               (λ () (derive cur c))))

  (lambda (str)
    (let* ([chars (string->list str)])
      ;; 1. 前向扫描：寻找第一个能让 (.*RE) 变成 nullable 的位置 E
      (define E
        (let f-loop ([cur forward-re] [s chars] [idx 0])
          (cond
            [(nullable? cur) idx] ;; 找到匹配终点
            [(null? s) #f]
            [else
             (let ([next (step f-cache cur (car s))])
               (if (Void? next) #f (f-loop next (cdr s) (add1 idx))))])))

      (if E
          ;; 2. 后向扫描：从 E 位置开始向左匹配 RE_rev，寻找最短的起点
          ;; 提取前 E 个字符并反转
          (let* ([prefix-rev (reverse (take chars E))]
                 [T (let b-loop ([cur backward-re] [s prefix-rev] [idx 0])
                      (cond
                        [(nullable? cur) idx] ;; 找到匹配起点（距离 E 的偏移）
                        [(null? s) (if (nullable? cur) idx #f)]
                        [else
                         (let ([next (step b-cache cur (car s))])
                           (if (Void? next) #f (b-loop next (cdr s) (add1 idx))))]))])
            (cons (- E T) E))
          #f))))

;; ---------------------------------------------------------
;; 测试脚本
;; ---------------------------------------------------------

(define (run-tests)
  (define a (make-chr #\a))
  (define b (make-chr #\b))
  (define c (make-chr #\c))
  (define d (make-chr #\d))

  ;; 正则: a(b|c)*d
  (define my-re (make-cat a (make-cat (make-star (make-alt (list b c))) d)))

  (printf "=== Testing Matcher ===\n")
  (define matches? (make-matcher my-re))
  (displayln (list 'ad      (matches? "ad")))       ; #t
  (displayln (list 'abd     (matches? "abd")))      ; #t
  (displayln (list 'abccbd  (matches? "abccbd")))   ; #t
  (displayln (list 'add     (matches? "add")))      ; #f
  (displayln (list 'a       (matches? "a")))        ; #f

  (printf "\n=== Testing Searcher ===\n")
  (define search (make-search my-re))
  (displayln (list 'search-ad       (search "ad")))           ; '(0 . 2)
  (displayln (list 'search-xxxad    (search "xxxad")))        ; '(3 . 5)
  (displayln (list 'search-adxxx    (search "adxxx")))        ; '(0 . 2)
  (displayln (list 'search-abccbd   (search "abccbd")))       ; '(0 . 6)
  (displayln (list 'search-none     (search "xyz")))          ; #f

  (printf "\n=== Testing Complex Search (Shortest-Left) ===\n")
  ;; 多个匹配，应该返回最左边的那个
  (displayln (list 'multi-ad   (search "ad...ad")))           ; '(0 . 2)

  ;; 贪婪/非贪婪行为测试
  ;; 在 "abdbd" 中，forward 会在第一个 d (index 3) 停止，然后 backward 找到 a。
  (displayln (list 'abdbd      (search "abdbd")))             ; '(0 . 3) -> "abd"

  ;; 验证 a...d 在长字符串中的表现
  (displayln (list 'long-str   (search "zzzaaaaaaddddzzz")))   ; '(8 . 10) -> "ad"

  (printf "\n=== Testing Reverse Logic ===\n")
  ;; 测试反转后的匹配 (d (b|c)* a)
  (define rev-matches? (make-matcher (re-reverse my-re)))
  (displayln (list 'rev-da     (rev-matches? "da")))          ; #t
  (displayln (list 'rev-dbca   (rev-matches? "dbca")))        ; #t
  (displayln (list 'rev-ad     (rev-matches? "ad")))          ; #f

  (printf "\nAll functional tests finished.\n"))

(run-tests)
