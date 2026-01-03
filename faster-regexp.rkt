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

;; 6. 性能测试 (那个曾经卡死的用例)
(define (test-performance n)
  (printf "Testing n=~a... " n)
  (define a (make-chr #\a))
  ;; a|Eps a|Eps ... a a a
  (define part1 (for/fold ([acc eps]) ([i n]) (make-cat (make-alt (list eps a)) acc)))
  (define part2 (for/fold ([acc eps]) ([i n]) (make-cat a acc)))
  (define re (make-cat part1 part2))

  (define cache (make-hash))
  (define (matches? cur s)
    (if (null? s)
        (nullable? cur)
        (let* ([c (car s)]
               [next (hash-ref! (hash-ref! cache cur (λ () (make-hash)))
                                c
                                (λ () (derive cur c)))])
          (if (Void? next) #f (matches? next (cdr s))))))

  (define input (make-string (* 2 n) #\a))
  (collect-garbage)
  (time (matches? re (string->list input))))

;; (test-performance 100)
;; (test-performance 200)
;; (test-performance 500)
;; (test-performance 1000)
;; (test-performance 2000)
;; (test-performance 5000)
;; (test-performance 10000)
;; (test-performance 50000)

;; ---------------------------------------------------------
;; 测试套件
;; ---------------------------------------------------------

(define (matches? re str)
  (let loop ([cur re] [s (string->list str)])
    (cond
      [(null? s) (nullable? cur)]
      [else
       (let ([next (derive cur (car s))])
         (if (Void? next)
             #f
             (loop next (cdr s))))])))

(define (test-all)
  (define a (make-chr #\a))
  (define b (make-chr #\b))
  (define c (make-chr #\c))

  (printf "Starting Correctness Tests...\n")

  ;; 1. 基础匹配
  (displayln (assert "Basic match" (matches? a "a") #t))
  (displayln (assert "Basic mismatch" (matches? a "b") #f))

  ;; 2. Alt 逻辑 (测试 nullable? 修正)
  (define a-or-b (make-alt (list a b)))
  (displayln (assert "Alt match 1" (matches? a-or-b "a") #t))
  (displayln (assert "Alt match 2" (matches? a-or-b "b") #t))
  (displayln (assert "Alt nullable" (nullable? (make-alt (list phi eps))) #t))

  ;; 3. ACI 规范化 (由于 Hash-Consing，eq? 必须成立)
  (define re1 (make-alt (list a b)))
  (define re2 (make-alt (list b a)))
  (displayln (assert "ACI Commutativity (re1 eq? re2)" (eq? re1 re2) #t))

  (define re3 (make-alt (list a a a)))
  (displayln (assert "ACI Idempotence (re3 eq? a)" (eq? re3 a) #t))

  (define re4 (make-alt (list a (make-alt (list b c)))))
  (define re5 (make-alt (list (make-alt (list a b)) c)))
  (displayln (assert "ACI Associativity (re4 eq? re5)" (eq? re4 re5) #t))

  ;; 4. Cat 与 Star
  (define complex (make-cat (make-star (make-alt (list a b))) c))
  (displayln (assert "Star/Cat match 1" (matches? complex "c") #t))
  (displayln (assert "Star/Cat match 2" (matches? complex "ababc") #t))
  (displayln (assert "Star/Cat match 3" (matches? complex "aba") #f))

  ;; 5. Nullable 深度测试
  ;; (a|ε)(b|ε) 应该是 nullable
  (define n1 (make-cat (make-alt (list a eps)) (make-alt (list b eps))))
  (displayln (assert "Nested nullable" (nullable? n1) #t))

  (displayln "All tests completed."))

(define (assert msg actual expected)
  (if (equal? actual expected)
      (format " [PASS] ~a" msg)
      (error (format " [FAIL] ~a: expected ~a, got ~a" msg expected actual))))

(test-all)
