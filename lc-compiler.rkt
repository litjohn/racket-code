#lang racket

;;; ======================================================
;;;  新版 Lexer (基于列表处理的函数式风格)
;;; ======================================================

;; 辅助函数: 判断一个字符是否为分隔符
(define (delimiter? c)
  (member c '(#\: #\( #\) #\= #\{ #\})))

;; 辅助函数: 判断一个字符是否为空白符
(define (whitespace? c)
  (char-whitespace? c))

;; 核心辅助函数: 从字符列表开头读取一个完整的单词(标识符或关键字)
;; 返回两个值: 1. 单词转换成的符号  2. 剩余的字符列表
(define (read-word char-list)
  (let loop ([cs char-list] [acc '()])
    (if (or (null? cs)
            (let ([c (car cs)])
              (or (whitespace? c) (delimiter? c))))
        ;; 遇到空白或分隔符，或者列表为空，结束读取
        (values (string->symbol (list->string (reverse acc)))
                cs)
        ;; 否则，将当前字符加入累加器，继续处理剩余列表
        (loop (cdr cs) (cons (car cs) acc)))))

;; 主词法分析循环
(define (tokenize-loop char-list)
  (if (null? char-list)
      '() ; 输入列表为空，返回空列表
      (let ([c (car char-list)])
        (cond
          ;; 1. 如果是空白符，则忽略它，直接处理剩余部分
          [(whitespace? c)
           (tokenize-loop (cdr char-list))]

          ;; 2. 如果是单个字符的分隔符，将其作为 token，然后处理剩余部分
          [(delimiter? c)
           (cons (string->symbol (string c))
                 (tokenize-loop (cdr char-list)))]

          ;; 3. 否则，它是一个单词的开始，调用 read-word 来读取它
          [else
           (let-values ([(word remaining-chars) (read-word char-list)])
             (cons word (tokenize-loop remaining-chars)))]))))

;; Lexer 的公共接口
;; 它将字符串转换为字符列表，然后启动主循环
(define (lexer str)
  (tokenize-loop (string->list str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. AST Node Definitions (保持不变)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct identifier (name) #:transparent)
(struct abstract-exp (param body) #:transparent)
(struct apply-exp (rator rand) #:transparent)
(struct bind-exp (name val) #:transparent)

(define (is-keyword? s)
  (member s '(func : ( ) { } return =)))

(define (is-identifier-token? t)
  (and (symbol? t) (not (is-keyword? t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Parser (修正版)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse tokens)
  (let-values ([(ast next-i) (parse-lc-exp (list->vector tokens) 0)])
    (if (= next-i (vector-length (list->vector tokens)))
        ast
        (error "Parsing failed: unexpected tokens remain at the end." (vector-ref (list->vector tokens) next-i)))))

(define (parse-lc-exp tokens i)
  (let ([tok1 (vector-ref tokens i)])
    (if (and (is-identifier-token? tok1)
             (< (+ i 1) (vector-length tokens))
             (eq? (vector-ref tokens (+ i 1)) '=))
        (parse-bind-exp tokens i)
        (parse-val-exp tokens i))))

(define (parse-bind-exp tokens i)
  (let* ([id-name (vector-ref tokens i)]
         [val-start-i (+ i 2)])
    (let-values ([(val-ast next-i) (parse-val-exp tokens val-start-i)])
      (values (bind-exp (identifier id-name) val-ast) next-i))))

; 修正: 使用 |(|, |)|, |{|, |}|
(define (parse-abstract-exp tokens i)
  (let* ([param-name (vector-ref tokens (+ i 3))]
         [body-start-i (+ i 7)])
    (unless (and (eq? (vector-ref tokens i) 'func)
                 (eq? (vector-ref tokens (+ i 1)) ':)
                 (eq? (vector-ref tokens (+ i 2)) '|(|)
                 (is-identifier-token? param-name)
                 (eq? (vector-ref tokens (+ i 4)) '|)|)
                 (eq? (vector-ref tokens (+ i 5)) '|{|)
                 (eq? (vector-ref tokens (+ i 6)) 'return))
      (error "Invalid function definition syntax"))
    (let-values ([(body-ast body-end-i) (parse-val-exp tokens body-start-i)])
      (unless (and (< body-end-i (vector-length tokens))
                   (eq? (vector-ref tokens body-end-i) '|}|))
        (error "Expected '}' at the end of function body"))
      (values (abstract-exp (identifier param-name) body-ast)
              (+ body-end-i 1)))))

; 修正: 修复 loop 的逻辑, 确保有 else 分支, 并使用正确的符号
(define (parse-val-exp tokens i)
  (let-values ([(left-ast next-i)
                (let ([tok (vector-ref tokens i)])
                  (cond
                    [(eq? tok 'func) (parse-abstract-exp tokens i)]
                    [(is-identifier-token? tok) (values (identifier tok) (+ i 1))]
                    [else (error "Invalid value expression: expected identifier or function at index" i)]))])
    (let loop ([current-ast left-ast] [current-i next-i])
      ; 检查是否是函数应用
      (if (and (< current-i (vector-length tokens))
               (eq? (vector-ref tokens current-i) '|(|))
        ; 是: 解析参数, 构建 apply-exp, 然后继续循环
        (let-values ([(arg-ast arg-end-i) (parse-val-exp tokens (+ current-i 1))])
          (unless (and (< arg-end-i (vector-length tokens))
                       (eq? (vector-ref tokens arg-end-i) '|)|))
            (error "Expected ')' to close function application"))
          (loop (apply-exp current-ast arg-ast) (+ arg-end-i 1)))
        ; 否: 循环结束, 返回当前积累的 AST 和索引
        (values current-ast current-i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert node)
    (match node
        [(bind-exp name val) `(define ,(convert name) ,(convert val))]
        [(apply-exp rator rand) `(,(convert rator) ,(convert rand))]
        [(abstract-exp arg body) `(lambda (,(convert arg)) ,(convert body))]
        [(identifier id) id]))

(define test-str-1 "id = func : (x) { return x }")
(define test-str-2 "apply = func : (f) { return func : (x) { return f(x) } }")
(define test-str-3 "id(y)")
(define test-str-4 "get-y(f)(x)")

(displayln "--- Test 1: Binding ---")
(displayln (convert (parse (lexer test-str-1))))

(displayln "--- Test 2: Currying ---")
(displayln (convert (parse (lexer test-str-2))))

(displayln "--- Test 3: Simple Application ---")
(displayln (convert (parse (lexer test-str-3))))

(displayln "--- Test 4: Chained Application ---")
(displayln (convert (parse (lexer test-str-4))))
