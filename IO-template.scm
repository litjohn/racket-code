(import (rnrs)
        (rnrs io ports))

;; 从指定端口（默认为当前输入）读取一个由空白符分割的单词
(define (read-word . maybe-port)
  ;; --- 确定要操作的端口 ---
  (let ([in (if (null? maybe-port)
                (current-input-port)
                (car maybe-port))])

    ;; --- 局部辅助函数定义 ---
    ;; define 必须在 let 体的开头
    (define (skip-whitespace)
      (let ([p (peek-char in)])
        (when (and (not (eof-object? p)) (char-whitespace? p))
          (get-char in)
          (skip-whitespace))))

    (define (read-chars)
      (let ([out (open-output-string)])
        (let loop ()
          (let ([p (peek-char in)])
            (if (or (eof-object? p) (char-whitespace? p))
                (get-output-string out)
                (begin
                  (put-char out (get-char in))
                  (loop)))))))

    ;; --- 主逻辑 ---
    (skip-whitespace)
    (if (eof-object? (peek-char in))
        (eof-object)
        (read-chars))))

(define (read-int)
  (string->number (read-word)))

;; 从指定端口（默认为当前输入）读取一整行
(define (readln . maybe-port)
  (let ([in (if (null? maybe-port)
                (current-input-port)
                (car maybe-port))])

    (get-line in)))
