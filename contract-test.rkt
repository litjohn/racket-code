#lang racket

;; (require racket/contract)

;; 使用 define/contract 来定义函数和它的契约
(define/contract (add-one x)
  ;; 这是契约表达式
  (-> number? number?)
  
  ;; 这是函数体，和之前完全一样
  (+ x 1))