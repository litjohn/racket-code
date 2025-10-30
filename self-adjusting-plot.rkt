#lang racket

(require racket/math)
(require plot)

;; --- clamp 函数 ---
(define (clamp v lo hi)
  (max lo (min v hi)))

;; --- 向量和几何计算的辅助函数 (无变化) ---
(define (make-vector p1 p2) (cons (- (car p2) (car p1)) (- (cdr p2) (cdr p1))))
(define (vector-dot-product v1 v2) (+ (* (car v1) (car v2)) (* (cdr v1) (cdr v2))))
(define (vector-magnitude v) (sqrt (+ (sqr (car v)) (sqr (cdr v)))))
(define (angle-between-vectors v1 v2)
  (let ([mag1 (vector-magnitude v1)] [mag2 (vector-magnitude v2)])
    (if (or (zero? mag1) (zero? mag2))
        0 ; 修正：如果一个向量是0，认为夹角是0，即没有拐弯
        (acos (clamp (/ (vector-dot-product v1 v2) (* mag1 mag2)) -1.0 1.0)))))

;; --- 核心自适应算法 ---
(define phi (/ (+ 1 (sqrt 5)) 2))
(define inv-phi (/ 1 phi))
(define one-minus-inv-phi (- 1 inv-phi))

(define (subdivide p1 p2 f max-depth min-interval-size angle-tolerance depth)
  (let ([x1 (car p1)] [y1 (cdr p1)] [x2 (car p2)] [y2 (cdr p2)])
    (if (or (>= depth max-depth) (< (- x2 x1) min-interval-size))
        (list p2)
        (let* ([interval-width (- x2 x1)]
               [xs1 (+ x1 (* interval-width one-minus-inv-phi))]
               [xs2 (+ x1 (* interval-width inv-phi))]
               [ps1 (cons xs1 (f xs1))]
               [ps2 (cons xs2 (f xs2))])
          (let* ([v1 (make-vector p1 ps1)] [v2 (make-vector ps1 ps2)] [v3 (make-vector ps2 p2)]
                                           [angle1 (angle-between-vectors v1 v2)] [angle2 (angle-between-vectors v2 v3)])

            ;; ==========================================================
            ;; *** BUG 修正点 ***
            ;; 检查拐角是否足够小 (接近0度)，而不是接近180度
            (if (and (< angle1 angle-tolerance)
                     (< angle2 angle-tolerance))
                ;; ==========================================================

                (list p2) ; 足够平滑，返回终点

                (append (subdivide p1 ps1 f max-depth min-interval-size angle-tolerance (+ 1 depth))
                        (subdivide ps1 ps2 f max-depth min-interval-size angle-tolerance (+ 1 depth))
                        (subdivide ps2 p2 f max-depth min-interval-size angle-tolerance (+ 1 depth)))))))))

;; --- 主调用函数 ---
(define (adaptive-plot f a b #:max-depth [max-depth 10]
                       #:min-interval-size [min-interval-size 1e-6]
                       #:angle-degrees-tolerance [angle-degrees-tolerance 2])
  (let* ([p_start (cons a (f a))]
         [p_end (cons b (f b))]
         [angle-rad-tolerance (degrees->radians angle-degrees-tolerance)])
    (cons p_start
          (subdivide p_start p_end f max-depth min-interval-size angle-rad-tolerance 0))))

;; --- 可视化函数 ---
(define (plot-function
         f a b title
         #:max-depth [max-depth 10]
         #:angle-degrees-tolerance [angle-tolerance 2])

  (define points
    (adaptive-plot
     f a b
     #:max-depth max-depth
     #:angle-degrees-tolerance angle-tolerance))

  (printf "对于 '~a'，在 [~a, ~a] 区间上，生成了 ~a 个点\n" title a b (length points))
  (define plot-data (map (lambda (p) (vector (car p) (cdr p))) points))
  (plot (list (lines plot-data)) #:title title #:x-label "x" #:y-label "y"))
