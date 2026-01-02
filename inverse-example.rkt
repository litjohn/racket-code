#lang racket

(require plot)

;; 设置绘图参数，让输出更美观
(plot-new-window? #t)
(plot-width 800)
(plot-height 800)

;; 1. 定义反演变换函数
;; p: 原始点 (list x y)
;; center: 圆心 (list cx cy)
;; R: 反演半径
(define (invert p center R)
  (let* ([x (first p)]
         [y (second p)]
         [cx (first center)]
         [cy (second center)]
         [dx (- x cx)]
         [dy (- y cy)]
         [dist-sq (+ (sqr dx) (sqr dy))])
    (if (< dist-sq 0.001) ; 避免圆心处的除以零异常
        (list +inf.0 +inf.0)
        (let ([factor (/ (sqr R) dist-sq)])
          (list (+ cx (* dx factor))
                (+ cy (* dy factor)))))))

;; 2. 生成网格线的函数
;; 将一条直线段转换成一系列经过反演的点，用于绘制曲线
(define (make-inverted-line p1 p2 center R #:samples [samples 1000])
  (let ([x1 (first p1)] [y1 (second p1)]
        [x2 (first p2)] [y2 (second p2)])
    (for/list ([t (in-range 0 1 (/ 1.0 samples))])
      (let ([px (+ x1 (* t (- x2 x1)))]
            [py (+ y1 (* t (- y2 y1)))])
        (invert (list px py) center R)))))

;; 3. 配置反演参数
(define R-circle 1.0)
(define center-pt '(0 0))

;; 4. 准备绘图数据
(define grid-range (range -2 2.1 0.4)) ; 网格范围从 -2 到 2

;; 生成水平网格线的反演结果
(define horizontal-curves
  (for/list ([y grid-range])
    (lines (make-inverted-line (list -10 y) (list 10 y) center-pt R-circle)
           #:color "blue" #:width 1 #:alpha 0.5)))

;; 生成垂直网格线的反演结果
(define vertical-curves
  (for/list ([x grid-range])
    (lines (make-inverted-line (list x -10) (list x 10) center-pt R-circle)
           #:color "red" #:width 1 #:alpha 0.5)))

;; 5. 执行绘制
(plot
 (list
  ;; 绘制反演基准圆 (单位圆)
  (polar (λ (θ) R-circle) #:color "black" #:style 'long-dash #:label "Inversion Circle")

  ;; 绘制反演后的网格
  horizontal-curves
  vertical-curves

  ;; 绘制一个参考对象：例如一个在圆外的圆，反演后会变成圆内的圆
  (let ([circle-pts (for/list ([t (in-range 0 (* 2 pi) 0.1)])
                      (let ([px (+ 1.5 (* 0.4 (cos t)))]
                            [py (+ 0.5 (* 0.4 (sin t)))])
                        (invert (list px py) center-pt R-circle)))])
    (lines circle-pts #:color "darkgreen" #:width 3 #:label "Inverted Shape")))
 #:title "2D Circle Inversion Model"
 #:x-min -2.5 #:x-max 2.5
 #:y-min -2.5 #:y-max 2.5)
