#lang racket

(require 2htdp/universe)
(require 2htdp/image)

;; ==========================================
;; 1. 常量定义 (CONSTANTS)
;; ==========================================

(define WIDTH 800)
(define HEIGHT 300)
(define GROUND-Y 250) ;; 地面 Y 坐标

;; 恐龙属性
(define DINO-WIDTH 40)
(define DINO-HEIGHT 50)
(define DINO-COLOR "dimgray")
(define DINO-X 100)

;; 仙人掌属性
(define CACTUS-WIDTH 25)
(define CACTUS-HEIGHT 40)
(define CACTUS-COLOR "forestgreen")

;; 碰撞箱内缩 (Hitbox Padding)
;; 这个值越大，游戏判定越宽容（不容易死）
(define HITBOX-PADDING 8)

;; 物理属性
(define GRAVITY 1.5)
(define JUMP-FORCE -18)
(define SCROLL-SPEED 8)

;; 图像素材 (稍微美化一点点，不像 Minecraft 那么方了)
(define DINO-IMG
  (above (circle (/ DINO-WIDTH 2) "solid" DINO-COLOR)
         (rectangle DINO-WIDTH (- DINO-HEIGHT (/ DINO-WIDTH 2)) "solid" DINO-COLOR)))

(define CACTUS-IMG
  (overlay/align "center" "bottom"
                 (rectangle 10 CACTUS-HEIGHT "solid" CACTUS-COLOR) ;; 主干
                 (circle (/ CACTUS-WIDTH 2) "solid" CACTUS-COLOR))) ;; 稍微圆润一点

(define GROUND-IMG (rectangle WIDTH 2 "solid" "black"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

;; ==========================================
;; 2. 数据结构 (DATA DEFINITIONS)
;; ==========================================

;; dino-y 代表恐龙 **脚底** 的 Y 坐标
(struct world (dino-y dino-vy cacti score game-over?) #:transparent)

(define INITIAL-STATE
  (world GROUND-Y 0 '() 0 #f))

;; ==========================================
;; 3. 游戏逻辑 (GAME LOGIC)
;; ==========================================

(define (update-dino w)
  (define y (world-dino-y w))
  (define vy (world-dino-vy w))

  (if (>= (+ y vy) GROUND-Y)
      (struct-copy world w [dino-y GROUND-Y] [dino-vy 0])
      (struct-copy world w [dino-y (+ y vy)] [dino-vy (+ vy GRAVITY)])))

(define (update-cacti w)
  (define current-cacti (world-cacti w))
  (define moved-cacti (map (lambda (x) (- x SCROLL-SPEED)) current-cacti))
  (define visible-cacti (filter (lambda (x) (> x -50)) moved-cacti))

  (define final-cacti
    (cond
      [(empty? visible-cacti) (list (+ WIDTH (random 100 300)))]
      [(< (last visible-cacti) (- WIDTH (random 200 600)))
       (append visible-cacti (list (+ WIDTH 50)))]
      [else visible-cacti]))

  (struct-copy world w [cacti final-cacti]))

;; [修正重点] 碰撞检测逻辑重写
(define (check-collision w)
  (define dy (world-dino-y w)) ;; 这是恐龙脚底的坐标

  ;; 恐龙的物理判定箱 (Hitbox)
  ;; 我们让判定箱比图片小一圈 (PADDING)，增加容错率
  (define dino-left   (+ (- DINO-X (/ DINO-WIDTH 2)) HITBOX-PADDING))
  (define dino-right  (- (+ DINO-X (/ DINO-WIDTH 2)) HITBOX-PADDING))
  (define dino-bottom (- dy HITBOX-PADDING)) ;; 脚底往上缩一点，避免擦地误判
  ;; 注意：这里不需要计算 dino-top，因为主要是检查有没有踩到仙人掌顶部

  ;; 检查所有仙人掌
  (define hit?
    (for/or ([cx (world-cacti w)])
      ;; 仙人掌的物理判定箱
      (define cactus-left   (+ (- cx (/ CACTUS-WIDTH 2)) HITBOX-PADDING))
      (define cactus-right  (- (+ cx (/ CACTUS-WIDTH 2)) HITBOX-PADDING))
      (define cactus-top    (+ (- GROUND-Y CACTUS-HEIGHT) HITBOX-PADDING))

      ;; 判定重叠:
      ;; 1. 恐龙左边 < 仙人掌右边
      ;; 2. 恐龙右边 > 仙人掌左边
      ;; 3. 恐龙脚底 > 仙人掌顶端 (说明高度不够，撞上了)
      (and (< dino-left cactus-right)
           (> dino-right cactus-left)
           (> dino-bottom cactus-top))))

  (if hit?
      (struct-copy world w [game-over? #t])
      w))

(define (update-score w)
  (struct-copy world w [score (+ 1 (world-score w))]))

(define (tock w)
  (cond
    [(world-game-over? w) w]
    [else
     (let* ([w1 (update-dino w)]
            [w2 (update-cacti w1)]
            [w3 (check-collision w2)])
       (if (world-game-over? w3)
           w3
           (update-score w3)))]))

;; ==========================================
;; 4. 渲染 (RENDERING)
;; ==========================================

(define (draw-cactus x scene)
  ;; 这里的坐标计算保持原样，确保视觉和物理对其
  (place-image CACTUS-IMG x (- GROUND-Y (/ CACTUS-HEIGHT 2)) scene))

(define (render w)
  (define scene-with-ground
    (place-image GROUND-IMG (/ WIDTH 2) (+ GROUND-Y 2) BACKGROUND))

  (define scene-with-cacti
    (foldl draw-cactus scene-with-ground (world-cacti w)))

  ;; 修正渲染：dino-y 是脚底，place-image 的锚点是中心
  ;; 所以中心点应该是 dino-y - (DINO-HEIGHT / 2)
  (define scene-with-dino
    (place-image DINO-IMG DINO-X (- (world-dino-y w) (/ DINO-HEIGHT 2)) scene-with-cacti))

  ;; 调试模式：取消注释下面这行可以看到红色的实际碰撞点，用于验证手感
  ;; (define scene-with-dino (place-image (circle 3 "solid" "red") DINO-X (- (world-dino-y w) HITBOX-PADDING) scene-with-dino))

  (define score-text
    (text (string-append "Score: " (number->string (world-score w))) 20 "black"))

  (define final-scene
    (place-image score-text (- WIDTH 80) 30 scene-with-dino))

  (if (world-game-over? w)
      (place-image (text "GAME OVER (Press Space)" 40 "red")
                   (/ WIDTH 2) (/ HEIGHT 2)
                   final-scene)
      final-scene))

;; ==========================================
;; 5. 输入处理 (INPUT HANDLING)
;; ==========================================

(define (handle-key w key)
  (cond
    [(and (world-game-over? w) (string=? key " "))
     INITIAL-STATE]
    [(or (string=? key " ") (string=? key "up"))
     (if (>= (world-dino-y w) GROUND-Y)
         (struct-copy world w [dino-vy JUMP-FORCE])
         w)]
    [else w]))

;; ==========================================
;; 6. 启动游戏
;; ==========================================

(big-bang INITIAL-STATE
  (on-tick tock)
  (to-draw render)
  (on-key handle-key)
  (name "Racket Dino Run"))
