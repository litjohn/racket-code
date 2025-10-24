; main.scm
(optimize-level 3)
(load "pairing-heap.sch")

;; --- Global Definitions ---
(define n (read))
(define m (read))
(define s (read))

; Using (+ n 1) is sufficient for 1-based indexing.
(define graph (make-vector (+ n 1) '()))
(define dists (make-vector (+ n 1) (flonum->fixnum 1e18)))


;; --- Core Algorithm: Dijkstra ---
(define (dijkstra start-node)
  ; Create a min-priority queue. The comparison function sorts pairs
  ; (node . distance) based on their distance (the cdr of the pair).
  (define pq (make-heap (lambda (x y) (< (cdr x) (cdr y)))))

  ; 1. Initialization
  ; The distance to the starting node is 0.
  (vector-set! dists start-node 0)
  ; Add the starting node to the priority queue.
  ((pq 'insert!) (cons start-node 0))

  ; 2. Main Loop
  ; Continue as long as there are nodes to process in the queue.
  (let loop ()
    (when (not (pq 'empty?))
      ; Get the node `u` with the smallest known distance `d`.
      (let* ((top (pq 'get-top))
             (u (car top))
             (d (cdr top)))
        (pq 'pop!)

        ; Optimization: If the distance `d` from the queue is greater than the
        ; already finalized distance for `u`, it's an outdated entry. Skip it.
        (when (<= d (vector-ref dists u))
          ; 3. Relaxation Step
          ; Iterate through all neighbors `v` of the current node `u`.
          (let-values (((adj-list) (vector-ref graph u)))
            (let inner-loop ((adj adj-list))
              (when (not (null? adj))
                (let* ((edge (car adj))
                       (v (car edge))
                       (w (cdr edge))
                       (new-dist (+ d w)))
                  
                  ; If we found a shorter path to `v` through `u`...
                  (when (< new-dist (vector-ref dists v))
                    ; ...update the distance and add the new path to the queue.
                    (vector-set! dists v new-dist)
                    ((pq 'insert!) (cons v new-dist))))
                  
                (inner-loop (cdr adj)))))))
      (loop))))


;; --- Helper Functions ---
; Adds a directed edge from `u` to `v` with weight `w`.
(define (add-edge u v w)
  (vector-set! graph u (cons (cons v w) (vector-ref graph u))))

; Reads `count` edges from standard input.
(define (read-edges count)
  (when (> count 0)
    (let* ((u (read))
          (v (read))
          (w (read)))
      (add-edge u v w))
    (read-edges (- count 1))))

; Displays the calculated shortest distances from the start node.
(define (display-results)
  (let loop ((i 1))
    (if (<= i n)
        (begin
          (let ((final-dist (vector-ref dists i)))
            ; If distance is still 1e18, the node is unreachable (print -1).
            ; Otherwise, print the calculated distance.
            (display (if (< final-dist 1e18) final-dist -1)))
          (if (< i n) (display #\space))
          (loop (+ i 1)))
        (newline))))


;; --- Main Execution ---
(read-edges m)
(dijkstra s)
(display-results)