#lang racket

(define a (make-vector 4))

(let read-a ([i 0])
    (when (< i 4)
        (vector-set! a i (read))

        (when (not (number? (vector-ref a i)))
            (error "Invalid input!"))

        (read-a (+ i 1)))
        
    (vector-sort! a <))

(struct node (val op1 op2 e1 e2) #:transparent #:mutable)

(define (print-ans p)
    (if (eq? (node-e1 p) (void))
        (display (node-val p))
        (begin
            (display #\()
            ; (display #\()
            (when (eq? (node-op1 p) '-)
                (display (node-op1 p)))

            (print-ans (node-e1 p))

            (display (node-op2 p))
            ; (display #\()
            (print-ans (node-e2 p))
            ; (display #\))
            (display #\)))))

(define operators (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)))

(define (solve l r)
    (if (= l r)
        (list (node (vector-ref a l) (void) (void) (void) (void)))
        (let ([res '()])
            (let loop ([i l])
                (when (< i r)
                    (let ([p1 (solve l i)]
                          [p2 (solve (+ i 1) r)])
                          
                        (for* ([op1 operators]
                               [op2 operators]
                               [e1 p1]
                               [e2 p2])
                            
                            (unless (or 
                                        (and (= (node-val e2) 0)
                                             (eq? (car op2) '/))

                                        (eq? (car op1) '*)
                                        (eq? (car op1) '/))

                                (let ([val ((cdr op2) ((cdr op1) (node-val e1)) (node-val e2))])
                                    (set! res (cons (node val (car op1) (car op2) e1 e2) res))))))
                    
                    (loop (+ i 1))))
                    
            res)))

(define ans (void))

; Helper function to reverse a subarray of vector `v` from `start` to `end` inclusive.
(define (reverse-subarray v start end)
  (let loop ([i start] [j end])
    (when (< i j)
      ; Swap elements at index i and j
      (let ([temp (vector-ref v i)])
        (vector-set! v i (vector-ref v j))
        (vector-set! v j temp))
      (loop (+ i 1) (- j 1)))))

; Implements the C++ std::next_permutation algorithm.
; Mutates global vector 'a' in place to the next lexicographical permutation.
; Returns #t if a next permutation was found, #f if it wrapped around to the first permutation.
(define (next-permutation a)
  (let ([n (vector-length a)])
    ; Step 1: Find pivot point 'i' (find first element a[i] < a[i+1] from right)
    (let loop-find-pivot ([i (- n 2)])
      (cond
        ; Case 1: No pivot found (vector is in reverse order)
        [(< i 0)
         ; Reverse the whole vector to get the first permutation.
         (reverse-subarray a 0 (- n 1))
         ; (displayln "!!!!!!!\n")
         #f] ; Return #f to indicate we wrapped around.
        
        ; Case 2: Found pivot point 'i'
        [(< (vector-ref a i) (vector-ref a (+ i 1)))
            ; (displayln i)
         ; Step 2: Find swap element 'j' (find first element a[j] > a[i] from right)
         (let loop-find-swap ([j (- n 1)])
           (if (> (vector-ref a j) (vector-ref a i))
             ; Step 3: Swap elements at i and j
             (let ([temp (vector-ref a i)])
               (vector-set! a i (vector-ref a j))
               (vector-set! a j temp))
               
            (loop-find-swap (- j 1))))
         
         ; Step 4: Reverse suffix starting from i + 1
         (reverse-subarray a (+ i 1) (- n 1))
         #t] ; Return #t to indicate a next permutation was found.

        ; Case 3: Continue searching for pivot point 'i' to the left
        [else (loop-find-pivot (- i 1))]))))

(define (print-a)
    (let loop ([i 0])
        (when (< i 4)
            (display (vector-ref a i))
            (display #\space)
            (loop (+ i 1))))
            
    (newline))

(let loop ([i 0])
    (when (< i 24)
        ; (print-a)

        (set! ans (solve 0 3))
        (for ([p ans])
            (when (= (node-val p) 24)
                (print-ans p)
                (newline)
                #; (exit)))
            
        (when (next-permutation a)
            (loop (+ i 1)))))