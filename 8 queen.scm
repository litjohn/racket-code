(optimize-level 3)

(define (solve-n-queens-optimized board-size print-limit)
  (let* ((n board-size)
         ;; --- 1-based indexing for arrays to match C++ style ---
         ;; queens-placement[row] = col (1-indexed)
         (queens-placement (make-fxvector (+ n 1) 0))
         ;; col-used[col] = 1 if used, 0 otherwise
         (col-used (make-fxvector (+ n 1) 0))
         ;; diag1-used[row+col] = 1 if used, 0 otherwise
         ;; (row+col) ranges from 1+1=2 to n+n=2n
         (diag1-used (make-fxvector (+ (* 2 n) 1) 0))
         ;; diag2-used[row-col+n] = 1 if used, 0 otherwise
         ;; (row-col+n) ranges from (1-n+n)=1 to (n-1+n)=2n-1
         (diag2-used (make-fxvector (+ (* 2 n) 1) 0))

         (solutions-found-count 0)  ; mutable counter for total solutions
         (solutions-printed-count 0) ; mutable counter for printed solutions
         )

    ;; --- Procedure to print a solution (if limit not reached) ---
    (define (print-solution)
      (set! solutions-found-count (fx+ solutions-found-count 1))
      (when (fx< solutions-printed-count print-limit)
        (set! solutions-printed-count (fx+ solutions-printed-count 1))
        (display (string-append "Solution #" (number->string solutions-printed-count) ": "))
        ;; Print queen positions for the current solution
        (do ((r 1 (fx+ r 1)))
            ((fx> r n) (newline)) ; After printing all columns, print newline
          (display (fxvector-ref queens-placement r))
          (when (fx< r n) (display " "))) ; Print space between numbers
        (flush-output-port (current-output-port)))) ; Ensure immediate display

    ;; --- Recursive search function (mirrors C++ search) ---
    (define (search-helper current-row) ; current-row is 1-indexed
      (if (fx> current-row n)
          ;; All queens placed, found a solution
          (print-solution)
          ;; Else, try placing a queen in current-row
          (do ((col 1 (fx+ col 1))) ; col is 1-indexed
              ((fx> col n)) ; Loop through all columns
            (let ((diag1-idx (fx+ current-row col))
                  (diag2-idx (fx+ (- current-row col) n)))
              ;; Check if (current-row, col) is safe
              (when (and (fxzero? (fxvector-ref col-used col))
                         (fxzero? (fxvector-ref diag1-used diag1-idx))
                         (fxzero? (fxvector-ref diag2-used diag2-idx)))
                ;; Place queen
                (fxvector-set! queens-placement current-row col)
                (fxvector-set! col-used col 1)
                (fxvector-set! diag1-used diag1-idx 1)
                (fxvector-set! diag2-used diag2-idx 1)

                ;; Recurse to next row
                (search-helper (+ current-row 1))

                ;; Backtrack: un-place queen
                (fxvector-set! col-used col 0)
                (fxvector-set! diag1-used diag1-idx 0)
                (fxvector-set! diag2-used diag2-idx 0)
                ;; queens-placement[current-row] will be overwritten by the next iteration
                ;; or when the loop for 'col' finishes and this function returns.
                )))))
    
    ;; Start search from row 1
    (search-helper 1)
    solutions-found-count) ; Return the total number of solutions found
)

;; --- Main program and output ---
(define (main)
  (let* ((board-size 13)
         (print-first-n 3) ; How many solutions to print
         (_ (begin
              (display (string-append "Calculating " (number->string board-size)
                                     "-queens solutions (printing first "
                                     (number->string print-first-n)
                                     ")...\n"))
              (flush-output-port (current-output-port))))
         (total-solutions (solve-n-queens-optimized board-size print-first-n)))

    (display (string-append "\nTotal solutions found for "
                             (number->string board-size) "x" (number->string board-size)
                             " board: "
                             (number->string total-solutions) "\n"))
    (newline)))

;; Run main program and time it
(time (main))
(exit)