#lang racket

(begin-for-syntax
  (define (find-last-add exp-vec l r)
    (let loop ([i (sub1 r)])
      (if (< i l)
          #f
          (if (eq? (vector-ref exp-vec i) '+)
              i
              (loop (sub1 i)))))))

(begin-for-syntax
  (define (find-last-mul exp-vec l r)
    (let loop ([i (sub1 r)])
      (if (< i l)
          #f
          (if (eq? (vector-ref exp-vec i) '*)
              i
              (loop (sub1 i)))))))

(begin-for-syntax (define (transform exp-vec l r)
    (if (= (add1 l) r)
        (let ([tmp (vector-ref exp-vec l)])
            (if (not (pair? tmp))
                tmp
                (let ([v (list->vector tmp)])
                    (transform v 0 (vector-length v)))))

        (let ([add-pos (find-last-add exp-vec l r)])
            (if add-pos
                `(+ ,(transform exp-vec l add-pos) ,(transform exp-vec (add1 add-pos) r))
                (let ([mul-pos (find-last-mul exp-vec l r)])
                    (if mul-pos
                        `(* ,(transform exp-vec l mul-pos) ,(transform exp-vec (add1 mul-pos) r))
                        (error "Invalid input!"))))))))

(define-syntax bin-exp
    (lambda (stx)
        (syntax-case stx ()
            [(_ . exp)
                (let ([exp-vec (list->vector (syntax->datum #'exp))])
                    (datum->syntax 
                        #'exp
                        (transform 
                            exp-vec 
                            0
                            (vector-length exp-vec))))])))