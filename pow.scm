(define (pow-mod a b p)
    (cond ((= b 0) 1)
      (else
        (let* ((res (pow-mod (mod (* a a) p) (div b 2) p)))

          (if (= (mod b 2) 1) 
                (mod (* a res) p) 
                res)
          ))))

(define (pow a b)
    (cond ((= b 0) 1)
      (else
        (let* ((res (pow (* a a) (div b 2))))

          (if (= (mod b 2) 1) 
                (* a res) 
                res)
          ))))

