#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; if it use applicative-order evaluation
;; the remainder operations that are actually performed is: 4
(gcd 206 40)
(if (= 40 0) 206 (gcd 40 6))  ;; 40 != 0, evaluate (gcd 40 6)
(if (= 6 0) 40 (gcd 6 4))     ;; 6 != 0, evaluate (gcd 6 4)
(if (= 4 0) 6 (gcd 4 2))      ;; 4 != 0, evaluate (gcd 4 2)
(if (= 2 0) 4 (gcd 2 0))      ;; 2 != 0, evaluate (gcd 2 0)
(if (= 0 0) 2 (gcd 0 0))      ;; 0 == 0, (gcd 0 0) will not be evaluated

;; if it use normal-order evaluation
;; the remainder operations that are actually performed is: 18
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))   ;; reduction because 40 != 0
(if (= (remainder 206 40) 0)  ;; evaluate * 1, 1 so far
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))   ;; reduction becuase 6 != 0
(if (= (remainder 40 (remainder 206 40)) 0)   ;; evaluate * 2, 3 so far
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))  ;; reduction because 4 != 0
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)  ;; evalute * 4, 7 so far
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))     ;; reduction because 2 != 0
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ;; evaluate * 7, 14 so far
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))   ;; reduction because 0 == 0, evalute * 4, 18 so far
