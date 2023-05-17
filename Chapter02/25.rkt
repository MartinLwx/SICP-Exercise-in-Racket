#lang racket

;; note:
;; car returns the first element of the list
;; cdr ALWAYS return a list
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))

(car
 (cdr
  (car
   (cdr
    (car
     (cdr
      (car
       (cdr
        (car
         (cdr
          (car
           (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
