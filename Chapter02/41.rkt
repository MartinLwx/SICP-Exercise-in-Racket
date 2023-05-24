#lang racket

;; fold utils
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

;; pair utils
(define (enumerate-interval low high)
   (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

;; in previous unique-pairs, we use a single map to get unique pairs
;; now we want to get a triple pairs, so we need to "map" each n to (n j k) where n > j > k
;; by nesting map we can achieve this goal
(define (triple-pairs n)
  (define (helper t)
    (if (< t 3)       ;; the minimal triple pair is (3 2 1)
        '()
        (let ([cur (flatmap (lambda (j)     ;; note that we need to use flatmap here
                              (map (lambda (k) (list t j k))
                                   (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- t 1)))])
          (append cur (helper (- t 1))))))
  (helper n))

;; filter is quite convenient
(define (sum-to-s n s)
  (filter (lambda (p)
            (= s
               (+ (car p) (cadr p) (caddr p))))
          (triple-pairs n)))

;; tests
(triple-pairs 5)
(sum-to-s 5 8)
