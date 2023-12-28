#lang racket

;; Define some utils.
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend exp)
  (cadr exp))

;; Return the second item if exp is (+ operand1 operand2),
;; else return (+ operand2 ...)
(define (augend exp)
  (cond ((= (length exp) 3) (caddr exp))
        (else (make-sum (caddr exp)
                        (cadddr exp)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (cond ((= (length p) 3) (caddr p))
        (else (make-product (caddr p)
                            (cadddr p)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; a ^ b
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^))) 

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

;; Calculate a ^ b
(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        ((and (number? a) (number? b))
         (expt a b))
        (else (list '^ a b))))
                
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))    ;; d(x) / d(x) = 1
        ((sum? exp)                            ;; d(u + v) / d(x) = d(u) / d(x) + d(v) / d(x)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)                        ;; d(uv) / d(x) = u * (d(v) / d(x)) + v * (d(u) / d(x))
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


;; tests
(deriv '(+ x y (+ x 3)) 'x)      ;; 2
(deriv '(* x y (+ x 3)) 'x)      ;; '(+ (* x y) (* y (+ x 3)))

