#lang racket

;; (car ''abracadabra) = (car (quote (quote abracadabra)))
(car ''abracadabra)    ;; 'quote
