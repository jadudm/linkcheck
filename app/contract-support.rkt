#lang racket

(provide (all-defined-out))

(define (valid-scheme? s)
  (member s '("http" "https")))

;; A contract to make sure a value is one of two (four) symbols.
(define ok/c
  (make-flat-contract #:name 'ok/c
                      #:first-order
                      (lambda (s) (member s '(ok ko OK KO)))))
