#lang racket

(provide (all-defined-out))

(define (valid-scheme? s)
  (member s '("http" "https")))