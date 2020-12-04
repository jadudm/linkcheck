#lang racket

(provide
 (contract-out
  [debug? (case->
           (-> boolean?)
           (-> boolean? void?))]
  [quiet? (case->
           (-> boolean?)
           (-> boolean? void?))]
  [local:scheme (case->
                 (-> string?)
                 (-> string? void?))]
  [local:host (case->
                 (-> string?)
                 (-> string? void?))]
  [local:port (case->
                 (-> number?)
                 (-> number? void?))]
  ))
              

(define debug? (make-parameter false))
(define quiet? (make-parameter false))
(define local:scheme (make-parameter "http"))
(define local:host   (make-parameter "localhost"))
(define local:port   (make-parameter 4000))

;; We can't test the contract boundaries with the
;; module+ test form. However, the contracts guarantee
;; that the parameters only allow the right *kind* of data in-and-out.
(module+ test
  (require chk)
  (chk
   (#:= (debug?) false)
   (#:= (quiet?) false)
   (#:= (local:scheme) "http")
   (#:= (local:port) 4000)
   )

  (debug? true)
  (local:scheme "https")
  (local:port 8080)
  (chk
   (#:= (debug?) true)
   (#:= (local:scheme) "https")
   (#:= (local:port) 8080)
   )
)