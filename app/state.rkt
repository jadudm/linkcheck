#lang racket
(require "contract-support.rkt")

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
  [alt-tag-length (case->
                   (-> number?)
                   (-> number? void?))]
  [urls:ok (case->
            (-> hash?)
            (-> hash? void?))]
  [urls:ko (case->
            (-> hash?)
            (-> hash? void?))]
  [urls:status (case->
                (-> hash?)
                (-> hash? void?))]
  [alt:ok (case->
             (-> hash?)
             (-> hash? void?))]
  [alt:ko (case->
             (-> hash?)
             (-> hash? void?))]
  ))
(provide get-count get-alt-count)

(define debug? (make-parameter false))
(define quiet? (make-parameter false))
(define local:scheme (make-parameter "http"))
(define local:host   (make-parameter "localhost"))
(define local:port   (make-parameter 4000))
(define alt-tag-length (make-parameter 3))

;; PURPOSE
;; These hashes track all URLs traversed.
;; They end up being OK, or KO.
(define urls:ok (make-parameter (make-immutable-hash)))
(define urls:ko (make-parameter (make-immutable-hash)))
(define urls:status (make-parameter (make-immutable-hash)))
(define alt:ok (make-parameter (make-immutable-hash)))
(define alt:ko (make-parameter (make-immutable-hash)))

;; PURPOSE
;; Gets the count of good and bad URLs that were found from a run.
(define/contract (get-count sym)
  (-> ok/c number?)
  (case sym
    [(ok OK) (hash-count (urls:ok))]
    [(ko KO) (hash-count (urls:ko))]))

;; PURPOSE
;; Gets the count of good and bad URLs that were found from a run.
(define/contract (get-alt-count sym)
  (-> ok/c number?)
  (case sym
    [(ok OK) (hash-count (alt:ok))]
    [(ko KO) (hash-count (alt:ko))]))


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