#lang racket
(require html-parsing
         json
         net/http-easy
         net/url
         racket/match
         racket/hash
         "contract-support.rkt"
         xml
         )

(provide (all-defined-out))

(define debug? (make-parameter false))
(define local:scheme (make-parameter "http"))
(define local:host   (make-parameter "localhost"))
(define local:port   (make-parameter 4000))

;; Logging
(define-logger check)
(define check-info-receiver (make-log-receiver check-logger 'info))

(define PROBABLY-NOT-HTML
  '(js png jpg jpeg css svg xml aspx json zip pdf gz ico
                       webmanifest rss tif tiff mpg mpeg mov aiff wav avi
                       scrbl ics pdf atom))

;; PURPOSE
;; These hashes track all URLs traversed.
;; They end up being OK, or KO.
(define urls:ok (make-hash))
(define urls:ko (make-hash))
(define urls:status (make-hash))

;; CONTRACT
;; make-path :: (or list? string?) -> path/param?
;; PURPOSE
;; Takes a string (like "/info/") and returns a list of path/param elements
;; that match that path. If given a list, return it as-is.
(define/contract (make-path path #:query [query empty])
  (->* (string?) (#:query (listof string?)) (listof path/param?))
  (cond
    [(list? path) path]
    [else
     (define trimmed (string-trim path "/"))
     ;; If we end with a slash, force-insert it back into the param.
     (define params
       (map (λ (s)
              (path/param s empty)) (regexp-split "/" trimmed)))
     params]))
       

;; CONTRACT
;; make-url :: see below
;; PURPOSE
;; This provides a way to create a base, local URL with overrides of specific
;; elements of the URL. This is primarily used for building new URLs with new
;; paths (from relative anchors in a page) but using the rest of the local URL.
(define/contract (make-url #:scheme [scheme (local:scheme)]
                  #:user [user false]
                  #:host [host (local:host)]
                  #:port [port (local:port)]
                  #:path-absolute? [path-absolute? true]
                  #:path [path ""]
                  #:query [query empty]
                  #:fragment [fragment false])
  (->* ()
       (#:scheme valid-scheme?
        #:user boolean?
        #:host string?
        #:path-absolute? boolean?
        #:path string?
        ;; FIXME: Query handling is untested.
        #:query (listof path/param?)
        #:fragment boolean?)
       url?
       )
  (url scheme user host port path-absolute? (make-path path) query fragment))

;; CONTRACT
;; fetch :: url -> xexpr
;; PURPOSE
;; Returns the content of a URL as an xexpr.
;; FIXME: Use http-easy instead of net/url here.
(define/contract (fetch url)
  (-> url? xexpr?)
  (log-check-info "fetching: ~a~n" url)
  (cond
    [(probably-html? url)
     (with-handlers ([exn? (lambda (e)
                             (printf "EXCEPTION ~a~n" url)
                             (printf "~a~n" e)
                             '())])
       (define res (get url #:stream? true))
       (define result (html->xexp (bytes->string/utf-8 (response-body res))))
       (response-close! res)
       result)]
    [else '()]))

;; CONTRACT
;; anchor->path :: xexpr -> string
;; PURPOSE
;; Extracts the HREF from an anchor.
(define/contract (anchor->path xexpr)
  (-> xexpr? string?)
  (log-check-info "anchor-to-path: ~s~n" xexpr)
  (define attributes
    (filter (λ (o)
              (and (list? o)
                   (equal? (first o) '@)))
            xexpr))
  ;; Either we have to filter out JS and CSS pages,
  ;; or we can get this far, and return a bogus default
  ;; for when things fail. Given the mess that is the web,
  ;; handling exceptions with a graceful default (if opaque)
  ;; seems reasonable.
  (with-handlers ([exn? (λ (e) "/")])
    (define href
      (second (first
               (filter (λ (o)
                         (and (list? o)
                              (member (first o) '(href src))))
                       (if (not (empty? attributes))
                           (first attributes)
                           '())))))
    (log-check-info "extracted: ~s~n" href)
    href))

;; CONTRACT
;; convert-url :: string? -> url?
;; PURPOSE
;; In moving to using URL structs (instead of strings)
;; this became a union type input... but, it unifies, and
;; makes  sure local paths are full URLs.
(define/contract (convert-url str)
  (-> string? url?)
  (match str
    [(pregexp "^/") (make-url #:path str)]
    [(pregexp "^http[s]{0,1}") (string->url str)]
    ;; Ignore any internal HREFs.
    [(pregexp "^#") (make-url)]
    ;; What if we get an empty string?
    ["" (make-url)]
    [else
     ;; If all else fails, flag it, and return the base URL.
     (log-check-fail (format "Cannot convert: ~s" str))
     (make-url)]))
             
;; CONTRACT
;; extract-urls :: x-expr -> (list-of url?)
;; PURPOSE
;; Takes an x-expr, and walks the document finding all
;; of the URLs. Pulls them as strings, and returns them as
;; URL structs.
(define/contract (extract-urls xexpr)
  (-> xexpr? (listof url?))
  (cond
    [(empty? xexpr) '()]
    ;; Check anchors, CSS/JS, images...
    [(and (list? xexpr)
          (member (first xexpr) '(a link img script)))
     (define path (anchor->path xexpr))
     (log-check-info "extracted href: ~s~n" path)
     (convert-url path)]
    [(list? xexpr)
     (define urls (map extract-urls xexpr))
     (define deduped (remove-duplicates (flatten urls)))
     (define cleaned (filter url? deduped))
     (when (not (empty? cleaned))
       (log-check-info "urls from xexpr: ~a~n" cleaned))
     cleaned]
    [else empty]))

;; CONTRACT
;; get-status-code :: url? -> string?
;; PURPOSE
;; Grabs the head from the server for a given URL
;; and returns a status code. Saves downloading the content.
(define/contract (get-status-code url)
  (-> url? number?)
  (define url-string (url->string url))
  (with-handlers ([exn? (λ (e)
                          (log-check-error (format "~s~n" e))
                          999
                          )])
    ;; Expand local paths to full URLs.
    (define wrapped-url (match url-string
                          [(regexp "^/")
                           (make-url #:path url-string)]
                          [(pregexp "^http[s]{0,1}") url-string]))
    (define res (head wrapped-url #:stream? true))
    (response-close! res)
    (response-status-code res)))

;; CONTRACT
;; check-200? :: url -> boolean
;; PURPOSE
;; Checks to see if a URL returns a good HTTP response.
(define (check-200? url)
  (define status-code (get-status-code url))
  (hash-set! urls:status (string->symbol (url->string url)) status-code)
  ;; Either a 2xx response, or a status of #"OK"
  ;; Some servers response #"OK ", which is annoying.
  ;; Stick with the numerics.
  (and (>= status-code 200)
       (<  status-code 300))
  )

;; CONTRACT
;; visited? :: url? -> boolean
;; PURPOSE
;; Takes a URL and checks to see if we've visited it yet.
(define/contract (visited? url)
  (-> url? boolean?)
  (define key (string->symbol (url->string url)))
  (or (hash-has-key? urls:ok key)
      (hash-has-key? urls:ko key)))

;; CONTRACT
;; local-url? :: url? -> boolean
;; PURPOSE
;; If the URL contains the scheme, host, and port
;; that we're traversing, then the claim is that it is a local
;; URL, and we can fetch the source for deeper traversal.
(define/contract (local-url? url)
  (-> url? boolean?)
  (and (equal? (local:scheme) (url-scheme url))
       (equal? (local:host) (url-host url))
       (equal? (local:port) (url-port url))))

;; CONTRACT
;; probably-html :: url? -> boolean
;; PURPOSE
;; Tries to weed out a few of the things most likely to *not*
;; be HTML. Things like JS, SVG, etc. Not comprehensive.
(define/contract (probably-html? url)
  (-> url? boolean?)
  (not (ormap (λ (end)
                (regexp-match (format ".*\\.~a" end)
                              (url->string url)))
              PROBABLY-NOT-HTML)))

;; PURPOSE
;; Runs the show.
(define/contract (linkcheck url)
  (-> url? void?)
  (log-check-info "--- RUNNING CHECK: ~a~n" (url->string url))
  (define contains-urls
    (cond
      [(and (local-url? url)
            (probably-html? url))
       (extract-urls (fetch url))]
      [else empty]))

  (log-check-info "About to check the following URLs:")
  (for ([u contains-urls])
    (log-check-info (format "[ v: ~a ] ~a~n"
                            (visited? u)
                            (url->string u))))
  
  (for ([u contains-urls])
    (when (not (visited? u))
      (fprintf (current-error-port) "[ ~a ] checking: ~a~n"
               (if (probably-html? u) "HTM" "NOT")
               (url->string u))
      (cond
        [(check-200? u)
         (hash-set! urls:ok
                    (string->symbol (string-trim (url->string u)))
                    u)
         (log-check-info "--- ~a ~a ~a~n"
                 (local-url? u)
                 (probably-html? u)
                 (url->string u))
         (linkcheck u)]
        [else (hash-set! urls:ko
                         (string->symbol (string-trim (url->string u)))
                         u)]
        ))))
  

;; Sets parameters and kicks everything off.
(define (start #:scheme [scheme "http"]
               #:host [host "localhost"]
               #:port [port 4000])
  ;; Launch debugging thread.
  (when (debug?)
    (void
     (thread (λ ()
               (let loop ()
                 (define v (sync check-info-receiver))
                 (printf "[ ~a ] ~a~n" (vector-ref v 0) (vector-ref v 1))
               (loop))))))
  
  ;; Setup and run the checks.
  (local:scheme scheme)
  (local:host host)
  (local:port port)
  (linkcheck (make-url)))

(define (report-csv)
  (printf "url,status~n")
  (for ([h (list urls:ko urls:ok)])
    (define keys (sort (map symbol->string (hash-keys h)) string<?))
    (for ([k keys])
      (printf "~a,~a~n" k (hash-ref urls:status (string->symbol k))))
    ))

(define (report-json)
  (define rept (make-hash))
  (define urls (sort (map symbol->string (hash-keys urls:status)) string<?))
  (hash-set! rept 'urls urls)
  (hash-set! rept 'response-codes urls:status)
  (write-json rept))



                                             
;                                               
;  ;;;;;;;;; ;;;;;;;   ;;;;;  ;;;;;;;;;  ;;;;;  
;  ;;;;;;;;; ;;;;;;;  ;;;;;;; ;;;;;;;;; ;;;;;;; 
;     ;;;    ;;;      ;;;  ;;    ;;;    ;;;  ;; 
;     ;;;    ;;;;;;;  ;;;;       ;;;    ;;;;    
;     ;;;    ;;;;;;;   ;;;;;     ;;;     ;;;;;  
;     ;;;    ;;;          ;;;    ;;;        ;;; 
;     ;;;    ;;;      ;;  ;;;    ;;;    ;;  ;;; 
;     ;;;    ;;;;;;;  ;;;;;;;    ;;;    ;;;;;;; 
;     ;;;    ;;;;;;;   ;;;;;     ;;;     ;;;;;  
;                                               

(module+ test
  (require chk)
  (chk
   ;; Do we generate a good default URL?
   (#:= (url "http" false "localhost" 4000 true (list (path/param "" empty)) empty false)
    (make-url))
   ;; Can we override pieces?
   (#:= (url "https" false "localhost" 4000 true (list (path/param "" empty)) empty false)
    (make-url #:scheme "https"))
   (#:t (check-200? (string->url
                     "https://cdn.jsdelivr.net/npm/chart.js@2.9.3/dist/Chart.min.js")))
   )
  )

   