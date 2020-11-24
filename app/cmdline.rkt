#lang racket
(require "check.rkt")

(require racket/cmdline)
(define report-type (make-parameter "txt"))

(command-line
 #:program "linkcheck"
 #:once-each
 [("-v" "--verbose") "Makes lots of noise."
                     (debug? true)]
 [("-s" "--https") "Set the transport scheme to 'https'. Defaults to 'http'."
                   (local:scheme "https")
                   (local:port 443)]
 [("-u" "--host") host
                  "Set the host. Defaults to 'localhost'."
                  (local:host host)]
 [("-p" "--port") port
                  "Set the port. Defaults to 4000."
                  (local:port (string->number port))]
 [("-r" "--report") rpt
                    "Sets report type. Either 'json', 'csv', or 'txt'. Default is 'txt'."
                    (report-type rpt)]
 #:args ()
 (start #:scheme (local:scheme) #:port (local:port) #:host (local:host))
 (define ok-count (hash-count urls:ok))
 (define ko-count (hash-count urls:ko))
 (case (string->symbol (report-type))
   [(json) (report-json)]
   [(csv) (report-csv)]
   [(txt)
    (printf "~a good, ~a bad.~n" ok-count ko-count)
    ])
 ;; Use the "bad" url count as the exit status.
 (exit ko-count)
 )