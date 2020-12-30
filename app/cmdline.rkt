#lang racket
(require "linkcheck.rkt"
         "state.rkt")
(require racket/cmdline)

;; This parameter is only used here... so...
(define report-type (make-parameter "txt"))

(define (parse-command-line)
  (command-line
   #:program "linkcheck"
   #:once-each
   [("-v" "--verbose") "Makes lots of noise."
                       (debug? true)]
   [("-q" "--quiet") "Don't say anything while running."
                     (debug? false)
                     (quiet? true)]
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
   [("-a" "--alt-tag-length") al
                          "Sets minimum ALT tag length. Default is 3 characters."
                          (alt-tag-length (string->number al))]
   #:args ()
   (start #:scheme (local:scheme) #:port (local:port) #:host (local:host))
   (case (string->symbol (report-type))
     [(json) (report-json)]
     [(csv) (report-csv)]
     [(txt)
      (when (not (quiet?))
        (printf "~a good, ~a bad. ~a alt tags missing (of ~a total)~n"
                (get-count 'ok)
                (get-count 'ko)
                (get-alt-count 'ko)
                (+ (get-alt-count 'ko) (get-alt-count 'ok))))
      ])
   ;; Use the "bad" url count as the exit status.
   ;; This way, a perfectly clean run yields '0' as the exit status.
   (exit (get-count 'ko))
   ))

(parse-command-line)