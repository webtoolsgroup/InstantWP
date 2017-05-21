#lang racket

#|
logger.rkt --- Instant WordPress logging module
Copyright (c) 2010-2015 Corvideon Ltd http://www.corvideon.ie
Author: Seamus Brady seamus@corvideon.ie
Created: 30/05/2015
Homepage: http://www.instantwp.com
|#

(require racket/date
         racket/runtime-path)

(provide iwp-log)

;; log file path
(define iwp-log-file (build-path (current-directory) "log/iwp.log"))

;; log file port
(define iwp-log-output (open-output-file iwp-log-file #:exists 'append))

;; log message to file
(define (iwp-log message) 
  (display (string-append (date->string (current-date) #t) " - " message "\n") iwp-log-output)
  (close-output-port iwp-log-output))
