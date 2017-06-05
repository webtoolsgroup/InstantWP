#lang racket

;; This module contains IWP http check code
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd


;; —————————————————————————————————
;; import and implementation section
(require
  net/http-client
  "iwp-constants.rkt"
  "iwp-ports.rkt")

(define (is-vm-webserver-up?)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define local-http-conn (http-conn-open LOCALHOST #:port (get-vm-http-port)))
    (http-conn? local-http-conn)))

