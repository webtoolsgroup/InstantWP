#lang racket

;; This module contains IWP http check code
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; ping web server
 is-vm-webserver-up?)

;; —————————————————————————————————
;; import and implementation section
(require
  net/http-client
  net/url
  "iwp-constants.rkt"
  "iwp-ports.rkt")

(define phpinfo-url (string-append "http://" LOCALHOST ":" (number->string (get-vm-http-port)) "/" PHP_INFO))

(define (is-vm-webserver-up?)
  (define response (fetch-url (string->url phpinfo-url)))
  (cond
    [(string? response) (string-contains? response "PHP Version")]
    [(not (string? response)) response]))

(define (fetch-url url)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define response (call/input-url url
                                     get-pure-port
                                     port->string))
    response))
