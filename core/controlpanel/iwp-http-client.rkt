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
  "iwp-config.rkt")


(define (is-vm-webserver-up?)
  (define response (fetch-url (string->url (get-phpinfo-url))))
  (cond
    [(string? response) (string-contains? response "PHP Version")]
    [(not (string? response)) response]))

(define (fetch-url url)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define response (call/input-url url
                                     get-pure-port
                                     port->string))
    response))
