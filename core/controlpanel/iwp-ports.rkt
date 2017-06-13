#lang racket

;; This module implements IWP port functions
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd 


(provide
 ;; get a port setting (get-port-setting "HTTP")
 get-port-setting
 ;; get the vm http port setting
 get-vm-http-port)

;; —————————————————————————————————
;; import and implementation section

(require
  "iwp-config.rkt"
  "iwp-constants.rkt")

;; get a port setting
(define (get-port-setting port-type)
  (define local-portoffset (get-config-setting PORTOFFSET))
  (define local-port-type (get-config-setting port-type))
  (+ (string->number local-port-type) (string->number local-portoffset)))

;; get vm http port
(define (get-vm-http-port)
  (get-port-setting HTTP))