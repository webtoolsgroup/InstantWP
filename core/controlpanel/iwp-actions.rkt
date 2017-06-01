#lang racket

;; This module implements IWP actions
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; open the wp-frontpage
 do-wpfrontpage-action)

;; —————————————————————————————————
;; import and implementation section

(require
  "iwp-constants.rkt"
  "iwp-environment.rkt")

(define (do-wpfrontpage-action)
  (do-iwpcli-action IWPCLI_WPFRONTPAGE))

(define (do-iwpcli-action command)
  (system (iwpcli-command-string command)))

(define (iwpcli-command-string command)
  (string-append (path->string (iwpcli-path)) " " command))
