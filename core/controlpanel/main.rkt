#lang racket

;; This module starts the IWP control panel
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; —————————————————————————————————
;; import and implementation section

(require racket/gui
         racket/runtime-path
         "iwp-config.rkt"
         "iwp-start-dialog.rkt"
         "iwp-main-dialog.rkt"
         )

;; setup some defaults to avoid console window
(current-output-port (open-output-nowhere))

;; var to hold config hash
(define iwp-config-hash (get-config-hash "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini"))

(module+ main
  ;; load config
  (display "Starting IWP...\n")
  (show-start-window))