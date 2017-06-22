#lang racket

;; This module starts the IWP control panel
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; —————————————————————————————————
;; import and implementation section

(require racket/gui
         "iwp-start-dialog.rkt")

;; setup some defaults to avoid console window
(current-output-port (open-output-nowhere))

(show-start-window)
