#lang racket

;; This module implements IWP quit panel window
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; show the start gui
 show-quit-window)

;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui
  "iwp-constants.rkt"
  "iwp-resources.rkt"
  "iwp-config.rkt"
  "iwp-actions.rkt")

;; define root window value hash
(define iwp-window-hash (make-hash))

;; get some constants
(define QUIT_TIMEOUT (string->number (get-config-setting "WaitOnVMQuitSeconds")))
(define SLEEP_DELAY (string->number (get-config-setting "QuitProgressBarDelay")))


;; define root window
(hash-set! iwp-window-hash "label" QUITTING_IWP_TITLE)
(hash-set! iwp-window-hash "width" START_QUIT_GUI_WIDTH)
(hash-set! iwp-window-hash "height" START_QUIT_GUI_HEIGHT)

(define root-window (new frame% [label (hash-ref iwp-window-hash "label")]
                             [width (hash-ref iwp-window-hash "width")]
                             [height (hash-ref iwp-window-hash "height")]
                             [style '(no-resize-border)]))

;; define panels
(define main-panel (new panel% (parent root-window)))
(define logo (iwp-logo))
(define logo-label (new message% (parent main-panel) (label logo)))

;; add progress bar
(define a-gauge (new gauge% [label QUITTING_LABEL]
                     [range 100]
                     [parent root-window]
                     [min-height 30]
                     [min-width 350]
                     [stretchable-width 350]
                     [vert-margin 0]))


;; the textbox to show progress
(define (do-progress-bar)
  ;; start the quit cmd in new thread
  (thread (lambda () (do-quit-action)))
  (for/list ([i (range QUIT_TIMEOUT)])
    (send a-gauge set-value i)
    (sleep SLEEP_DELAY))
  (after-progress-bar))

;; Show the frame by calling its show method
(define (show-quit-window)
  (send root-window show #t)
  (do-progress-bar))

;; what happens after the progress bar
(define (after-progress-bar)
  (exit))
