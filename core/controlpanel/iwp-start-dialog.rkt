#lang racket

;; This module implements IWP start panel window
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; show the start gui
 show-start-window)

;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui
  "iwp-constants.rkt"
  "iwp-resources.rkt"
  "iwp-http-client.rkt"
  "iwp-main-dialog.rkt")

;; define root window value hash
(define iwp-window-hash (make-hash))

;; define root window
(hash-set! iwp-window-hash "label" STARTING_IWP)
(hash-set! iwp-window-hash "width" START_QUIT_GUI_WIDTH)
(hash-set! iwp-window-hash "height" START_QUIT_GUI_HEIGHT)

(define root-window (new frame% [label (hash-ref iwp-window-hash "label")]
                             [width (hash-ref iwp-window-hash "width")]
                             [height (hash-ref iwp-window-hash "height")]
                             [style '(no-resize-border)]))

;; define panels
(define main-panel (new panel% (parent root-window)))
(define logo (read-bitmap "/Users/seamus/GitHub/InstantWP/core/images/logo.gif"))
(define logo-label (new message% (parent main-panel) (label logo)))

;; add progress bar
(define a-gauge (new gauge% [label "Loading..."]
                     [range 100]
                     [parent root-window]
                     [min-height 10]
                     [min-width 350]
                     [stretchable-width 350]
                     [vert-margin 0]))

;; start button definition
(define start-button (new button% [parent root-window]
     [label "Start"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Frontpage"))]))

;; Enable the start button
(define (enable-start-button)
  (send start-button enable #t))

;; Disable the start button
(define (disable-start-button)
  (send start-button enable #f))

;; the textbox to show progress
(define (do-progress-bar) 
  (for/list ([i (range 100)])
    (send a-gauge set-value i)
    (cond
      [(not (is-vm-webserver-up?)) (sleep 1)]))
  (after-progress-bar))

;; Show the frame by calling its show method
(define (show-start-window)
  (send root-window show #t)
  (disable-start-button)
  (do-progress-bar))

;; what happens after the progress bar
(define (after-progress-bar)
  (enable-start-button)
  (show-main-window)
  (send root-window show #f))
