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
  "iwp-main-dialog.rkt"
  "iwp-config.rkt")

;; define root window value hash
(define iwp-window-hash (make-hash))

;; get some constants
(define START_TIMEOUT (string->number (get-config-setting "StartProgressBarDelay")))
(define SLEEP_DELAY (string->number (get-config-setting "WebCheckTimeoutSeconds")))
(define START_PROGRESS_DELAY (string->number (get-config-setting "DelayPing")))


;; define root window
(hash-set! iwp-window-hash "label" STARTING_IWP_TITLE)
(hash-set! iwp-window-hash "width" START_QUIT_GUI_WIDTH)
(hash-set! iwp-window-hash "height" START_QUIT_GUI_HEIGHT)

(define root-window (new frame% [label (hash-ref iwp-window-hash "label")]
                             [width (hash-ref iwp-window-hash "width")]
                             [height (hash-ref iwp-window-hash "height")]
                             [style '(no-resize-border)]))

;; define panels
(define main-panel (new panel% (parent root-window)))
;; (define logo (iwp-logo))
;; (define logo-label (new message% (parent main-panel) (label logo)))

;; add progress bar
(define a-gauge (new gauge% [label ""]
                     [range 100]
                     [parent root-window]
                     [min-height 30]
                     [min-width 350]
                     [stretchable-width 350]
                     [vert-margin 0]))


;; the textbox to show progress
(define (do-progress-bar)
  (define wp-available #f)
  (for/list ([i (range START_TIMEOUT)] #:break (equal? #t wp-available))
    (send a-gauge set-value i)
    ;; test the web server once the progress bar has moved a bit
    (set! wp-available (is-wp-available? i))
    (cond
      [(not wp-available) (sleep SLEEP_DELAY)]))
  (after-progress-bar))

;; Show the frame by calling its show method
(define (show-start-window)
  (send root-window show #t)
  (send root-window center)
  (do-progress-bar))

;; what happens after the progress bar
(define (after-progress-bar)
  (send root-window show #f)
  (show-main-window))

;; call the web server vm
(define (is-wp-available? progress)
  (cond
    [(>= progress START_PROGRESS_DELAY) (is-vm-webserver-up?)]
    [(<= progress START_PROGRESS_DELAY) #f]))
