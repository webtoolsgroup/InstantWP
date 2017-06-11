#lang racket

;; This module implements IWP main control panel window
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; show the main gui
 show-main-window
 ;; hide the main gui
 hide-main-window)

;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui/base
  "iwp-constants.rkt"
  "iwp-resources.rkt"
  "iwp-environment.rkt"
  "iwp-actions.rkt"
  "iwp-quit-dialog.rkt")

;; define root window value hash
(define iwp-window-hash (make-hash))

;; set the root window hash values
(hash-set! iwp-window-hash "label" IWP_DIALOG_TITLE)
(hash-set! iwp-window-hash "width" MAIN_GUI_WIDTH)
(hash-set! iwp-window-hash "height" MAIN_GUI_HEIGHT)

;; set up tabs
(define my-tabs-list (list CONTROL_PANEL_TAB
                           ADVANCED_TAB
                           WP_RESOURCES_TAB))

;; custom frame with exit - main window
(define root-window (new (class frame% (super-new)
                         (define/augment (on-close)
                           (do-exit)))
                       [label (hash-ref iwp-window-hash "label")]
                       [width (hash-ref iwp-window-hash "width")]
                       [height (hash-ref iwp-window-hash "height")]
                       [style '(no-resize-border)]))


;; define tab control
(define tab-control (new tab-panel%
                       (parent root-window)
                       (choices my-tabs-list)
                       (callback
                        (lambda (tpanel event)
                          (case (send tpanel get-selection)
                            ((0) (send tpanel change-children (lambda (children) (list start-panel))))
                            ((1) (send tpanel change-children (lambda (children) (list advanced-panel))))
                            ((2) (send tpanel change-children (lambda (children) (list wp-resources-panel)))))))))

;; define panels
(define start-panel (new panel% (parent tab-control)))
(define advanced-panel (new panel% (parent tab-control)))
(define wp-resources-panel (new panel% (parent tab-control)))
;; this panel holds the buttons at the bottom of dialog
(define base-buttons-panel (new horizontal-pane% (parent root-window) [alignment '(right bottom)] [stretchable-height #f]))

;; hide other tabs
(send tab-control change-children (lambda (children) (list start-panel)))

;; start panel horizontal panels
(define start-panel-h0 (new horizontal-pane% (parent start-panel) [alignment '(center top)]))
(define start-panel-h1 (new horizontal-pane% (parent start-panel) [alignment '(center center)]))
(define start-panel-h2 (new horizontal-pane% (parent start-panel) [alignment '(right bottom)]))


;; start panel button definitions

(define wp-admin-button (new button% [parent start-panel-h0]
     [label (wp-admin-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-wpadmin-action))]))

(define wp-frontpage-button (new button% [parent start-panel-h0]
     [label (wp-frontpage-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-wpfrontpage-action))]))

(define wp-themes-button (new button% [parent start-panel-h1]
     [label (wp-themes-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-themes-action))]))

(define wp-plugins-button (new button% [parent start-panel-h1]
     [label (wp-plugins-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-plugins-action))]))

(define mysql-button (new button% [parent start-panel-h2]
     [label (wp-mysql-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-mysql-action))]))

(define docs-button (new button% [parent start-panel-h2]
     [label (wp-docs-bitmap)]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-docs-action))]))


;; Quit button definition
(define about-button (new button% [parent base-buttons-panel]
     [label "About"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-about-action))]))

;; Quit button definition
(define quit-button (new button% [parent base-buttons-panel]
     [label "Quit"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (do-quit-action))]))


;; Show the frame by calling its show method
(define (show-main-window)
  (send root-window show #t))

;; Hide the frame by calling its show method
(define (hide-main-window)
  (send root-window show #f))

;; quit action
(define (do-quit-action)
  (cond
    [(should-quit-iwp?) (do-exit)]))

(define (do-exit)
  (message-box "InstantWP Close" "InstantWP will now close." #f '(ok no-icon))
  (hide-main-window)
  (show-quit-window)
  (do-quit-action))

