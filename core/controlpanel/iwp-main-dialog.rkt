#lang racket


;; This module implements IWP main control panel window
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; show the main gui
 show-main-window)

;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui/base
  "iwp-constants.rkt"
  "iwp-environment.rkt")


;; define root window value hash
(define iwp-window-hash (make-hash))

;; set the root window hash values
(hash-set! iwp-window-hash "label" IWP_DIALOG_TITLE)
(hash-set! iwp-window-hash "width" GUI_WIDTH)
(hash-set! iwp-window-hash "height" GUI_HEIGHT)

;; set up tabs
(define my-tabs-list (list CONTROL_PANEL_TAB
                           ADVANCED_TAB
                           WP_RESOURCES_TAB))

;; main window
(define root-window (new frame% [label (hash-ref iwp-window-hash "label")]
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

;; hide other tabs
(send tab-control change-children (lambda (children) (list start-panel)))

;; start panel horizontal panels
(define start-panel-h0 (new horizontal-pane% (parent start-panel) [alignment '(center top)]))
(define start-panel-h1 (new horizontal-pane% (parent start-panel) [alignment '(center center)]))
(define start-panel-h2 (new horizontal-pane% (parent start-panel) [alignment '(center bottom)]))


;; start panel button definitions

(define wp-admin-button (new button% [parent start-panel-h0]
     [label wp-admin-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Admin"))]))

(define wp-frontpage-button (new button% [parent start-panel-h0]
     [label wp-frontpage-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Frontpage"))]))

(define wp-themes-button (new button% [parent start-panel-h1]
     [label wp-themes-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Themes"))]))

(define wp-plugins-button (new button% [parent start-panel-h1]
     [label wp-plugins-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Plugins"))]))

(define mysql-button (new button% [parent start-panel-h2]
     [label wp-mysql-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "MySql"))]))

(define docs-button (new button% [parent start-panel-h2]
     [label wp-docs-bitmap]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Docs"))]))


;; Quit button definition
(define quit-button (new button% [parent root-window]
     [label "Quit"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Quit"))]))

; Show the frame by calling its show method
(define (show-main-window)
  (send root-window show #t))
