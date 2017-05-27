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
  "iwp-constants.iwp")


;; define root window value hash
(define iwp-window-hash (make-hash))

;; set the root window hash values
(hash-set! iwp-window-hash "label" IWP_DIALOG_TITLE)
(hash-set! iwp-window-hash "width" 750)
(hash-set! iwp-window-hash "height" 400)

;; set up tabs
(define my-tabs-list (list CONTROL_PANEL_TAB
                           ADVANCED_TAB
                           WP_RESOURCES_TAB))

;; main window
(define root-wnd (new frame% [label (hash-ref iwp-window-hash "label")]
                             [width (hash-ref iwp-window-hash "width")]
                             [height (hash-ref iwp-window-hash "height")]
                             [style '(no-resize-border)]))

;; define tab panel
(define tab-panel (new tab-panel%
                             (parent root-wnd)
                             (choices my-tabs-list)
                             (callback
                               (lambda (tpanel event)
                                (case (send tpanel get-selection)
                                 ((0) (send tpanel change-children (lambda (children) (list 0-panel))))
                                 ((1) (send tpanel change-children (lambda (children) (list 1-panel))))
                                 ((2) (send tpanel change-children (lambda (children) (list 2-panel)))))))))
;; define panels
(define 0-panel (new panel% (parent tab-panel)))
;; (define a-text (new message% (parent 0-panel) (label "Control Panel")))

;; horizontal panel
(define 0-panel-h0 (new horizontal-pane% (parent 0-panel) [alignment '(center top)]))
(define 0-panel-h1 (new horizontal-pane% (parent 0-panel) [alignment '(center center)]))
(define 0-panel-h2 (new horizontal-pane% (parent 0-panel) [alignment '(center bottom)]))

(define 1-panel (new panel% (parent tab-panel)))
;(define b-text (new message% (parent 1-panel) (label "Advanced")))
(define 2-panel (new panel% (parent tab-panel)))
;(define c-text (new message% (parent 2-panel) (label "Resources")))

;; hide other tabs
(send tab-panel change-children (lambda (children) (list 0-panel)))

;; resource path
(define *iwp-resource-path* (build-path (current-directory) "resources/"))

;; define resources
(define wp-admin-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/admin.jpg")))
(define wp-frontpage-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/frontpage.jpg")))
(define wp-mysql-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/mysql.jpg")))
(define wp-themes-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/themes.jpg")))
(define wp-plugins-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/plugins.jpg")))
(define wp-docs-bitmap (read-bitmap  (build-path *iwp-resource-path* "images/docs.jpg")))

; Make a button in the frame
(new button% [parent 0-panel-h0]
     [label wp-admin-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Admin"))])

(new button% [parent 0-panel-h0]
     [label wp-frontpage-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Frontpage"))])

(new button% [parent 0-panel-h1]
     [label wp-themes-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Themes"))])

(new button% [parent 0-panel-h1]
     [label wp-plugins-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Plugins"))])

(new button% [parent 0-panel-h2]
     [label wp-mysql-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "MySql"))])

(new button% [parent 0-panel-h2]
     [label wp-docs-bitmap]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Docs"))])
 
; Show the frame by calling its show method
(define (show-main-window)
  (send root-wnd show #t))
