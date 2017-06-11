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

;; --------------------------------------
;; define main window
;; --------------------------------------

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
(define advanced-panel (new vertical-panel% (parent tab-control)))
(define wp-resources-panel (new panel% (parent tab-control)))
;; this panel holds the buttons at the bottom of dialog
(define base-buttons-panel (new horizontal-pane% (parent root-window) [alignment '(right bottom)] [stretchable-height #f]))

;; hide other tabs
(send tab-control change-children (lambda (children) (list start-panel)))

;; --------------------------------------
;; main window control functions
;; --------------------------------------

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

;; --------------------------------------
;; start panel control definitions
;; --------------------------------------

;; start panel horizontal panels
(define start-panel-h0 (new horizontal-pane% (parent start-panel) [alignment '(center top)]))
(define start-panel-h1 (new horizontal-pane% (parent start-panel) [alignment '(center center)]))
(define start-panel-h2 (new horizontal-pane% (parent start-panel) [alignment '(right bottom)]))

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

;; --------------------------------------
;; advanced panel control definitions
;; --------------------------------------

;; advanced panel horizontal panels
(define advanced-panel-h0 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))
(define advanced-panel-h1 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))
(define advanced-panel-h2 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))
(define advanced-panel-h3 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))
(define advanced-panel-h4 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))
(define advanced-panel-h5 (new horizontal-pane% (parent advanced-panel) [alignment '(center top)]))

;; SSH button and label
(define do-ssh-button (new button% [parent advanced-panel-h0]
     [label "SSH Terminal"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "SSH"))]))

(define ssh-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h0]
                    [stretchable-height #f]))

(send ssh-label set-value SSH_INFO)

;; SFTP button and label
(define do-sftp-button (new button% [parent advanced-panel-h1]
     [label "SFTP Client"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "SFTP Client"))]))

(define sftp-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h1]
                    [stretchable-height #f]))

(send sftp-label set-value SFTP_INFO)

;; QEMU monitor button and label
(define do-monitor-button (new button% [parent advanced-panel-h2]
     [label "QEMU Monitor"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "QEMU Monitor"))]))

(define qemu-monitor-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h2]
                    [stretchable-height #f]))

(send qemu-monitor-label set-value QEMU_MONITOR_INFO)

;; Edit config button and label
(define do-edit-config-button (new button% [parent advanced-panel-h3]
     [label "Edit Config File"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Edit Config File"))]))

(define edit-config-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h3]
                    [stretchable-height #f]))

(send edit-config-label set-value EDIT_CONFIG_INFO)

;; EWeb file manager button and label
(define do-web-file-manager-button (new button% [parent advanced-panel-h4]
     [label "Web File Manager"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "Web File Manager"))]))

(define web-file-manager-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h4]
                    [stretchable-height #f]))

(send web-file-manager-label set-value WEB_FILE_MANAGER_INFO)

;; PHP info button and label
(define do-php-info-button (new button% [parent advanced-panel-h5]
     [label "PHP Info"]
     [min-width 150]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (display "PHP Info"))]))

(define php-info-label (new text-field%
                    [label ""]
                    [style '(multiple)]
                    [enabled #f]
                    [min-height 20]
                    [parent advanced-panel-h5]
                    [stretchable-height #f]))

(send php-info-label set-value PHP_INFO_INFO)

