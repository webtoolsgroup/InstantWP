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
  "iwp-config.rkt"
  "iwp-constants.rkt"
  "iwp-resources.rkt"
  "iwp-environment.rkt"
  "iwp-actions.rkt"
  "iwp-quit-dialog.rkt")

;; --------------------------------------
;; button and label creation functions
;; --------------------------------------

;; get the button sleep delay

(define BUTTON_SLEEP (string->number (get-config-setting "buttonSleepAfterClick")))

(define (create-new-button parent bitmap label width height callback)
  (new button% [parent parent]
     [label (list bitmap (lpad label) 'left)]
     [min-width width]
     [min-height height]
     [callback (lambda (button event)
                       (callback)
                       (send button enable #f)
                       (sleep BUTTON_SLEEP)
                       (send button enable #t))]))


(define (create-new-label label width height parent)
  (new message%
     [label label]
     [enabled LBL_ENABLED]
     [min-width width]
     [min-height height]
     [parent parent]
     [stretchable-height #f]))

(define (lpad label-string)
  (string-append LEFT_PADDING  label-string))

;; --------------------------------------
;; define main window
;; --------------------------------------

;; define root window value hash
(define iwp-window-hash (make-hash))

;; set the root window hash values
(hash-set! iwp-window-hash "label" IWP_DIALOG_TITLE)
(hash-set! iwp-window-hash "width" MAIN_GUI_WIDTH)
(hash-set! iwp-window-hash "height" MAIN_GUI_HEIGHT)

;; turn on/off the WP Resources in tab list
(define (get-tab-list)
  (cond
    [(string=? (get-config-setting "showWPResourcesTab") "no") (list CONTROL_PANEL_TAB ADVANCED_TAB)]
    [(string=? (get-config-setting "showWPResourcesTab") "yes") (list CONTROL_PANEL_TAB ADVANCED_TAB WP_RESOURCES_TAB)]))

;; set up tabs
(define my-tabs-list (get-tab-list))


;; custom frame with exit - main window
(define root-window (new (class frame% (super-new)
                         (define/augment (on-close)
                           (do-exit)))
                       [label (hash-ref iwp-window-hash "label")]
                       [width (hash-ref iwp-window-hash "width")]
                       [height (hash-ref iwp-window-hash "height")]
                       [style '(no-resize-border)]))


;; logo at the top of dialog
(define logo (iwp-logo))
(define logo-label (new message% (parent root-window) (label logo)))


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
  (send root-window show #t)
  (send root-window center))

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

;; start panel vertical panel for holding horizontal panels
(define start-panel-v0 (new vertical-pane% (parent start-panel) [alignment '(center center)] [border 10]))

;; start panel horizontal panels
(define start-panel-h0 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))
(define start-panel-h1 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))
(define start-panel-h2 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))
(define start-panel-h3 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))
(define start-panel-h4 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))
(define start-panel-h5 (new horizontal-pane% (parent start-panel-v0) [alignment '(center top)]))

(define wp-frontpage-button
  (create-new-button start-panel-h0 (wp-frontpage-bitmap) "WordPress Frontpage"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-wpfrontpage-action))

(define wp-frontpage-label
  (create-new-label WPFRONTPAGE_INFO MAIN_LBL_WIDTH 10 start-panel-h0))

(define wp-admin-button
  (create-new-button start-panel-h1 (wp-admin-bitmap) "WordPress Admin"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-wpadmin-action))

(define wp-admin-label
  (create-new-label WPADMIN_INFO MAIN_LBL_WIDTH MAIN_LBL_HEIGHT start-panel-h1))


(define wp-themes-button
  (create-new-button start-panel-h2 (wp-themes-bitmap) "Themes Folder"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-themes-action))

(define wp-themes-label
  (create-new-label WPTHEMES_INFO MAIN_LBL_WIDTH MAIN_LBL_HEIGHT start-panel-h2))

(define wp-plugins-button
  (create-new-button start-panel-h3 (wp-plugins-bitmap) "Plugin Folder"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-plugins-action))

(define wp-plugins-label
  (create-new-label WPPLUGINS_INFO MAIN_LBL_WIDTH MAIN_LBL_HEIGHT start-panel-h3))

(define wp-mysql-button
  (create-new-button start-panel-h4 (wp-mysql-bitmap) "MySQL Admin"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-mysql-action))

(define wp-mysql-label
  (create-new-label MYSQL_INFO MAIN_LBL_WIDTH MAIN_LBL_HEIGHT start-panel-h4))

(define docs-button
  (create-new-button start-panel-h5 (wp-docs-bitmap) "Help"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-docs-action))

(define docs-label
  (create-new-label DOCS_INFO MAIN_LBL_WIDTH MAIN_LBL_HEIGHT start-panel-h5))


;; About button definition
(define about-button
  (create-new-button base-buttons-panel (about-bitmap) "About"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-about-action))

;; Quit button definition
(define quit-button
  (create-new-button base-buttons-panel (quit-bitmap) "Quit"
                     MAIN_BTN_WIDTH MAIN_BTN_HEIGHT
                     do-quit-action))

;; --------------------------------------
;; advanced panel control definitions
;; --------------------------------------

;; start panel vertical panel for holding horizontal panels
(define advanced-panel-v0 (new vertical-pane% (parent advanced-panel) [alignment '(center center)] [border 10]))

;; advanced panel horizontal panels
(define advanced-panel-h0 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))
(define advanced-panel-h1 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))
(define advanced-panel-h2 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))
(define advanced-panel-h3 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))
(define advanced-panel-h4 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))
(define advanced-panel-h5 (new horizontal-pane% (parent advanced-panel-v0) [alignment '(center top)]))

;; SSH button and label
(define ssh-button
  (create-new-button advanced-panel-h0 (ssh-bitmap) "SSH Terminal"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-start-ssh))

(define ssh-label
  (create-new-label SSH_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h0))

;; SFTP button and label
(define sftp-button
  (create-new-button advanced-panel-h1 (sftp-bitmap) "SFTP Client"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-start-sftp))

(define sftp-label
  (create-new-label SFTP_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h1))


;; QEMU monitor button and label
(define qemu-button
  (create-new-button advanced-panel-h2 (qemu-bitmap) "QEMU Monitor"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-start-qemu-monitor))

(define qemu-label
  (create-new-label QEMU_MONITOR_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h2))


;; Web file manager button and label
(define filemanager-button
  (create-new-button advanced-panel-h3 (filemanager-bitmap) "Web File Manager"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-filemanager-action))

(define filemanager-label
  (create-new-label WEB_FILE_MANAGER_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h3))


;; Edit config button and label
(define edit-config-button
  (create-new-button advanced-panel-h4 (edit-config-bitmap) "Edit Config File"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-start-edit-config))

(define edit-config-label
  (create-new-label EDIT_CONFIG_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h4))

;; PHP info button and label
(define phpinfo-button
  (create-new-button advanced-panel-h5 (phpinfo-bitmap) "PHP Info"
                     ADV_BTN_WIDTH ADV_BTN_HEIGHT
                     do-phpinfo-action))

(define phpinfo-label
  (create-new-label PHP_INFO_INFO ADV_LBL_WIDTH ADV_LBL_HEIGHT advanced-panel-h5))



