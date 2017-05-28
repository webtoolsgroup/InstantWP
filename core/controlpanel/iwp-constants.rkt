#lang racket


;; This module implements IWP constants
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; list of constants declared
(provide GUI_WIDTH
         GUI_HEIGHT
         IWP_DIALOG_TITLE
         CONTROL_PANEL_TAB
         ADVANCED_TAB
         WP_RESOURCES_TAB)

;; —————————————————————————————————
;; import and implementation section

(define GUI_WIDTH 750)
(define GUI_HEIGHT 400)
(define IWP_DIALOG_TITLE "InstantWP Control Panel")
(define CONTROL_PANEL_TAB "Control Panel")
(define ADVANCED_TAB "Advanced")
(define WP_RESOURCES_TAB "WordPress Resources")