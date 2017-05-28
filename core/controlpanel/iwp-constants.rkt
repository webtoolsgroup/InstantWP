#lang racket


;; This module implements IWP constants
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; list of constants declared
(provide MAIN_GUI_WIDTH
         MAIN_GUI_HEIGHT
         IWP_DIALOG_TITLE
         CONTROL_PANEL_TAB
         ADVANCED_TAB
         WP_RESOURCES_TAB
         START_QUIT_GUI_WIDTH
         START_QUIT_GUI_HEIGHT
         STARTING_IWP)

;; —————————————————————————————————
;; import and implementation section

(define MAIN_GUI_WIDTH 750)
(define MAIN_GUI_HEIGHT 400)
(define START_QUIT_GUI_WIDTH 400)
(define START_QUIT_GUI_HEIGHT 100)
(define IWP_DIALOG_TITLE "InstantWP Control Panel")
(define CONTROL_PANEL_TAB "Control Panel")
(define ADVANCED_TAB "Advanced")
(define WP_RESOURCES_TAB "WordPress Resources")
(define STARTING_IWP "Starting InstantWP...")