#lang racket

;; This module implements the startup for Win
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd



;; ---------------------------------------
;; Constants
;; ---------------------------------------

(define RUN_EDIT_BAT "startQEMUMonitor.bat")
(define START_ARG "")

(shell-execute #f RUN_EDIT_BAT START_ARG (current-directory) 'SW_SHOWDEFAULT)