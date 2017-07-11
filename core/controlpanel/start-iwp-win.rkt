#lang racket

;; This module implements the startup for Win
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd



;; ---------------------------------------
;; Constants
;; ---------------------------------------

(define RUN_IWP_CLI "run-iwpcli.bat")
(define START_ARG "start")

(shell-execute #f RUN_IWP_CLI START_ARG (current-directory) 'SW_HIDE)
