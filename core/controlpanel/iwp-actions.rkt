#lang racket

;; This module implements IWP actions
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; open the wp-frontpage
 do-wpfrontpage-action
 ;; open wp admin
 do-wpadmin-action
 ;; open the plugins folder
 do-plugins-action
 ;; open the themes folder
 do-themes-action
 ;; open phpmyadmin
 do-mysql-action
 ;; open the docs
 do-docs-action
 ;; open about doc
 do-about-action)

;; —————————————————————————————————
;; import and implementation section

(require
  "iwp-constants.rkt"
  "iwp-environment.rkt")

(define (do-wpfrontpage-action)
  (do-iwpcli-action IWPCLI_WPFRONTPAGE))

(define (do-wpadmin-action)
  (do-iwpcli-action IWPCLI_WPADMIN))

(define (do-plugins-action)
  (do-iwpcli-action IWPCLI_PLUGINS))

(define (do-themes-action)
  (do-iwpcli-action IWPCLI_THEMES))

(define (do-mysql-action)
  (do-iwpcli-action IWPCLI_MYSQL))

(define (do-docs-action)
  (do-iwpcli-action IWPCLI_DOCS))

(define (do-about-action)
  (do-iwpcli-action IWPCLI_ABOUT))

(define (do-iwpcli-action command)
  (system (iwpcli-command-string command)))

(define (iwpcli-command-string command)
  (print iwpcli-run-path)
  (string-append (path->string (iwpcli-run-path)) " " command))
