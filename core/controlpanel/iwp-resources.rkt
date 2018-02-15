#lang racket


;; This module implements IWP resources such as bitmaps
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; list of resources declared
(provide (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui/base
  "iwp-environment.rkt")

;; are we on macos?
(define (is-macos-bitmap?)
  (equal? (system-type) 'macosx))

;; are we on windows?
(define (is-windows-bitmap?)
  (equal? (system-type) 'windows))

;; define main button bitmaps
(define (wp-admin-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "settings.png")))

(define (wp-frontpage-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "world.png")))

(define (wp-mysql-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "database.png")))

(define (wp-themes-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "folder.png")))

(define (wp-plugins-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "folder.png")))

(define (wp-docs-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "documents.png")))

(define (about-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "project.png")))

(define (quit-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "quit.png")))

(define (ssh-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "ssh.png")))

(define (sftp-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "sftp.png")))

(define (qemu-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "config.png")))

(define (webconsole-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "settings.png")))

(define (edit-config-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "pencil.png")))

(define (phpinfo-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "settings.png")))

(define (iwp-logo)
  (read-bitmap  (build-path (iwp-images-dir-path) "logo-top.png")))

(define (wp-resources)
  (read-bitmap  (build-path (iwp-images-dir-path) "IWP-small.png")))

(define (info-sign)
  (read-bitmap  (build-path (iwp-images-dir-path) "info.png")))

;; start up wizard bitmap
(define (readme-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "readme1.png")))

(define (deploy-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) "get-unleashed.png")))

(define (deploy2-bitmap) 
  (cond
    [(is-windows-bitmap?)  (read-bitmap  (build-path (iwp-images-dir-path) "deploy-button.png"))]
    [(is-macos-bitmap?) (read-bitmap  (build-path (iwp-images-dir-path) "deploy-button-mac.png")) ]))
