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

;; define main button bitmaps
(define wp-admin-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/admin.jpg")))
(define wp-frontpage-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/frontpage.jpg")))
(define wp-mysql-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/mysql.jpg")))
(define wp-themes-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/themes.jpg")))
(define wp-plugins-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/plugins.jpg")))
(define wp-docs-bitmap (read-bitmap  (build-path (iwp-resource-dir-path) "images/docs.jpg")))