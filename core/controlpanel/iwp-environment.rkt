#lang racket

;; This module contains IWP environment code
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; the path to the config file
 iwp-config-file
 ;; the path to control panel resources
 iwp-resource-dir-path
 ;; main control button bitmap definitions
 wp-admin-bitmap
 wp-frontpage-bitmap
 wp-mysql-bitmap
 wp-themes-bitmap
 wp-plugins-bitmap
 wp-docs-bitmap)


;; —————————————————————————————————
;; import and implementation section
(require
  racket/gui/base)


;; config file path
(define iwp-config-file (build-path "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini"))

;; resource path
(define iwp-resource-dir-path (build-path (current-directory) "resources/"))

;; get list of iwp paths
(define (get-iwp-paths)
  (pathlist-closure (list (build-path (current-directory)))))

;; get the iwp-clippath
(map (λ (path)
         (define test-path (build-path path "iwpcli"))
         (cond 
           [(file-exists? test-path)  test-path]))
         (get-iwp-paths))

;; define button bitmaps
(define wp-admin-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/admin.jpg")))
(define wp-frontpage-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/frontpage.jpg")))
(define wp-mysql-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/mysql.jpg")))
(define wp-themes-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/themes.jpg")))
(define wp-plugins-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/plugins.jpg")))
(define wp-docs-bitmap (read-bitmap  (build-path iwp-resource-dir-path "images/docs.jpg")))