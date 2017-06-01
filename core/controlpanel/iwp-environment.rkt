#lang racket

;; This module contains IWP environment code
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; the path to the config file
 iwp-config-file-path
 ;; the path to iwpcli
 iwpcli-path
 ;; the path to control panel resources
 iwp-resource-dir-path)


;; —————————————————————————————————
;; import and implementation section
(require
  racket/gui/base
  control
  "iwp-constants.rkt")


;; iwpcli file path
(define (iwpcli-path)
  (build-path (get-iwp-root-dir) (iwpcli-executable-name)))

;; config file path
(define (iwp-config-file-path)
  (build-path (get-iwp-root-dir) IWP_CONFIG_DIR (iwpcli-config-name)))

;; resource path
(define (iwp-resource-dir-path)
  (build-path (current-directory) "resources/"))

;; search for the root directory of IWP
(define (search-for-iwp-root-dir)
  (let ([test-path (build-path (current-directory) "..")] )
    (for/list ([i (range 10)]
               #:break (file-exists? (build-path test-path (iwpcli-executable-name))))
      (set! test-path (build-path test-path ".."))) 
    test-path))

;; get the iwp root dir
(define (get-iwp-root-dir)
  (define iwp-root-dir (search-for-iwp-root-dir))
    (cond
    [(file-exists? (build-path iwp-root-dir (iwpcli-executable-name))) iwp-root-dir]
    [(file-exists? (build-path iwp-root-dir (iwpcli-executable-name))) (exit-with-root-dir-error)]))

;; handle error finding iwp root dr
(define (exit-with-root-dir-error)
  (message-box "IWP Config Error" "Error finding IWP roor directory! \nIWP will now exit." #f '(ok no-icon))
  (exit))

;; are we on macos?
(define (is-macos?)
  (equal? (system-type) 'macosx))

;; are we on windows?
(define (is-windows?)
  (equal? (system-type) 'windows))

;; name of the iwpcli executable
(define (iwpcli-executable-name)
  (cond
    [(is-windows?) IWPCLI_WIN]
    [(is-macos?) IWPCLI_MAC]))

;; name of the config file
(define (iwpcli-config-name)
  (cond
    [(is-windows?) IWP_CONFIG_FILE_WIN]
    [(is-macos?) IWP_CONFIG_FILE_MAC]))

