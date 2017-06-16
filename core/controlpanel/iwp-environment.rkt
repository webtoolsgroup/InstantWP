#lang racket

;; This module sets up the IWP environment
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; the path to the config file
 iwp-config-file-path
 ;; the path to iwpcli
 iwpcli-run-path
 ;; the path to control panel resources
 iwp-images-dir-path
 ;; path for platform dir
 iwp-platform-dir-path
 ;; start terminal script
 start-terminal-script-path
 ;; identify OS
 is-windows?
 is-macos?)


;; —————————————————————————————————
;; import and implementation section
(require
  racket/gui/base
  racket/runtime-path
  "iwp-constants.rkt")


;; get the local path of the executable
;; and adjust if we are in DrRacket
(define (get-dir-of-current-script)
  (define local-script-path (get-path-to-current-stript))
  (cond
    [(is-dev-mode? local-script-path) (current-directory)]
    [else local-script-path]))

;; returns the current script path or DrRacket path
(define (get-path-to-current-stript)
  (path->string
   (path-only
    (path->complete-path
     (find-system-path 'run-file)))))

;; tell if we are running in DrRacket
(define (is-dev-mode? script-path)
  (string-contains? script-path "DrRacket"))

;; iwpcli file path
(define (iwpcli-run-path)
  (build-path (get-iwp-root-dir) (iwpcli-runscript-name)))

;; config file path
(define (iwp-config-file-path)
  (build-path (get-iwp-root-dir) IWP_CONFIG_DIR (iwpcli-config-filename)))

;; resource path
(define (iwp-images-dir-path)
  (build-path  (get-iwp-root-dir) "images"))

;; platform path
(define (iwp-platform-dir-path)
  (build-path  (get-iwp-root-dir) "platform"))

;; startTerminal path
(define (start-terminal-script-path)
  (build-path (iwp-platform-dir-path) START_TERMINAL))


;; search for the root directory of IWP
(define (search-for-iwp-root-dir)
  (let ([test-path (clean-and-simplify-path (build-path (get-dir-of-current-script) ".."))] )
    (for/list ([i (range 10)]
               #:break (file-exists? (build-path test-path (iwpcli-executable-name))))
      (set! test-path (build-path test-path ".."))) 
    test-path))

;; build the test path for search
(define (clean-and-simplify-path path)
  (define local-cleaned-path (cleanse-path path))
  (define local-simpler-path (simplify-path local-cleaned-path #t))
  (path->directory-path local-simpler-path))

;; get the iwp root dir
(define (get-iwp-root-dir)
  (define local-iwp-root-dir (search-for-iwp-root-dir))
  (define local-iwp-root-test  (build-path local-iwp-root-dir (iwpcli-executable-name)))
  (cond
    [(file-exists? local-iwp-root-test) local-iwp-root-dir]
    [(not (file-exists? local-iwp-root-test)) (exit-with-root-dir-error local-iwp-root-test)]))

;; handle error finding iwp root dr
(define (exit-with-root-dir-error iwp-root-test)
  (define local-exit-msg (string-append "IWP root directory is incorrect: "  (path->string iwp-root-test)  "\nIWP will now exit."))
  (message-box "IWP Config Error" local-exit-msg #f '(ok no-icon))
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

;; name of the run iwpcli shell script
(define (iwpcli-runscript-name)
  (cond
    [(is-windows?) RUN_IWPCLI_WIN]
    [(is-macos?) RUN_IWPCLI_MAC]))

;; name of the config file
(define (iwpcli-config-filename)
  (cond
    [(is-windows?) IWP_CONFIG_FILE_WIN]
    [(is-macos?) IWP_CONFIG_FILE_MAC]))

