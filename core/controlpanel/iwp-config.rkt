#lang racket

;; This module implements configuration functions for IWP
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd 


(provide
 ;; get the hash of configuration settings
 ;; (hash-ref (get-config-hash "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini") "QEMUBinary")
 get-config-hash
 ;; get one config setting (get-config-setting "QEMUBinary")
 get-config-setting
 ;; get a port setting (get-port-setting "HTTP")
 get-port-setting
 ;; get the vm http port setting
 get-vm-http-port
 ;; get the phpinfo url
 get-phpinfo-url
 ;; web filemanager url
 get-filemanager-url
 ;; wp resources url
 get-wp-resources-url
 ;; ssh path
 get-ssh-script-path
 ;; sftp path
 get-sftp-script-path
 ;; qemu monitor path
 get-qemu-script-path
 ;; edit config file path
 get-edit-config-script-path
 ;; start terminal path
 get-start-terminal-script-path
 ;; quit iwp script
 get-quit-iwp-script-path
 ;; open url script path
 get-open-url-script)


;; —————————————————————————————————
;; import and implementation section

(require (only-in
          racket/string
          string-trim
          string-split
          string-join)
          racket/gui
          "iwp-environment.rkt"
          "iwp-constants.rkt")

;; define a local config hash
(define local-config-hash (make-hash))

;; get a config setting
(define (get-config-setting setting)
  (get-config-hash (iwp-config-file-path))
  (hash-ref local-config-hash setting))


;; open config file port - this closes the file automatically
(define (call-with-input-config-file port config-path-string)
    (call-with-input-file config-path-string
      (λ (port)
        (read-ini-file port))))


;; define the file and port and load
(define (load-file-contents config-path-string) 
  (let* ([iwp-config-port (open-input-file config-path-string)]
         [iwp-config-file-contents (call-with-input-config-file iwp-config-port config-path-string)])
    iwp-config-file-contents))

;; handle error loading config file
(define (exit-with-config-error)
  (message-box "IWP Config Error" "Error loading IWP config file! \nIWP will now exit." #f '(ok no-icon))
  (exit))

;; returns a hash of config items
(define (get-config-hash config-path-string)
  (with-handlers ([exn:fail? (λ (exn) (exit-with-config-error))])
    (define ini-file-contents (load-file-contents config-path-string))
    (define ini-file-sections (get-section-contents ini-file-contents))
    (get-setting-values ini-file-sections)
    local-config-hash))

;; adds settings to config hash
(define (add-setting-to-hash lst)
  (map (λ (l)
         (hash-set! local-config-hash (first l)  (second l)))
       lst))

;; returns a list of settings values
(define (get-setting-values lst)
  (map add-setting-to-hash lst))

;; takes the list of ini file sections and gives back a list of settings
(define (get-section-contents lst)
  (map rest lst))

;; read-ini-file
;; code provided by TonyG at https://github.com/tonyg/racket-inverted-index/blob/master/inifile.rkt
(define (read-ini-file [p (current-input-port)])
  (let loop ((name #f)
	     (acc '())
	     (sections '()))
    (define (local-finish-section)
      (if name
	  (cons (cons name (reverse acc)) sections)
	  sections))
    (define local-line (read-line p 'any))
    (if (eof-object? local-line)
	(reverse (local-finish-section))
	(let ((line (string-trim local-line)))
	  (cond
	   [(equal? line "") (loop name acc sections)]
	   [(eqv? (string-ref line 0) #\[) (loop (substring line 1 (- (string-length line) 1))
						 '()
						 (local-finish-section))]
	   [else
	    (define local-pieces (string-split line "="))
	    (loop name
		  (cons (list (car local-pieces) (string-join (cdr local-pieces) "=")) acc)
		  sections)])))))

;; get vm http port
(define (get-vm-http-port)
  (get-port-setting HTTP))

;; get a port setting
(define (get-port-setting port-type)
  (define local-portoffset (get-config-setting PORTOFFSET))
  (define local-port-type (get-config-setting port-type))
  (+ (string->number local-port-type) (string->number local-portoffset)))

;; phpinfo url
(define (get-phpinfo-url)
  (define local-portoffset (get-config-setting PORTOFFSET))
  (string-append "http://" LOCALHOST ":" (number->string (get-vm-http-port)) "/" (get-config-setting PHP_INFO)))

;; filemanager url
(define (get-filemanager-url)
  (define local-portoffset (get-config-setting PORTOFFSET))
  (string-append "http://" LOCALHOST ":" (number->string (get-vm-http-port)) "/" (get-config-setting WEB_FILEMANAGER)))

;; filemanager url
(define (get-wp-resources-url)
  "https://www.instantwp.com/unleashed")

;; startSSHScript
(define (get-ssh-script-path)
   (define local-ssh-script-path (get-config-setting START_SSH))
  (build-path (path->string (iwp-platform-dir-path)) local-ssh-script-path))

;; startSFTPScript
(define (get-sftp-script-path)
   (define local-sftp-script-path (get-config-setting START_SFTP))
  (build-path (path->string (iwp-platform-dir-path)) local-sftp-script-path))

;; startQEMUMonitorScript
(define (get-qemu-script-path)
   (define local-qemu-script-path (get-config-setting START_QEMU))
  (build-path (path->string (iwp-platform-dir-path)) local-qemu-script-path))

;; startEditConfigFileScript
(define (get-edit-config-script-path)
   (define local-edit-config-script-path (get-config-setting START_EDIT_CONFIG))
  (build-path (path->string (iwp-platform-dir-path)) local-edit-config-script-path))

;; quit iwp script
(define (get-quit-iwp-script-path)
   (define local-edit-config-script-path (get-config-setting QUIT_SCRIPT))
  (build-path (path->string (iwp-platform-dir-path)) local-edit-config-script-path))

;; startTerminal script
(define (get-start-terminal-script-path)
   (define local-edit-config-script-path (get-config-setting START_TERMINAL))
  (build-path (path->string (iwp-platform-dir-path)) local-edit-config-script-path))

;; open URL script
(define (get-open-url-script)
   (define local-edit-config-script-path (get-config-setting OPEN_URL))
  (build-path (path->string (iwp-platform-dir-path)) local-edit-config-script-path))