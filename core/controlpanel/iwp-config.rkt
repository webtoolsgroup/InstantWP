#lang racket

;; This module implements IWP ini file functions
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd 


(provide
 ;; get the hash of configuration settings
 ;; (hash-ref (get-config-hash "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini") "QEMUBinary")
 get-config-hash
 ;; get one config setting (get-config-setting "QEMUBinary")
 get-config-setting)

;; —————————————————————————————————
;; import and implementation section

(require (only-in
          racket/string
          string-trim
          string-split
          string-join)
          racket/gui
          "iwp-environment.rkt")

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