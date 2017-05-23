#lang racket/base

#|
iwp-inin-file.rkt --- InstantWP ini file functions
Copyright (c) 2010-2017 Corvideon Ltd http://www.corvideon.ie
Author: Seamus Brady seamus@corvideon.ie
Homepage: http://www.instantwp.com
License; GPLv3
|#

(require (only-in racket/string string-trim string-split string-join))

(provide read-ini-file)


;; get-setting-values
;; takes the list of ini file sections and gives back a list of settings values
;; (get-setting-values (get-section-contents (read-ini-file (open-input-file "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini"))))
(define (get-setting-values lst)
  (map (λ (l) (map cdr l)) lst))

;; get-setting-names
;; takes the list of ini file sections and gives back a list of settings names
;; (get-setting-names (get-section-contents (read-ini-file (open-input-file "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini"))))
(define (get-setting-names lst)
  (map (λ (l) (map car l)) lst))


;; get-section-details
;; takes the list of ini file sections and gives back a list of settings
;; (get-section-contents (read-ini-file (open-input-file "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini")))
(define (get-section-contents lst)
  (map cdr lst))

;; read-ini-file
;; takes an open input port and reads back the contents of an ini file as a list
;; (read-ini-file (open-input-file "/Users/seamus/GitHub/InstantWP/core/config/iwp-osx.ini")))
;; code provided by TonyG at https://github.com/tonyg/racket-inverted-index/blob/master/inifile.rkt
(define (read-ini-file [p (current-input-port)])
  (let loop ((name #f)
	     (acc '())
	     (sections '()))
    (define (finish-section)
      (if name
	  (cons (cons name (reverse acc)) sections)
	  sections))
    (define line (read-line p 'any))
    (if (eof-object? line)
	(reverse (finish-section))
	(let ((line (string-trim line)))
	  (cond
	   [(equal? line "") (loop name acc sections)]
	   [(eqv? (string-ref line 0) #\[) (loop (substring line 1 (- (string-length line) 1))
						 '()
						 (finish-section))]
	   [else
	    (define pieces (string-split line "="))
	    (loop name
		  (cons (list (car pieces) (string-join (cdr pieces) "=")) acc)
		  sections)])))))