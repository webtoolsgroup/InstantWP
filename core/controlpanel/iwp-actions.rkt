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
 do-about-action
 ;; quit action
 do-quit-action
 ;; start ssh
 do-start-ssh
 ;; start sftp
 do-start-sftp
 ;; start qemu monitor
 do-start-qemu-monitor
 ;; start web filemanager
 do-filemanager-action
 ;; start php info page
 do-phpinfo-action
 ;; start edit config
 do-start-edit-config
 ;; should quit?
 should-quit-iwp?
 ;; test
 iwpcli-command-string)

;; —————————————————————————————————
;; import and implementation section

(require
    racket/gui/base
    net/sendurl
    "iwp-constants.rkt"
    "iwp-environment.rkt"
    "iwp-config.rkt")

;; button actions

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

(define (do-quit-action)
  (do-iwpcli-action IWPCLI_QUIT))

(define (do-start-ssh)
  (do-action-in-terminal (path->string (get-ssh-script-path))))

(define (do-start-sftp)
  (do-generic-action (path->string (get-sftp-script-path))))

(define (do-start-qemu-monitor)
  (do-action-in-terminal (path->string (get-qemu-script-path))))

(define (do-start-edit-config)
  (do-generic-action (path->string (get-edit-config-script-path))))

(define (do-filemanager-action)
  (do-open-url (get-filemanager-url)))

(define (do-phpinfo-action)
  (do-open-url (get-phpinfo-url)))

;; process system event functions

;; build a command string
(define (iwpcli-command-string command)
  ;;(message-box "IWPCLI Path" (path->string (iwpcli-run-path)) #f '(ok no-icon))
  (string-append (path->string (iwpcli-run-path)) " " command))

;; run iwpcli
(define (do-iwpcli-action command)
  (cond
    [(is-windows?) (do-shell-execute command)]
    [(is-macos?)  (system (iwpcli-command-string command))]))

;; generic action func
(define (do-generic-action action-string)
   (cond
    [(is-windows?) (system action-string)]
    [(is-macos?)  (system action-string)]))

;; shell-execute batch file and hide window
(define (do-shell-execute action-string)
  (shell-execute #f (path->string (iwpcli-run-path)) action-string (path->string (iwpcli-bin-dir)) 'SW_HIDE))

;; open a terminal and do an action
(define (do-action-in-terminal action-string)
   (cond
    [(is-windows?) (start-win-terminal action-string)]
    [(is-macos?)  (start-osx-terminal action-string)]))

;; open a url
(define (do-open-url url-string)
   (cond
    [(is-windows?) (system (string-append  "start " url-string))]
    [(is-macos?) (system  (string-append  "open " url-string))]))

(define (start-osx-terminal action-string)
  (system  (string-append  "open -a 'Terminal' " action-string)))

(define (start-win-terminal action-string)
   (system (string-append "start "  action-string)))


(define (should-quit-iwp?)
  (define answer (message-box "Quit InstantWP" "Quit InstantWP?" #f '(yes-no)))
  (cond
    [(symbol=? answer 'yes) #t]
    [(symbol=? answer 'no) #f]))

