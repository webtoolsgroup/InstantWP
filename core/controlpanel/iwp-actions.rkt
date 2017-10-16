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
 ;; start web console
 do-webconsole-action
 ;; start php info page
 do-phpinfo-action
 ;; start edit config
 do-start-edit-config
 ;; open wp resource
do-wp-resources-action
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
  (web-server-warning)
  (do-iwpcli-action IWPCLI_WPFRONTPAGE))

(define (do-wpadmin-action)
  (web-server-warning)
  (do-iwpcli-action IWPCLI_WPADMIN))

(define (do-plugins-action)
  (web-server-warning)
  (do-iwpcli-action IWPCLI_PLUGINS))

(define (do-themes-action)
  (web-server-warning)
  (do-iwpcli-action IWPCLI_THEMES))

(define (do-mysql-action)
  (web-server-warning)
  (do-iwpcli-action IWPCLI_MYSQL))

(define (do-docs-action)
  (do-iwpcli-action IWPCLI_DOCS))

(define (do-about-action)
  (do-iwpcli-action IWPCLI_ABOUT))

(define (do-webconsole-action)
  (web-server-warning)
  (do-iwpcli-action IWPCLI_WEBCONSOLE))

(define (do-quit-action)
  (do-iwpcli-action IWPCLI_QUIT))

(define (do-start-ssh)
  (cond
    [(is-windows?)
     ;; yes, windows makes everything difficult again...
     (run-win-script-exe (path->string (get-ssh-script-path)))]
    [(is-macos?)  (do-action-in-terminal (path->string (get-ssh-script-path)))]))

;; this needed a custom script to avoid console box
(define (do-start-sftp)
   (cond
    [(is-windows?)
     ;; yes, windows makes everything difficult again...
     (run-win-script-exe (path->string (get-sftp-script-path)))]
    [(is-macos?)  (system (path->string (get-sftp-script-path)))]))

(define (do-start-qemu-monitor)
  (cond
    [(is-windows?)
     ;; yes, windows makes everything difficult again...
     (run-win-script-exe (path->string (get-qemu-script-path)))]
    [(is-macos?)  (do-action-in-terminal (path->string (get-qemu-script-path)))]))


(define (do-start-edit-config)
  (void  (cond
           [(is-windows?) (run-win-script-exe (path->string (get-edit-config-script-path)))]
           [(is-macos?)   (do-generic-action (path->string (get-edit-config-script-path)))])))

(define (do-phpinfo-action)
  (do-open-url (get-phpinfo-url)))

(define (do-wp-resources-action)
  (do-open-url (get-wp-resources-url)))

;; process system event functions

;; build a command string
(define (iwpcli-command-string command)
  ;;(message-box "IWPCLI Path" (path->string (iwpcli-run-path)) #f '(ok no-icon))
  (string-append (path->string (iwpcli-run-path)) " " command))

;; run iwpcli
(define (do-iwpcli-action command)
  (cond
    [(is-windows?) (do-shell-execute-for-iwpcli command)]
    [(is-macos?)  (system (iwpcli-command-string command))]))

;; don't ask! this stops stoopid windows security popups
(define (run-win-script-exe command)
  (define SCRIPT_ROOT (string-append (path->string (iwp-platform-dir-path)) (get-config-setting "scriptRootDir")))
  (shell-execute #f
                 command
                 ""
                 SCRIPT_ROOT
                 'SW_SHOWNORMAL ))

;; generic action func
(define (do-generic-action action-string)
   (cond
    [(is-windows?) (system action-string)]
    [(is-macos?)  (system action-string)]))

;; shell-execute batch file and hide window
(define (do-shell-execute-for-iwpcli action-string)
  (void
   (shell-execute #f
                  (path->string (iwpcli-run-path))
                  action-string
                  (path->string (iwpcli-bin-dir))
                  'SW_HIDE)))

;; shell-execute batch file and hide window
(define (do-generic-shell-execute command)
  (void (shell-execute #f
                       command
                       ""
                       (path->string (iwpcli-bin-dir))
                       'SW_HIDE)))


;; open a terminal and do an action
(define (do-action-in-terminal action-string)
   (cond
    [(is-windows?) (start-win-terminal action-string)]
    [(is-macos?)  (start-osx-terminal action-string)]))

;; open a url
(define (do-open-url url-string)
   (cond
    [(is-windows?) (do-generic-shell-execute url-string)]
    [(is-macos?) (system  (string-append  "open " url-string))]))

;; start Terminal
(define (start-osx-terminal action-string)
  (system  (string-append  "open -a 'Terminal' " action-string)))

;; just do a shell-ex
(define (start-win-terminal action-string)
   (do-generic-shell-execute action-string))

(define (should-quit-iwp?)
  (define answer (message-box "Quit InstantWP" "Quit InstantWP?" #f '(yes-no)))
  (cond
    [(symbol=? answer 'yes) #t]
    [(symbol=? answer 'no) #f]))

;; --------------------------------------
;; ;; web server warning funcs
;; --------------------------------------

(define (web-server-warning)
  (cond
    [(not (web-server-warning-file-exists?))
     (do-web-server-warning-action)]))

(define (do-web-server-warning-action)
  (web-server-warning-msg)
  (create-web-server-warning-file)
  ;; sleep for 2 so the db can connect
  (sleep 2))

(define (web-server-warning-msg)
  (message-box "IWP Web Server Startup" WEB_SERVER_MSG #f '(ok no-icon)))

(define (web-server-warning-file-path)
  (build-path (iwp-platform-dir-path) (get-config-setting WEBSERVER_WARN)))

(define (create-web-server-warning-file)
  (define out (open-output-file (web-server-warning-file-path)))
  (write "Message shown." out)
  (close-output-port out))

(define (web-server-warning-file-exists?)
  (file-exists? (web-server-warning-file-path)))
