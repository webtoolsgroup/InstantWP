#lang racket


;; This module implements IWP constants
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; list of constants declared
(provide (all-defined-out))

;; —————————————————————————————————
;; import and implementation section



;; ---------------------------------------
;; some local func to test OS
;; stops a circular dependency having them here
;; ---------------------------------------

;; are we on macos?
(define (is-macos-constant?)
  (equal? (system-type) 'macosx))

;; are we on windows?
(define (is-windows-constant?)
  (equal? (system-type) 'windows))


;; ---------------------------------------
;; GUI constants
;; some OS dependent
;; ---------------------------------------

(define MAIN_GUI_WIDTH 750)
(define MAIN_GUI_HEIGHT 500)
(define START_QUIT_GUI_WIDTH 500)
(define START_QUIT_GUI_HEIGHT 50)
(define MAIN_BTN_WIDTH
  (cond
    [(is-windows-constant?) 225]
    [(is-macos-constant?)  225 ]))
(define MAIN_BTN_HEIGHT
  (cond
    [(is-windows-constant?) 45]
    [(is-macos-constant?)  40 ]))
(define MAIN_LBL_WIDTH
    (cond
    [(is-windows-constant?) 400]
    [(is-macos-constant?)  400 ]))
(define MAIN_LBL_HEIGHT
  (cond
    [(is-windows-constant?) 5]
    [(is-macos-constant?)  10 ]))
(define ADV_BTN_WIDTH
  (cond
    [(is-windows-constant?) 225]
    [(is-macos-constant?)  200 ]))
(define ADV_BTN_HEIGHT
  (cond
    [(is-windows-constant?) 45]
    [(is-macos-constant?)  40 ]))
(define ADV_LBL_WIDTH
  (cond
    [(is-windows-constant?) 400]
    [(is-macos-constant?)  400 ]))
(define ADV_LBL_HEIGHT
  (cond
    [(is-windows-constant?) 10]
    [(is-macos-constant?)  10 ]))
(define LBL_ENABLED
  (cond
    [(is-windows-constant?) #t]
    [(is-macos-constant?)  #t]))
(define LEFT_PADDING
  (cond
    [(is-windows-constant?) "    "]
    [(is-macos-constant?)  ""]))
(define TOP_PADDING
  (cond
    [(is-windows-constant?) 10]
    [(is-macos-constant?)  5]))
(define WP_RESOURCES_BTN_WIDTH
  (cond
    [(is-windows-constant?) 350]
    [(is-macos-constant?)  350]))
(define WP_RESOURCES_BTN_HEIGHT
  (cond
    [(is-windows-constant?) 20]
    [(is-macos-constant?)  20]))
(define WP_RESOURCES_BTN_STRETCH_HEIGHT
  (cond
    [(is-windows-constant?) #f]
    [(is-macos-constant?)  50]))
;; ---------------------------------------
;; Button/dialog captions
;; ---------------------------------------

(define IWP_DIALOG_TITLE "InstantWP Control Panel")
(define CONTROL_PANEL_TAB "Control Panel")
(define ADVANCED_TAB "Advanced")
(define WP_RESOURCES_TAB "Products")
(define LOADING_LABEL "Loading...")
(define QUITTING_LABEL "Quitting...")
(define STARTING_IWP_TITLE "Starting InstantWP - this should take less than 60 seconds...")
(define QUITTING_IWP_TITLE "Quitting InstantWP - this should take less than 30 seconds...")

;; ---------------------------------------
;; System config constants
;; ---------------------------------------

(define RUN_IWPCLI_WIN "bin\\run-iwpcli.bat")
(define RUN_IWPCLI_MAC "bin/run-iwpcli")
(define IWPCLI_WIN "iwpcli.exe")
(define IWPCLI_MAC "iwpcli")
(define IWP_CONFIG_DIR "config")
(define IWP_CONFIG_FILE_WIN "iwp-win.ini")
(define IWP_CONFIG_FILE_MAC "iwp-osx.ini")
(define IWPCLI_STATUS "status")
(define IWPCLI_START "start")
(define IWPCLI_QUIT "quit")
(define IWPCLI_WPFRONTPAGE "wpfrontpage")
(define IWPCLI_WPADMIN "wpadmin")
(define IWPCLI_PLUGINS "plugins")
(define IWPCLI_THEMES "themes")
(define IWPCLI_MYSQL "mysql")
(define IWPCLI_DOCS "docs")
(define IWPCLI_ABOUT "about")
(define IWPCLI_SSH "ssh")
(define IWPCLI_MONITOR "monitor")
(define IWPCLI_WEBCONSOLE "webconsole")
(define PORTOFFSET "PortOffset")
(define HTTP "HTTP")
(define LOCALHOST "127.0.0.1")
(define PHP_INFO "PHPInfo")
(define WEB_FILEMANAGER "FileManager")
(define START_SSH "startSSHScript")
(define START_SFTP "startSFTPScript")
(define START_QEMU "startQEMUMonitorScript")
(define START_EDIT_CONFIG "startEditConfigFileScript")
(define START_TERMINAL "startTerminal")
(define QUIT_SCRIPT "quitIWPScript")
(define OPEN_URL "openURL")
(define HIDE_README "HideReadMeFile")
(define WEBSERVER_WARN "WebServerWarnMessage")
(define SHOW_DEPLOY "showDeployInfo")


;; ---------------------------------------
;; Help label constants
;; ---------------------------------------

(define WEB_SERVER_MSG "As this is the first time the web server has been accessed, it may take a few moments to initialise fully. Subsequent page loading will be significantly quicker.")

(define WPFRONTPAGE_INFO 
  (cond
    [(is-windows-constant?) "Opens the WordPress website.\n"]
    [(is-macos-constant?)  "Opens the WordPress website." ]))
(define WPADMIN_INFO "Opens the WordPress Dashboard.\nYou can login with 'admin' and 'password'.")
(define WPTHEMES_INFO
  (cond
    [(is-windows-constant?)  "Opens the WordPress wp-themes directory.\n"]
    [(is-macos-constant?)   "Opens the WordPress wp-themes directory." ]))
(define WPPLUGINS_INFO 
  (cond
    [(is-windows-constant?)  "Opens the WordPress wp-plugins directory.\n"]
    [(is-macos-constant?)   "Opens the WordPress wp-plugins directory." ]))
(define MYSQL_INFO "Opens PHPMyAdmin.\nYou can login with 'root' with a blank password.")
(define DOCS_INFO  
  (cond
    [(is-windows-constant?)  "Opens the InstantWP documentation.\n"]
    [(is-macos-constant?)  "Opens the InstantWP documentation." ]))
(define SSH_INFO "Open an SSH session as user 'iwp' with password 'iwp'.\nThe web root folder is at /var/www/localhost/htdocs/")
(define SFTP_INFO "Opens the SFTP client. Use the following details - \nHost: 127.0.0.1, Port: 10022, Username: 'root', Password: 'root'.")
(define QEMU_MONITOR_INFO 
  (cond
    [(is-windows-constant?)  "Open the QEMU Monitor console for interacting with the VM.\n"]
    [(is-macos-constant?)  "Open the QEMU Monitor console for interacting with the VM." ]))
(define EDIT_CONFIG_INFO 
  (cond
    [(is-windows-constant?) "This will open the InstantWP config file in your default text editor.\n"]
    [(is-macos-constant?) "This will open the InstantWP config file in your default text editor." ]))
(define WEB_CONSOLE_INFO "Opens the InstantWP Web Console.\nLogin with user 'iwp' and password 'iwp'.")
(define PHP_INFO_INFO 
  (cond
    [(is-windows-constant?) "Information about the installed version of PHP.\n"]
    [(is-macos-constant?) "Information about the installed version of PHP." ]))