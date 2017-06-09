#lang racket


;; This module implements IWP constants
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

;; list of constants declared
(provide (all-defined-out))

;; —————————————————————————————————
;; import and implementation section

(define MAIN_GUI_WIDTH 750)
(define MAIN_GUI_HEIGHT 450)
(define START_QUIT_GUI_WIDTH 400)
(define START_QUIT_GUI_HEIGHT 100)
(define IWP_DIALOG_TITLE "InstantWP Control Panel")
(define CONTROL_PANEL_TAB "Control Panel")
(define ADVANCED_TAB "Advanced")
(define WP_RESOURCES_TAB "WordPress Resources")
(define STARTING_IWP "Starting InstantWP...")
(define RUN_IWPCLI_WIN "bin/run-iwpcli.bat")
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
(define PORTOFFSET "PortOffset")
(define HTTP "HTTP")
(define LOCALHOST "127.0.0.1")
(define PHP_INFO "phpinfo.php")