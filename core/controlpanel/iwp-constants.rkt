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
(define START_QUIT_GUI_HEIGHT 30)
(define IWP_DIALOG_TITLE "InstantWP Control Panel")
(define CONTROL_PANEL_TAB "Control Panel")
(define ADVANCED_TAB "Advanced")
(define WP_RESOURCES_TAB "WordPress Resources")
(define LOADING_LABEL "Loading...")
(define QUITTING_LABEL "Quitting...")
(define STARTING_IWP_TITLE "Starting InstantWP...")
(define QUITTING_IWP_TITLE "Quitting InstantWP...")
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
(define WEB_FILEMANAGER "FileManager")
(define START_SSH "startSSHScript")
(define START_SFTP "startSFTPScript")
(define START_QEMU "startQEMUMonitorScript")
(define START_EDIT_CONFIG "startEditConfigFileScript")
(define START_TERMINAL "startTerminal")
(define SSH_INFO #<<EOF
This button will open an SSH session to the VM. 
You will be logged in automatically as user 'iwp' with password 'iwp'.
The web root folder is at /var/www/localhost/htdocs/
EOF
)
(define SFTP_INFO #<<EOF
This will open the SFTP client - Fugu SFTP on OSX and WinSCP on Windows.
You can use the following details to login to the VM:
 - The server to connect to: 127.0.0.1
 - The username: root
 - The network port: 10022
 - The password: root
The web root directory is at /var/www/localhost/htdocs/
EOF
)
(define QEMU_MONITOR_INFO #<<EOF
Access to the QEMU monitor console for interacting with the Virtual Machine. 
See the QEMU Monitor documentation for more information at https://en.wikibooks.org/wiki/QEMU/Monitor
EOF
)
(define EDIT_CONFIG_INFO #<<EOF
This will open the InstantWP config file in your default text editor.
EOF
)
(define WEB_FILE_MANAGER_INFO #<<EOF
Login with user 'iwp' and password 'iwp' for normal access.
Login with user 'root' and password 'root' for administrator access.

EOF
)
(define PHP_INFO_INFO "Information about the installed version of PHP.")