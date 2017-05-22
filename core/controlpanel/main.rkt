#lang racket

#|
main.rkt --- InstantWP Control Panel 
Copyright (c) 2010-2017 Corvideon Ltd http://www.corvideon.ie
Author: Seamus Brady seamus@corvideon.ie
Homepage: http://www.instantwp.com
License; GPLv3
|#

(require racket/gui
         racket/runtime-path
         yaml
         "start-progress-dialog.rkt"
         "control-panel-dialog.rkt"
         "logger.rkt"
         )

;; setup some defaults to avoid console window
;(current-output-port (open-output-nowhere))

;; config file path
(define iwp-config-file (build-path (current-directory) "config/iwp.yaml"))

;; load the config from the yaml file
(define (load-config) 
  (let* ([iwp-config-port 
          (open-input-file iwp-config-file)]
         [iwp-config (get-config-data iwp-config-port)])
    iwp-config))

;; handle error loading config file
(define (exit-with-config-error)
  (message-box "IWP Config error" "Error loading IWP config file!" #f '(ok no-icon))
  (exit))

;; open config file port
(define (get-config-data port)
  (with-handlers ([exn:fail? (λ (exn) (exit-with-config-error))])
    (call-with-input-file 
        iwp-config-file
      (λ (port)
        (read-yaml port)))))

;; var to hold config hash
(define *iwp-config* (load-config))


(module+ main
  ;; load config
  (display "Starting IWP...\n")
  (iwp-log (path->string (current-directory)))
  (show-main-window)
  )