#lang racket

;; This module implements IWP readme window
;; License GPLv3
;; (c) 2010-2017 Corvideon Ltd

(provide
 ;; show the readme gui
 show-readme-window
 ;; hide the readme gui
 hide-readme-window)


;; —————————————————————————————————
;; import and implementation section

(require
  racket/gui/base
  "iwp-main-dialog.rkt"
  "iwp-config.rkt"
  "iwp-constants.rkt"
  "iwp-resources.rkt"
  "iwp-environment.rkt"
  "iwp-actions.rkt")

;; define root window value hash
(define iwp-window-hash (make-hash))

;; set the root window hash values
(hash-set! iwp-window-hash "label" "Welcome to InstantWP!")
(hash-set! iwp-window-hash "width" MAIN_GUI_WIDTH)
(hash-set! iwp-window-hash "height" 500)

;; custom frame with exit - main window
(define root-window (new (class frame% (super-new)
                         (define/augment (on-close)
                           (do-exit)))
                       [label (hash-ref iwp-window-hash "label")]
                       [width (hash-ref iwp-window-hash "width")]
                       [height (hash-ref iwp-window-hash "height")]
                       [style '(no-resize-border)]))

;; logo at the top of dialog
(define logo (iwp-logo))
(define logo-label (new message% (parent root-window) (label logo)))

;; define panels
(define main-panel (new panel% (parent root-window)))
(define title-panel (new horizontal-pane% (parent root-window) [alignment '(left top)]))
(define readme-panel (new horizontal-pane% (parent root-window) [alignment '(left top)]))

;; this panel holds the prev/next buttons at the bottom of dialog
(define prev-next-buttons-panel (new horizontal-pane% (parent root-window) [alignment '(right bottom)] [stretchable-height #f]))

;; this panel holds the buttons at the bottom of dialog
(define base-buttons-panel (new horizontal-pane% (parent root-window) [alignment '(right bottom)] [stretchable-height #f]))

;; --------------------------------------
;; button and label creation functions
;; --------------------------------------

(define (create-new-button parent bitmap label width height callback)
  (new button%
     [parent parent]
     [label label]
     [min-width width]
     [min-height height]
     [callback (lambda (button event)
                       (callback))]))


(define (create-new-label label width height parent)
  (new message%
     [label label]
     [enabled LBL_ENABLED]
     [min-width width]
     [min-height height]
     [parent parent]
     [stretchable-height #f]))


;; --------------------------------------
;; main window control functions
;; --------------------------------------

;; Show the frame by calling its show method
(define (show-readme-window)
  ;; sleep for 1 so the db can connect
  (sleep 1)
  (cond
    [(hide-readme-file-exists?) (show-main-window)]
    [(not (hide-readme-file-exists?)) (show-readme-window-private)]))

(define (show-readme-window-private)
  (send root-window show #t)
  (send root-window center))

;; Hide the frame by calling its show method
(define (hide-readme-window)
  (send root-window show #f))

(define (do-exit)
  (void))

(define (close-action)
  (show-main-window)
  (hide-readme-window))

(define (close-dont-show-action)
  (create-hide-readme-file)
  (close-action))

;; --------------------------------------
;; file functions
;; --------------------------------------

;; HideReadme file path
(define (hide-readme-file-path)
  (build-path (iwp-platform-dir-path) (get-config-setting HIDE_README)))

(define (create-hide-readme-file)
  (define out (open-output-file (hide-readme-file-path)))
  (write "hide readme!" out)
  (close-output-port out))

(define (hide-readme-file-exists?)
  (file-exists? (hide-readme-file-path)))


;; --------------------------------------
;;setup controls
;; --------------------------------------

;; total number of screens for wizard
(define readme-limit 2)

;; current screen
(define readme-counter 1)

(define (next-readme-counter)
  (cond
    [(= readme-limit readme-counter) (enable-close-buttons) ]
    [else (inc-readme-counter)]))

(define (prev-readme-counter)
  (cond
    [(= readme-counter 1) (void) ]
    [else (dec-readme-counter)]))

(define (dec-readme-counter)
  (set! readme-counter (sub1 readme-counter)))

(define (inc-readme-counter)
  (set! readme-counter (add1 readme-counter))
    (cond
    [(= readme-limit readme-counter) (enable-close-buttons) ]))

(define (enable-close-buttons)
  (send close-button enable #t)
  (send close-dont-show-button enable #t))

(define (get-readme-bitmap)
  (read-bitmap  (build-path (iwp-images-dir-path) (string-append "readme" (number->string readme-counter) ".png"))))

(define readme-message
  (create-new-label (readme-bitmap)  MAIN_LBL_WIDTH 10 title-panel))

(define (next-action)
  (next-readme-counter)
  (send readme-message set-label (get-readme-bitmap)))

(define (prev-action)
  (prev-readme-counter)
  (send readme-message set-label (get-readme-bitmap)))

(define prev-button
  (create-new-button prev-next-buttons-panel (about-bitmap) "Prev"
                     100 30
                     prev-action))
(define next-button
  (create-new-button prev-next-buttons-panel (about-bitmap) "Next"
                     100 30
                     next-action))

(define close-button
  (create-new-button base-buttons-panel (about-bitmap) "Close"
                     100 30
                     close-action))

(define close-dont-show-button
  (create-new-button base-buttons-panel (quit-bitmap) "Close and Don't Show Again"
                     100 30
                     close-dont-show-action))

;; turn off close buttons
(send close-button enable #f)
(send close-dont-show-button enable #f)
