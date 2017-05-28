#lang racket

#|
quit-progress-dialog.rkt --- InstantWP closing down progress dialog 
Copyright (c) 2010-2017 Corvideon Ltd http://www.corvideon.ie
Author: Seamus Brady seamus@corvideon.ie
Homepage: http://www.instantwp.com
License; GPLv3
|#


(require racket/gui)

;; define root window
(define iwp-window-hash (make-hash))
(hash-set! iwp-window-hash "label" "Starting InstantWP...")
(hash-set! iwp-window-hash "width" 400)
(hash-set! iwp-window-hash "height" 100)

(define root-window (new frame% [label (hash-ref iwp-window-hash "label")]
                             [width (hash-ref iwp-window-hash "width")]
                             [height (hash-ref iwp-window-hash "height")]
                             [style '(no-resize-border)]))

;; define panels
(define main-panel (new panel% (parent root-window)))
(define logo (read-bitmap "/Users/seamus/GitHub/InstantWP/core/images/logo.gif"))
(define logo-label (new message% (parent main-panel) (label logo)))

(define progress-textbox
  (new text-field% [label ""] [parent root-window]
       [style '(single)]
       [min-height 10]
       [min-width 350]
       [stretchable-width 350]
       [vert-margin 0]
)) ;; add field to frame

(define start-button (new button% [parent root-window]
     [label "Start"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (display "Frontpage"))]))


(send root-window show #t)
(send start-button enable #f)
(define textedit (send progress-textbox get-editor))

(for ([i (in-range 20)]) ; iterator binding
  (send textedit erase)
  (send textedit insert (string-append "Loading " (number->string i) "%"))
  (sleep 1)) ; body

(send start-button enable #t)
