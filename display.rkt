#lang racket/gui
(require racket/gui/base)
(require "req.rkt")
(require "files.rkt")

; The Gnome Scribe window for Work Breakdown Structures.
; Maintains the current WBS, with a toolbar to allow for file operations.
(define wbs% (class frame%
               ; Initializations
               (init top-level-req)                           ; The task object, a type of req%

               ; Fields
               (define main-req top-level-req)                ; The requirements structure being displayed

               ; Superclass initialization
               (super-new [label "Gnome Scribe WBS"]
                          [width 600]
                          [height 600]
                          [x 100]
                          [y 60])

               ; Methods
               (define/public (display-wbs)                   ; Displays the main requirement (task)
                 (display-req main-req main-req main-panel))
               
               (define (display-req req parent-req container) ; Creates and displays a requirement in a panel
                 ; Requirement panel (with sub-requirements)
                 (define panel (new collapse-vert-panel% [parent container]
                                    [vert-margin 2]
                                    [horiz-margin 6]
                                    [alignment (list 'left 'center)]
                                    [stretchable-height #f]))
                 ; Info panel
                 (define req-panel (new horizontal-panel% [parent panel]
                                        [alignment (list 'left 'center)]
                                        [stretchable-height #f]))
                 (new toggle-button% [parent req-panel]
                      [label-on (read-bitmap (string->path "./assets/expandArrow.jpg")
                                             'jpeg)]
                      [label-off (read-bitmap (string->path "./assets/collapseArrow.jpg")
                                              'jpeg)]
                      [start-position #t]
                      [callback-on (lambda ()
                                     (send panel expand-children))]
                      [callback-off (lambda ()
                                      (send panel collapse-children))])
                 (new check-box% [parent req-panel]
                      [label "Complete?"]
                      [value (send req get-status)]
                      [callback (lambda (button event)
                                  (send req toggle-status))])
                 (new text-field% [parent req-panel]
                      [label "Name:"]
                      [init-value (send req get-name)]
                      [callback (lambda (field event)
                                  (send req update-name (send field get-value)))])
                 (new button% [parent req-panel]
                      [label "Add Sub-Req"]
                      [callback (lambda (button event)
                                  (send req add-new-subreq)
                                  (update-wbs))])
                 (new button% [parent req-panel]
                      [label "Delete"]
                      [callback (lambda (button event)
                                  (send parent-req remove-subreq req)
                                  (update-wbs))])

                 ; Subrequirements
                 (for ([subreq (send req get-subreqs)]
                       #:unless (empty? req))
                   (display-req subreq req panel)))

               (define (collapse-reqs panel)
                 (for ([child (rest (send panel get-children))])
                   (send child show #f)))

               (define (expand-reqs panel)
                 (for ([child (rest (send panel get-children))])
                   (send child show #t)))
               
               (define/public (update-wbs)                    ; Updates the work breakdown structure to reflect a change
                 (for ([child (send main-panel get-children)]
                       #:unless (empty? child))
                   (send main-panel delete-child child))
                 (display-wbs))

               (define (set-up-display)                       ; Sets up the toolbar and sets the frame icon
                 ; TODO frame set-icon read-bitmap
                 (define panel (new horizontal-panel% [parent this]
                                    [style (list 'border)]
                                    [border 4]
                                    [alignment (list 'left 'top)]
                                    [stretchable-height #f]))
                 (new button% [parent panel]
                      [label "New"]
                      [callback (lambda (button event)
                                  (set! main-req (new req% [given-name "New Project"]))
                                  (update-wbs))])
                 (new button% [parent panel]
                      [label "Open"]
                      [callback (lambda (button event)
                                  (let ([file-path (get-file)])
                                    (unless (boolean? file-path)
                                      (set! main-req (open-wbs file-path))
                                      (update-wbs))))])
                 (new button% [parent panel]
                      [label "Save"]
                      [callback (lambda (button event)
                                  (let ([file-path (put-file)])
                                    (unless (boolean? file-path)
                                      (save-wbs file-path main-req))))]))

               ; Fields (continued) and construction
               (set-up-display)
               (define main-panel (new panel% [parent this]   ; The panel in which the work breakdown structure is displayed
                                       [style (list 'vscroll 'auto-hscroll)]
                                       [alignment (list 'left 'top)]))))

; A toggle button for the collapse-expand feature
(define toggle-button% (class button%
                         ; Initial arguments
                         (init [label-on "On"])     ; The display when the button is toggled
                         (init [label-off "Off"])   ; The display when the button is not toggled
                         (init [start-position #f]) ; The initial status, #t -> toggled, #f -> not toggled
                         (init [callback-on #f])    ; The callback when the button is toggled (#f -> #t)
                         (init [callback-off #f])   ; The callback when the button is untoggled (#t -> #f)

                         ; Fields
                         (define on-label label-on)         ; The string or image for when status is #t
                         (define off-label label-off)       ; The string or image for when status is #f
                         (define status start-position)     ; The toggle status of the button
                         (define on-callback callback-on)   ; The function to call when the button is toggled
                         (define off-callback callback-off) ; The function to call when the button is untoggled

                         ; Super-class initialization
                         (super-new [label (if start-position label-on label-off)]
                                    [callback (lambda (button event)
                                                (send button toggle-button))])
                         (inherit set-label)

                         ; Methods
                         (define/public (toggle-button) ; Switches the button between on and off, calling the associated callback
                           (if status
                               (untoggle)
                               (toggle)))
                         (define (untoggle)             ; Switches the button from on to off, uses the off callback
                           (set! status (not status))
                           (set-label off-label)
                           (unless (boolean? off-callback)
                             (off-callback)))
                         (define (toggle)               ; Switches the button from off to on, uses the on callback
                           (set! status (not status))
                           (set-label on-label)
                           (unless (boolean? on-callback)
                             (on-callback)))))

; A vertical panel that can collapse and expand its children
(define collapse-vert-panel% (class vertical-panel%
                               ; Fields
                               (define collapsed-children empty) ; Holder list for children that are collapsed

                               ; Super-class initialization
                               (super-new)
                               (inherit get-children delete-child add-child)

                               ; Methods
                               (define/public (collapse-children) ; Hide all children besides the first one (which has the req info)
                                 (for ([child (rest (get-children))])
                                   (delete-child child)
                                   (set! collapsed-children
                                         (append collapsed-children (list child)))))
                               (define/public (expand-children)   ; Unhide all of the children hidden by collapse-children
                                 (for ([child collapsed-children])
                                   (add-child child))
                                 (set! collapsed-children empty))))

; Main method creates a default top-level req% and displays it in the wbs% window
(module+ main
  (unless (directory-exists? (string->path "./structures"))
    (make-directory (string->path "./structures")))
  (define task (new req% [given-name "New Project"]))
  (define frame (new wbs% [top-level-req task]))
  (send frame display-wbs)
  (send frame show #t))
