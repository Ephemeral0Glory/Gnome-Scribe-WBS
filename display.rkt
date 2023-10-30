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
                 (define high-panel (new horizontal-panel% [parent container]
                                         [stretchable-height #f]))
                 (unless (eq? container main-panel) (new panel% [parent high-panel] ; Spacer panel offsets sub-reqs
                                                         [min-width 19]
                                                         [stretchable-width #f]))
                 (define panel (new collapse-vert-panel% [parent high-panel]
                                    [alignment (list 'left 'center)]
                                    [stretchable-height #f]))
                 ; Requirement data panel
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
                      [callback (lambda (button event) ; Change the req status, update its color, sort it
                                  (send req toggle-status)
                                  (send (third (send req-panel get-children))
                                        set-field-background
                                        (if (send req get-status)
                                            (make-color 150 250 160) ; Green for complete
                                            (make-color 255 175 175))) ; Red for incomplete
                                  (send parent-req sort-subreqs)
                                  (send container change-children
                                        (lambda (children)
                                          (sort-panels children)))
                                  (update-wbs))])
                 (define name (new text-field% [parent req-panel]
                                   [label "Name:"]
                                   [init-value (send req get-name)]
                                   [callback (lambda (field event)
                                               (send req update-name (send field get-value)))]))
                 (send name set-field-background
                       (if (send req get-status)
                           (make-color 150 250 160) ; Green for complete
                           (make-color 255 175 175))) ; Red for incomplete
                 (new button% [parent req-panel]
                      [label "Add Sub-Req"]
                      [callback (lambda (button event)
                                  (send req add-new-subreq)
                                  (display-req (last (send req get-subreqs)) req panel)
                                  (update-wbs))])
                 (new button% [parent req-panel]
                      [label "Delete"]
                      [callback (lambda (button event)
                                  (send parent-req remove-subreq req)
                                  (send container delete-child high-panel)
                                  (update-wbs))])

                 ; Display subrequirements
                 (for ([subreq (send req get-subreqs)]
                       #:unless (empty? req))
                   (display-req subreq req panel)))

               (define (get-checkbox high-panel)                   ; Retrieves the check-box widget from a high-panel
                 (second (send (first (send (second (send high-panel get-children)) get-children)) get-children)))

               (define (sort-panels panels)                   ; Sorts the given subreq panels in the same way as the subreqs
                 (append (list (first panels)) ; First panel is the horizontal req-data panel
                         (sort (rest panels) ; Sub-requirements' high-panels
                               (lambda (first second)
                                 (let ([complete1? (send (get-checkbox first) get-value)]
                                       [complete2? (send (get-checkbox second) get-value)])
                                   (if (and complete1? (not complete2?))
                                       #f
                                       (if (and complete2? (not complete1?))
                                           #t
                                           (and complete1? complete2?))))))))
               
               (define/public (update-wbs)                    ; Updates the work breakdown structure to reflect a change
                 (send main-panel refresh))

               (define (set-up-display)                       ; Sets up the toolbar and sets the frame icon
                 ; Frame icon
                 (send this set-icon (read-bitmap (string->path "./assets/collapseArrow.jpg") 'jpeg))
                 ; Toolbar panel
                 (define panel (new horizontal-panel% [parent this]
                                    [style (list 'border)]
                                    [border 4]
                                    [alignment (list 'left 'top)]
                                    [stretchable-height #f]))
                 (new button% [parent panel]
                      [label "New"]
                      [callback (lambda (button event)
                                  (set! main-req (new req% [given-name "New Project"]))
                                  (reset-wbs))])
                 (new button% [parent panel]
                      [label "Open"]
                      [callback (lambda (button event)
                                  (let ([file-path (get-file "Choose a file to open" ; Message
                                                             this                    ; Parent
                                                             #f #f                   ; Directory and filename
                                                             ".gss"                  ; Extension
                                                             null                    ; Style
                                                             '(("Gnome Scribe Structure" "*.gss")))]) ; Filters
                                    (unless (boolean? file-path)
                                      (set! main-req (open-wbs file-path))
                                      (reset-wbs))))])
                 (new button% [parent panel]
                      [label "Save"]
                      [callback (lambda (button event)
                                  (let ([file-path (put-file "Save structure as" ; Message
                                                             this #f             ; Parent and directory
                                                             (string-downcase (send main-req get-name)) ; Filename
                                                             ".gss"              ; Extension
                                                             null                ; Style
                                                             '(("Gnome Scribe Structure" "*.gss")))])  ; Filters
                                    (unless (boolean? file-path)
                                      (save-wbs file-path main-req))))]))

               (define (reset-wbs)                                    ; Displays current WBS after clearing main-panel
                 ; Clear main-panel
                 (send this delete-child main-panel)
                 (set! main-panel (new panel% [parent this]
                                       [style (list 'vscroll 'auto-hscroll)]
                                       [alignment (list 'left 'top)]))
                 ; Redisplay
                 (display-wbs))

               ; Fields (continued) and construction
               (set-up-display) ; Set up toolbar
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
