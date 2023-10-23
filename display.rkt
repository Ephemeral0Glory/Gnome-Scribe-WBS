#lang racket/gui
(require racket/gui/base)
(require "req.rkt")

(define wbs% (class frame%
               ; Initializations
               (init top-level-req)                           ; The task object, a type of req%

               ; Fields
               (define main-req top-level-req)                ; The requirements structure being displayed

               ; Superclass initialization
               (super-new [label "Gnome Scribe WBS"]
                          [width 600]
                          [height 600])
               
               (define main-panel (new panel% [parent this]   ; The panel in which the work breakdown structure is displayed
                                       [style (list 'auto-vscroll 'auto-hscroll)]
                                       [alignment (list 'left 'top)]))

               ; frame set-icon read-bitmap

               ; Methods
               (define/public (display-wbs)                   ; Displays the main requirement (task)
                 (display-req main-req main-req main-panel))
               
               (define (display-req req parent-req container) ; Creates and displays a requirement in a panel
                 ; Requirement panel (with sub-requirements)
                 (define panel (new vertical-panel% [parent container]
                                    [style (list 'border)]
                                    [border 8]
                                    [alignment (list 'left 'top)]
                                    [stretchable-height #f]))
                 ; Info panel
                 (define req-panel (new horizontal-panel% [parent panel]
                                        [alignment (list 'left 'center)]
                                        [stretchable-height #f]))
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
               
               (define/public (update-wbs)                   ; Updates the work breakdown structure to reflect a change
                 (for ([child (send main-panel get-children)]
                       #:unless (empty? child))
                   (send main-panel delete-child child))
                 (display-wbs))))

; Main method creates a default top-level req% and displays it in the wbs% window
(module+ main
  (define task (new req% [given-name "New Task"]))
  (define frame (new wbs% [top-level-req task]))
  (send frame display-wbs)
  (send frame show #t))
