#lang racket

(provide req%)

; A requirement for the task completion.
; The overall task is also a requirement.
(define req% (class object%
               ; Initializations
               (init [given-name "New Requirement"]) ; The name to initially use
               (init [completion-status #f])         ; True if the requirement is complete
               (init [given-subreqs empty])          ; The list of subrequirements, if any
               
               ; Fields
               (define name given-name)              ; The current name of the requirement
               (define completed? completion-status) ; The current completion status of the requirement
               (define subreqs given-subreqs)        ; The list of current subrequirements

               ; Superclass initialization
               (super-new)

               ; Methods
               (define/public (get-name)                      ; Returns the current name of the requirement
                 name)
               (define/public (get-status)                    ; Returns the completion status of the requirement
                 completed?)
               (define/public (get-subreqs)                   ; Returns the list of subrequirements
                 subreqs)
               (define/public (toggle-status)                 ; Toggles the completion status between true and false
                 (set! completed? (not completed?)))
               (define/public (update-name new-name)          ; Changes the name of this requirement to the new one
                 (set! name new-name))
               (define/public (add-new-subreq)                ; Adds a new subrequirement to the subrequirements list
                 (set! subreqs (append subreqs (list (new req%)))))
               (define/public (remove-subreq req-to-remove)   ; Removes the first instance of the given requirement
                 (set! subreqs (remove req-to-remove subreqs)))))