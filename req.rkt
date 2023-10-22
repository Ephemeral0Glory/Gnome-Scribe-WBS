#lang racket

(provide req%)

; A requirement for the task completion.
; The overall task is also a requirement.
(define req% (class object%
               ; Initializations
               (init [given-name "New Requirement"]) ; The name to initially use
               (init [completion-status #f])         ; True if the requirement is complete
               (init [given-subreqs empty])        ; The list of subrequirements, if any
               
               ; Fields
               (define name given-name)              ; The current name of the requirement
               (define completed? completion-status) ; The current completion status of the requirement
               (define subreqs given-subreqs)        ; The list of current subrequirements

               ; Superclass initialization
               (super-new)

               ; Methods
               (define/public (get-name)
                 name)                               ; Returns the current name of the requirement
               (define/public (get-status)
                 completed?)                         ; Returns the completion status of the requirement
               (define/public (get-subreqs)
                 subreqs)                            ; Returns the list of subrequirements
               (define/public (toggle-status)
                 (set! completed? (not completed?))) ; Toggles the completion status between true and false
               (define/public (update-name new-name)
                 (set! name new-name))               ; Changes the name of this requirement to the new one
               (define/public (add-new-subreq)
                 (set! subreqs (append subreqs (list (new req%))))) ; Adds a new subrequirement to the subrequirements list
               (define/public (remove-subreq req-to-remove)
                 (set! subreqs (remove req-to-remove subreqs))))) ; Removes the first instance of the given requirement