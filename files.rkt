#lang racket
(require "req.rkt")

(provide open-wbs save-wbs)

; This file contains all functions for reading and writing files for the
; Gnome Scribe program.
(define (open-wbs file-name)        ; Opens the given file and creates a WBS object from the contents
  (when (path? file-name)
    (create-req (read-wbs-file file-name))))

(define (create-req data-list)      ; Creates a WBS object from the given list of data
  (unless (empty? (first data-list))
    (new req% [given-name (first data-list)]
         [completion-status (first (rest data-list))]
         [given-subreqs (create-req-list (rest (rest data-list)))])))

(define (create-req-list data-list) ; Creates a list of WBS objects from the given list of data
  (if (or (empty? data-list)
          (empty? (first data-list)))
      empty
      (for/list ([req-data (first data-list)]) ; (first) allows for proper iteration
        (create-req req-data))))

(define (read-wbs-file file-path)   ; Creates a list of data from a file
  (define input (open-input-file file-path #:mode 'text))
  (define data (read input))
  (close-input-port input)
  data)

(define (save-wbs file-path wbs)              ; Saves the given WBS req% object as a list of lists
  (define output (open-output-file file-path
                                   #:mode 'text
                                   #:exists 'replace))
  (write (listify-wbs wbs) output)
  (close-output-port output))

(define (listify-wbs req)                     ; Creates a list of lists representation of a req% object
  (list (send req get-name)
        (send req get-status)
        (let ([subreqs (send req get-subreqs)])
          (if (empty? subreqs)
              empty
              (for/list ([subreq subreqs])
                (listify-wbs subreq))))))

; Testing definitions
(define test-data (list "Example Task" #f (list
                                       (list "req1" #f (list
                                                        (list "req1a" #t empty)
                                                        (list "req1b" #f empty)))
                                       (list "req2" #t (list
                                                        (list "req2a" #t (list
                                                                          (list "req2aI" #t empty)
                                                                          (list "req2aII" #t empty)))
                                                        (list "req2b" #t empty)))
                                       (list "req3" #f (list
                                                        (list "req3a" #f (list
                                                                          (list "req3aI" #t empty)
                                                                          (list "req3aII" #f empty)
                                                                          (list "req3aIII" #f (list
                                                                                               (list "req3aIII1" #f empty)
                                                                                               (list "req3aIII2" #t empty)))))
                                                        (list "req3b" #t empty))))))
(define test (create-req test-data))