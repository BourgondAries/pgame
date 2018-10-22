#lang racket/base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plumbing to handle user breaks in the boundary between
;; reloadable and core. Call 'break' to exit the program
;; gracefully.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide break breakpoint break-seen?)

(require logger)

(define (break-seen?) +exn:break-seen?+)

(define +exn:break-seen?+ #f)

;; Mainly plumbing to handle break signals, not very important
(define (breakpoint exn)
  ;; Breaks raised here are propagated to the next continuation
  (if (exn:break? exn)
    (begin
      (set! +exn:break-seen?+ #t)
      ([exn:break-continuation exn]))
    (ftal^ exn))
  (exit 1)
  ) ; From call-with-exception docs: —an exception handler can simply return a result instead of escaping, in which case the raise call propagates the value to the previous exception handler

(define (break state cleanup)
  (with-handlers ([exn:break? (lambda (_) (displayln "") (exit 2))])
    (info* "Cleaning up, re-break to force termination")
    (cleanup state)
    (exit 1)
    #f))
