#lang scene ;; Includes everything, looks for init, pre, pure, etc...
;; Parses it into a set of functions, loops, does everything...
;; Probably the best way to do FSM-based systems
;; Sending msgs to other threads? The system SHOULD have a list of
;; threads, ALWAYS!

(init
  )

(pre
  )

(pure
  )

(post
  (HSM  ;; Hierarchical exit conditions, the first one triggered is used
    ()))

(exit
  )

;; Helper functions here for quick development...
(define ...)
