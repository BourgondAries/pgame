#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup and exit procedure
;;
;; The cleanup and exit procedure allows a clean exit of
;; the game. It should destroy all resources that would
;; otherwise be collected by the operating system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide shutdown)

(require "../all.rkt")

(define/H~> shutdown
  (glfwDestroyWindow  io.window)
  (glfwTerminate      ())
  )
