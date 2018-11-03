#lang racket/gui

(provide native-menu)

(require logger glfw3 spipe "../pure.rkt")

(define/H~> native-menu
  pop
  trce
  (display-menu ()))

(define (display-menu)
  (glfwTerminate)
  (define the-dialog
    (new dialog% [label "quux"]))
  (new message%
       [label "Es\nwar\neinmal\nvor\nlanger\nZeit"]
       [parent the-dialog])
  (erro "nice")
  (send the-dialog show #t)
  (sleep 30)
  (erro "nice")
  )

(module+ main
  (display-menu))
