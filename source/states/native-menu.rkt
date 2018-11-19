#lang racket/gui

(provide native-menu)

(require pgame/source/all pgame/utils)

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
