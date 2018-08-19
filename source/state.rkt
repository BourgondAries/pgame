#lang racket

(provide state)

(require syntax/parse/define
         logger
         "pure.rkt"
         spipe
         (for-syntax racket/base racket/syntax))


(define-syntax-parser state
  ([_ name
      ((~datum enter)
       enter-body ...)
      ((~datum pre)
       pre-body ...)
      ((~datum pure)
       pure-body ...)
      ((~datum post)
       post-body ...)
      ((~datum exit)
       exit-body ...)]
   #:with name-core (format-id #'name "~a-core" #'name)
   #'(begin
       (define (name state)
         (trce^ name)
         (H~>
           state
           enter-body ...
           ((set-fsm 'name-core) game.fsm)))
       (define (name-core state)
         (H~>
           state
           pre-body ...
           ((lambda (game)
            (H~> game pure-body ...)) game)
           post-body ...
           )))))
