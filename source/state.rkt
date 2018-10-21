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
   #:with (requires ...) (datum->syntax #'name (list "../pure.rkt" "../impure.rkt" "../drawing.rkt" "../shutdown.rkt") #'name)
   #'(begin
       (require requires ...)
       (define (name state)
         (trce^ `(entering name))
         (trce^ name)
         (H~>
           state
           enter-body ...
           ((set-fsm 'name-core) ae.fsm)))
       (define (name-core state)
         (H~>
           state
           pre-body ...
           (H~> ae
                pure-body ...)
           post-body ...
           )))))
