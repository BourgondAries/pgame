#lang racket

(require pgame/utils)

(define-recursive-load states "states")

((hash-ref (functions->hash states) 'begin-state)
 (hasheq 'io (hasheq 'states (functions->hash states))))
