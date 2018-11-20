#lang racket

(require pgame/utils)

(define-recursive-load states "states" functions->hash)

((hash-ref states 'begin-state) (hasheq 'io (hasheq 'states states)))
