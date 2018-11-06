#! /usr/bin/env racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main entry point for pgame
;;
;; This file sets up the reloadable entry point and break
;; handler, allowing one to modify code while running
;; the program.
;;
;; For documentation first run `raco pkg install pgame/`,
;; and then see `raco docs pgame`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/base

(require reloadable)

(define core
  (reloadable-entry-point->procedure
    (make-reloadable-entry-point 'core "source/core.rkt")))

(void (reload!))
(void (core '()))
