#lang racket/base

(require
  (for-syntax racket/base)
  syntax/parse/define)

(define-syntax-parser reprovide
  ([_ module-path:expr ...+]
   #'(begin
       (provide (all-from-out module-path ...))
       (require module-path ...))))

(reprovide
  "drawing.rkt"
  "impure.rkt"
  "pure.rkt"
  "shutdown.rkt"
  "state.rkt"
  "states/decode-tmx.rkt"
  "states/menu.rkt"
  "states/top-map.rkt"
  ffi/vector
  finalizer
  glfw3
  logger
  math/matrix
  memo
  nested-hash
  opengl
  opengl/util
  racket/list
  spipe
  syntax/parse/define
  threading
  )
