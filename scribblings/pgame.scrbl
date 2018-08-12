#lang scribble/manual
@require[@for-label[pgame
                    racket/base]]

@title{pgame}
@author{Kevin Robert Stravers}

@defmodule[pgame]

The overarching state of the program is referred to as @racket[state].

@racket[state] has two subcomponents: @racket[system] and @racket[game].

@racket[system] contains data regarding the screen, interfaces, files, etc.

@racket[game] contains pure game data and is guaranteed to be serializable.
