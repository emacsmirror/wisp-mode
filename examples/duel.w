#!/usr/bin/env bash
# -*- wisp -*-
D="$(dirname $(realpath "$0"))"
W="$(dirname $(dirname $(realpath "$0")))"
guile -L "$W" -c '(import (language wisp spec))'
exec -a "$0" guile -L "$W" --language=wisp -x .w -e '(examples duel)' -c '' "$@"
; !#

define-module : examples duel
    . #:export : main

import : examples enter-three-witches

set! *random-state* : random-state-from-platform

Enter : Galtag Nimbleday
        Lowlife Pirate
        choose your answer

define challenges
  `
    : You fight like a Dairy Farmer!
      How appropriate! You fight like a cow!
      And I've got a little TIP for you, get the POINT?

; write answers
; newline

define : random-challenge
         list-ref challenges : random : length challenges

define : list->textline L
         string-join : map ->string L
                     . " "

;; TODO: define-syntax define-interaction
define : duel me other
  let*
    : challenge : random-challenge
      tease : car challenge
      answers : map list->textline : cdr challenge
    say-name other
    say-words
      : ,@tease
    say-name ' : choose your answer
    ;; TODO: shuffle the answers, check whether the right one is given
    ;; (the first answer in the original ordering is the right one)
    say-words
         : ,(let ((counter 0))
             (string-join
               (map (λ (x)
                       (set! counter (+ 1 counter))
                       (string-append (number->string counter) "  " x)) answers) "\n  "))
    let
      : answer : list-ref answers (- (string->number (format #f "~a" (peek-char))) 1)
      drain-input (current-input-port)
      say-name me
      say-words
        : ,answer
    

define-syntax-rule : Duel fighter1 fighter2
   duel
     quote fighter1
     quote fighter2


define : main args
  Duel
      Galtag Nimbleday
      Lowlife Pirate

