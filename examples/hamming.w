#!/usr/bin/env sh
# -*- wisp -*-
guile -L $(dirname $(dirname $(realpath "$0"))) -c '(import (language wisp spec))'
exec guile -L $(dirname $(dirname $(realpath "$0"))) -x .w --language=wisp -e '(@@ (examples hamming) main)' -s "$0" "$@"
; !#

define-module : examples hamming
import : examples doctests
         ice-9 match
         srfi srfi-1 ;; list operations

define : mod2sum . numbers
       . "Modulo-2 sum, i.e. for even parity"
       ##
           tests : test-eqv 1 (mod2sum 1 0 1 1 0)
       modulo (apply + numbers) 2

define : hamming-11/7-encode numbers
       . "Hamming encoding for a list of 7 zeros or ones."
       ##
           tests
               test-equal '(0 0 1 0 0 0 0 1 0 0 1)
                   hamming-11/7-encode '(1 0 0 0 0 0 1)
       define : H . bits
           apply mod2sum bits
       match numbers
           : i3 i5 i6 i7 i9 i10 i11
             list
                 H i3 i5 i7 i9 i11  ;; bit 1
                 H i3 i6 i7 i10 i11 ;; bit 2
                 . i3               ;; bit 3
                 H i5 i6 i7         ;; bit 4
                 . i5 i6 i7         ;; bit 5, 6, 7
                 H i9 i10 i11       ;; bit 8
                 . i9 i10 i11       ;; bit 9, 10, 11

define : flip numbers index
    . "flip the bit-number (0→1 or 1→0) at the index."
    ## : tests : test-equal '(1 0 1) : flip '(0 0 1) 0
    append
        take numbers index
        list : mod2sum 1 : list-ref numbers index
        drop numbers {index + 1}

define : hamming-11/7-decode numbers
       . "Hamming decoding for a list of 11 zeros or ones"
       ##
         tests
             test-equal '(1 0 0 0 0 0 1)
                 hamming-11/7-decode '(0 0 1 0 0 0 0 1 0 0 1)
             test-equal '(1 0 0 0 0 0 1)
                 hamming-11/7-decode : flip '(0 0 1 0 0 0 0 1 0 0 1) 5
             test-equal '(0 1 0 0 0 0 1)
                 hamming-11/7-decode '(0 1 0 1 1 1 0 1 0 0 1)
                 
       define m2+ mod2sum
       define broken-bit
          match numbers
              : h1 h2 i3 h4 i5 i6 i7 h8 i9 i10 i11
                +
                  * 1 : m2+ h1 i3 i5 i7 i9 i11
                  * 2 : m2+ h2 i3 i6 i7 i10 i11
                  * 4 : m2+ h4 i5 i6 i7
                  * 8 : m2+ h8 i9 i10 i11
       define fixed
           if : zero? broken-bit
              . numbers
              flip numbers {broken-bit - 1}
       match fixed
           : h1 h2 i3 h4 i5 i6 i7 h8 i9 i10 i11
             list i3 i5 i6 i7 i9 i10 i11
          

define %this-module : current-module
define : main args
       . " Testing doctests"
       doctests-testmod %this-module

