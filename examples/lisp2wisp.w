#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(dirname $(realpath "$0"))) -c '(import (language wisp spec))'
exec -a "$0" guile -L $(dirname $(dirname $(realpath "$0"))) --language=wisp -x .w -e '(examples lisp2wisp)' -c '' "$@"
; !#

;; Turning lisp-code programs into wisp-code — approximate inverse of wisp2lisp.

;; Limitation: Currently this strips out comments. TODO: strip all comments and assign them to the correct lines categorized by line-number.

;; Approach:
;; - Read the AST as list of lists with repeated (read)
;; - Turn it into basic wisp that only uses the : for empty lines and uses no parens at all
;; - Collapse lines using : and () with a heuristic (maximum line-length, known forms).
;; - use curly-infix

;; for emacs (progn (defun test-this-file () (interactive) (save-current-buffer) (async-shell-command (concat (buffer-file-name (current-buffer)) " --test"))) (local-set-key (kbd "<f9>") 'test-this-file))



define-module : examples lisp2wisp
              . #:export (lisp2wisp main)

import : srfi srfi-42 ; list-ec
         srfi srfi-9 ; records
         ice-9 optargs

import : examples doctests
         srfi srfi-1 ; list operations
         srfi srfi-37 ; commandline parsing
         srfi srfi-60 ; bit conversion via integer->list 
         rnrs bytevectors
         ice-9 match
         ice-9 format
         ice-9 rdelim ; for read-string
         ice-9 binary-ports
         ice-9 pretty-print

define : read-file filepath
    let* 
        : port : open-input-file filepath
          data : read-delimited "" port
        close port
        . data

define : write-file filepath bytevector
    let* 
        : port : open-output-file filepath
        put-bytevector port bytevector
        close port


define : read-all port
  let readloop : : res : '
    pretty-print res
    let : : next : read port
      pretty-print next
      if : eof-object? next
          . res 
          readloop : append res : list next

define : lisp2wisp port
    ##
        tests
            test-equal : string-trim-right : read-file "../tests/btest.scm"
                lisp2wisp : open-input-file "../tests/btest.scm"
    string-join : map (λ(x) (format #f "~s" x)) : read-all port
                . "\n"

define %this-module : current-module
define : main args
    doctests-testmod %this-module
