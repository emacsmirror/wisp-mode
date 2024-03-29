;; Language interface for Wisp in Guile
;;
;;; -Author: Arne Babenhauserheide

;;; adapted from guile-sweet: https://gitorious.org/nacre/guile-sweet/source/ae306867e371cb4b56e00bb60a50d9a0b8353109:sweet/common.scm

;;; Copyright (C) 2005--2014 by David A. Wheeler and Alan Manuel K. Gloria
;;; Copyright (C) 2014--2023 Arne Babenhauserheide.
;;; Copyright (C) 2023 Maxime Devos <maximedevos@telenet.be>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

; adapted from spec.scm: https://gitorious.org/nacre/guile-sweet/source/ae306867e371cb4b56e00bb60a50d9a0b8353109:sweet/spec.scm
define-module : language wisp spec
;   . #:use-module : wisp
  . #:use-module : language wisp
  . #:use-module : system base compile
  . #:use-module : system base language
  . #:use-module : language scheme compile-tree-il
  . #:use-module : language scheme decompile-tree-il
  . #:export : wisp

;;;
;;; Language definition
;;;

define : read-one-wisp-sexp port env
  ;; Allow using "# foo" as #(foo).
  ;; Don't use the globally-acting read-hash-extend, because this
  ;; doesn't make much sense in parenthese-y (non-Wisp) Scheme.
  ;; Instead, use fluids to temporarily add the extension.
  with-fluids : : %read-hash-procedures : fluid-ref %read-hash-procedures
    read-hash-extend #\# : lambda args #\#
    ;; Read Wisp files as UTF-8, to support non-ASCII characters.
    ;; TODO: would be nice to support ';; coding: whatever' lines
    ;; like in parenthese-y Scheme.
    set-port-encoding! port "UTF-8"
    if : eof-object? : peek-char port
        read-char port ; return eof: we’re done
        let : : chunk : wisp-scheme-read-chunk port
          and : not : null? chunk ; <---- XXX: maybe (pair? chunk)
               car chunk

define-language wisp
  . #:title "Wisp Scheme Syntax. See SRFI-119 for details. THIS IS EXPERIMENTAL, USE AT YOUR OWN RISK"
  ; . #:reader read-one-wisp-sexp
  . #:reader read-one-wisp-sexp ; : lambda (port env) : let ((x (read-one-wisp-sexp port env))) (display x)(newline) x ;
  . #:compilers `((tree-il . ,compile-tree-il))
  . #:decompilers `((tree-il . ,decompile-tree-il))
  . #:evaluator : lambda (x module) : primitive-eval x
  . #:printer write ; TODO: backtransform to wisp? Use source-properties?
  . #:make-default-environment
  lambda :
    ;; Ideally we'd duplicate the whole module hierarchy so that `set!',
    ;; `fluid-set!', etc. don't have any effect in the current environment.
    let : : m : make-fresh-user-module
      ;; Provide a separate `current-reader' fluid so that
      ;; compile-time changes to `current-reader' are
      ;; limited to the current compilation unit.
      module-define! m 'current-reader : make-fluid
      ;; Default to `simple-format', as is the case until
      ;; (ice-9 format) is loaded. This allows
      ;; compile-time warnings to be emitted when using
      ;; unsupported options.
      module-set! m 'format simple-format
      . m

