#!/usr/bin/env sh
# -*- wisp -*-
guile -L $(dirname $(dirname $(realpath "$0"))) -c '(import (language wisp spec))'
exec guile -L $(dirname $(dirname $(realpath "$0"))) --language=wisp -e '(@@ (examples doctests) main)' -s "$0" "$@"
; !#

define-module : examples doctests
              . #:export : doctests-testmod

import : ice-9 optargs
         ice-9 rdelim
         ice-9 match
         ice-9 pretty-print
         oop goops
         texinfo reflection

; define basic dir
define* (dir #:key (all? #f))
   if all?
      map (λ (x) (cons (module-name x)
                        (module-map (λ (sym var) sym) (resolve-interface (module-name x)))))
           cons (current-module) : module-uses (current-module)
      module-map (λ (sym var) sym) (current-module)
; add support for giving the module as argument
define-generic dir
define-method (dir (all? <boolean>)) (dir #:all? all?)
define-method (dir (m <list>)) (module-map (λ (sym var) sym) (resolve-interface m))
; add support for using modules directly (interfaces are also modules, so this catches both)
define-method (dir (m <module>)) (module-map (λ (sym var) sym) m)

define : string-index s fragment
       . "return the index of the first character of the FRAGMENT in string S."
       let loop : (s s) (i 0)
           if : = 0 : string-length s
              . #f
              if : string-prefix? fragment s
                 . i
                 loop (string-drop s 1) (+ i 1)

define : doctests-extract-from-string s
       . "Extract all test calls from a given string."
       let lp
           : str s
             tests : list
           if : string-null? str
              reverse tests
              let : : idx : string-index str "(test"
                  if : not idx
                      reverse tests
                      let : : sub : substring str idx
                          lp ; recurse with the rest of the string
                             with-input-from-string sub
                                 λ () (read) (read-string)
                             cons
                                 with-input-from-string sub
                                     λ () : read
                                 . tests

define : subtract a b
    . "Subtract B from A"
    . #((tests ('positive (test-eqv 3 (subtract 5 2)))))
    - a b

define : doctests-testmod mod
       . "Execute all doctests in the current module

          This procedure provides an example test:"
       . #((tests 
            ('mytest
              (define v (make-vector 5 99))
              (test-assert (vector? v))
              (test-eqv 99 (vector-ref v 2))
              (vector-set! v 2 7)
              (test-eqv 7 (vector-ref v 2)))
            ('mytest2
              (test-assert #t))))
       ;; thanks to Vítor De Araújo: https://lists.gnu.org/archive/html/guile-user/2017-08/msg00003.html
       let*
           : names : module-map (λ (sym var) sym) mod
             filename
                 if (module-filename mod) (string-join (string-split (module-filename mod) #\/ ) "-")
                     string-join (cons "._" (map symbol->string (module-name mod))) "-"
             docstrings
                 map (λ (x) (if (procedure? x) (procedure-documentation x)))
                     map (λ (x) (module-ref mod x)) names
             doctests
                 map (λ (x) (if (procedure? x) (procedure-property x 'tests)))
                     map (λ (x) (module-ref mod x)) names
           let loop
               : names names
                 docstrings docstrings
                 doctests doctests
               ;; pretty-print doctests
               ;; newline
               when : not : null? docstrings
                   when doctests
                       let*
                           : name : car names
                             docstring : car docstrings
                           let loop-tests
                              : doctest : car doctests
                              when : and (pair? doctest) (car doctest) : pair? : car doctest
                                 ;; pretty-print : car doctest
                                 ;; newline
                                 let*
                                   :
                                     testid
                                        if : not : pair? doctest
                                             . #f
                                             string-join : list filename (symbol->string name) : symbol->string : primitive-eval : car : car doctest
                                                         . "--"
                                     cleaned
                                         if : not : list? doctest
                                           . '#f
                                           append
                                               cons 'begin
                                                   cons '(import (srfi srfi-64))
                                                       cons
                                                           list 'test-begin testid
                                                           cdr : car doctest
                                               list : list 'test-end testid
                                   ;; pretty-print cleaned
                                   ;; newline
                                   when cleaned
                                       let :
                                           eval cleaned mod
                                       newline
                                   loop-tests : cdr doctest
                   loop (cdr names) (cdr docstrings) (cdr doctests)

define %this-module : current-module
define : main args
         doctests-testmod %this-module

