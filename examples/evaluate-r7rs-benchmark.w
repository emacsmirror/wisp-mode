#!/bin/sh
# -*- wisp -*-
guile -L $(dirname $(dirname $(realpath "$0"))) -c '(import (language wisp spec))'
exec -a "$0" guile -L $(dirname $(dirname $(realpath "$0"))) --language=wisp -x .w -e '(examples evaluate-r7rs-benchmark)' -c '' "$@"
!#

;; Evaluate the benchmarks from ecraven at http://ecraven.github.io/r7rs-benchmarks/benchmark.html
;; Uses data from http://ecraven.github.io/r7rs-benchmarks/all.csv

;; example usage: 
;; $ for i in bigloo bones chez chibi chicken- chickencsi- cyclone femtolisp foment gambitc gauche guile ironscheme kawa larceny mit mosh petite picrin racket rhizome rscheme s9fes sagittarius scheme48- stalin tinyscheme vicare ypsilon; do echo $i $(./evaluate-r7rs-benchmark.w guile-ecraven-benchmarks-result-2017-08-13.csv $i | grep Geom -A 2 | grep -v = | grep .); done | sed 's/(//' > evaluate-r7rs-benchmark.data
;; $ echo -e 'set xtics rotate by 90 right\nplot "< sort -g -k2 evaluate-r7rs-benchmark.data" using 0:2:xtic(1) with lines title "runtime: geometric mean multiple of fastest", "< sort -g -k2 evaluate-r7rs-benchmark.data" using 0:3:xtic(1) with lines title "successful tests"' | gnuplot -p
;;
;; evaluation into a table sorted by slowdown:
;; $ for i in guile-3.0.5 guile-2 bigloo bones chez chibi- chicken cyclone femtolisp foment gambitc gauche ironscheme kawa loko mit petite racket s7 s9fes sagittarius; do  ./evaluate-r7rs-benchmark.w  ~/Downloads/all.csv $i | grep -A2 '===.*Geometric' | sed s/Geometric.*===// | sed 's/=== //' | xargs ; done 2>/dev/null | column -t | sort -k2 -g

define-module : examples evaluate-r7rs-benchmark
    . #:export : main

import : ice-9 rdelim
         srfi srfi-1
         ice-9 pretty-print
         ice-9 optargs
         ice-9 i18n

define : read-csv port
    let loop : : lines '()
        if : eof-object? : peek-char port 
           reverse : map (λ (x) (string-split x #\,)) lines
           loop : cons (read-line port) lines

define : min-alist-by-test data-by-project
    let lp 
        : min-data '()
          data-by-project data-by-project
        if : null? data-by-project
           . min-data
           let*
             : proj : car : car data-by-project
               test : car : cdr : car data-by-project
               time : string->number : car : cdr : cdr : car data-by-project
               best : assoc-ref min-data test
             lp
                if : and time : or (not best) : < time best
                   assoc-set! min-data test time
                   . min-data
                cdr data-by-project

define : select-project-data data-by-project project
       define : notproj? datapoint
              not : string-prefix? project : car datapoint
       define : only-project data
              remove notproj? data
       map cdr : only-project data-by-project

define : get-multiples guile-data data-min-by-test
  let lp 
      : gd guile-data
        multiples-of-best '()
      if : null? gd
         remove (λ(x) (equal? #f x)) multiples-of-best
         let*
             : guile : string->number : car : cdr : car gd
               test : car : car gd
               multiple
                 if : not guile
                    . guile
                    / guile
                      or (assoc-ref data-min-by-test test) guile
             lp : cdr gd
                  if multiple
                     cons multiple multiples-of-best
                     . multiples-of-best


define : get-multiples-alist guile-data data-min-by-test
  let lp 
      : gd guile-data
        multiples-of-best '()
      if : null? gd
         remove (λ(x) (equal? #f x)) multiples-of-best
         let*
             : guile : string->number : car : cdr : car gd
               test : car : car gd
               multiple
                 if : not guile
                    cons test guile
                    cons test 
                      / guile
                        or (assoc-ref data-min-by-test test) guile
             lp : cdr gd
                  if multiple
                     cons multiple multiples-of-best
                     . multiples-of-best


define : help args
    format #t "Usage: ~a [--help] [--csv] csv-file [project-prefix]\n" (car args)

define args : program-arguments

when : null? : cdr args
     help args
     exit 1

define csv-file
    car : cdr args

define : remove-options args
    . "remove all options (starting with -) from the argument list. This ignores --."
    remove : λ(x) : string-prefix? "-" x

define project-prefix
    if : null? : cdr : cdr args
       . "guile"
       car : cdr : cdr args

define : main args
    pretty-print args
    when : and {(length args) > 1} : equal? "--help" : second args 
         help args
         exit 0
    let*
      : port : open-input-file csv-file
        data-by-project : read-csv port
        data-min-by-test : min-alist-by-test data-by-project
        guile-data : select-project-data data-by-project project-prefix
      when : member "--csv" args
          ; display "test slowdown\n"
          map : λ (x) : apply format #t "~a ~a\n" : list (car x) (cdr x)            
                  get-multiples-alist guile-data data-min-by-test          
          format #t "total ~a\n"
              if : null? : get-multiples guile-data data-min-by-test
                  . #f
                  expt
                      apply * : get-multiples guile-data data-min-by-test
                      / 1 : length : get-multiples guile-data data-min-by-test
          exit 0
          
      display "=== Best times ===\n\n"
      pretty-print : sort data-min-by-test (λ (x y) (string<? (car x) (car y)))
      newline
      format #t "=== ~a times ===\n\n" : string-locale-titlecase project-prefix
      pretty-print : sort guile-data (λ (x y) (string<? (car x) (car y)))
      newline
      format #t "=== ~a slowdown ===\n\n" : string-locale-titlecase project-prefix
      pretty-print
        sort : get-multiples-alist guile-data data-min-by-test
             λ (x y) (string<? (car x) (car y))
      newline
      format #t "=== ~a Geometric Mean slowdown (successful tests / total tests) ===\n\n" : string-locale-titlecase project-prefix
      format #t "~a (~a / ~a)"
         if : null? : get-multiples guile-data data-min-by-test
            . #f
            expt
               apply * : get-multiples guile-data data-min-by-test
               / 1 : length : get-multiples guile-data data-min-by-test
         length : remove (λ(x) (equal? #f (string->number (car (cdr x))))) guile-data
         length guile-data
      newline
