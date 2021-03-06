#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(dirname $(realpath "$0"))) -c '(import (wisp-scheme) (language wisp spec))'
exec -a "$0" guile -L $(dirname $(dirname $(realpath "$0"))) --language=wisp -x .w -e '(examples unicode-math-fun)' -c '' "$@"
; !#

define-module : examples unicode-math-fun
    . #:export : main

;; Having fun with unicode and math :)

define : Σ . n
    apply + n

define : Π . n
    apply * n

define : ∪ . lists
    apply append lists

define : ∩ list1 list2
    let : : h : make-hash-table : length list2
        let fill : : toadd list2
            when : not : equal? toadd '()
                hash-set! h (list-ref toadd 0) #t
                fill : list-tail toadd 1
        let loop : (inboth '()) (tocheck list1)
            if : equal? tocheck '()
                . inboth
                let : : cur : list-ref tocheck 0
                    if : hash-ref h cur
                        loop 
                            append inboth : list cur
                            list-tail tocheck 1
                        loop inboth : list-tail tocheck 1

define : main args
    display : Σ 1 2 8 0  5 7 59 12 5
    newline
    display : Π 1 2 8 0  5 7 59 12 5
    newline
    display : ∪ '(1 2 3) '(4 5 6)
    newline
    display : ∩ '(1 789 7 897 89 78 78 97 89 2 3 6) '(4 5 6 2  8 7 879 879 879 879 8797 97 97 987 89789 7 7897 987 897 987 87 897 896)
    newline
