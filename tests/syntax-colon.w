let
  :
    a 1
    b 2
  let
    :
      :
        . c 3
    format #t "a: ~A, b: ~A, c: ~A"
              .    a      b      c

: a

define : hello
  display "hello\n"

let
  : a 1
    b 2
  format #t "a: ~A, b: ~A"
            .    a      b

let : : a ' :

let 
  :    ; foo
    a
      '

:
  a

define : \:
  hello

\:
