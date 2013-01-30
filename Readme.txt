Wisp: Whitespace-Lisp
=====================

    (defun a (b c)
      (let 
        ((d e)
          ((f)
          ,(g)))
      '(h i)
      j)

becomes

    defun a
      let
        : d e
          : f
            , g
      ' h i
      . j


State: basics work. : and , and ' as start of the line do not work yet.
