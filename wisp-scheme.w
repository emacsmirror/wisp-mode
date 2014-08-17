#!/home/arne/wisp/wisp-multiline.sh
; !#

;; Scheme-only implementation of a wisp-preprocessor which output a
;; scheme Tree IL to feed to a scheme interpreter instead of a
;; preprocessed file.

;; Plan:
;; read reads the first expression from a string. It ignores comments,
;; so we have to treat these specially. Our wisp-reader only needs to
;; worry about whitespace.
;; 
;; So we can skip all the string and bracket linebreak escaping and
;; directly create a list of codelines with indentation. For this we
;; then simply reuse the appropriate function from the generic wisp
;; preprocessor.

use-modules : srfi srfi-1

;; Helper functions for the indent-and-symbols data structure: '((indent token token ...) ...)
define : line-indent line
         car line

define : line-code line
         cdr line

define : line-continues? line
         equal? "." : car : line-code line

define : line-only-colon? line
         and
           equal? ":" : car : line-code line
           null? : cdr : line-code line

define : line-empty-code? line
         null? : line-code line

define : line-empty? line
         and
           = 0 : line-indent line
           line-empty-code? line


define : wisp-scheme-read-chunk-lines port
         let loop
           : indent-and-symbols : list ; '((5 "(foobar)" "\"yobble\"")(3 "#t"))
             inindent #t
             inunderscoreindent : equal? #\_ : peek-char port
             incomment #f
             currentindent 0
             currentsymbols '()
             emptylines 0
           let : : next-char : peek-char port
             cond
               : eof-object? next-char
                 append indent-and-symbols : list : append (list currentindent) currentsymbols
               : <= 2 emptylines
                 . indent-and-symbols
               : and inindent : equal? #\space next-char
                 read-char port ; remove char
                 loop
                   . indent-and-symbols
                   . #t ; inindent
                   . #f ; inunderscoreindent
                   . #f ; incomment
                   1+ currentindent
                   . currentsymbols
                   . emptylines
               : and inunderscoreindent : equal? #\_ next-char
                 read-char port ; remove char
                 loop 
                   . indent-and-symbols
                   . #t ; inindent
                   . #t ; inunderscoreindent
                   . #f ; incomment
                   1+ currentindent
                   . currentsymbols
                   . emptylines
               ; any char but whitespace *after* underscoreindent is
               ; an error. This is stricter than the current wisp
               ; syntax definition. TODO: Fix the definition. Better
               ; start too strict.
               : and inunderscoreindent : not : equal? #\space next-char
                 throw 'wisp-syntax-error "initial underscores without following whitespace at beginning of the line after" : last indent-and-symbols
               : or (equal? #\newline next-char) (equal? #\return next-char)
                 read-char port ; remove the newline
                 ; TODO: Check whether when or if should be preferred here. guile 1.8 only has if.
                 if : and (equal? #\newline next-char) : equal? #\return : peek-char port
                      read-char port ; remove a full \n\r. Damn special cases...
                 let* ; distinguish pure whitespace lines and lines
                      ; with comment by giving the former zero
                      ; indent. Lines with a comment at zero indent
                      ; get indent -1 for the same reason - meaning
                      ; not actually empty.
                   :
                     indent 
                       cond 
                         incomment 
                           if : = 0 currentindent ; specialcase
                             . -1
                             . currentindent 
                         : not : null? currentsymbols ; pure whitespace
                           . currentindent
                         else
                           . 0
                     parsedline : append (list indent) currentsymbols
                   ; TODO: If the line is empty, . Either do it here and do not add it, just
                   ; increment the empty line counter, or strip it later. Replace indent
                   ; -1 by indent 0 afterwards.
                   loop
                     append indent-and-symbols : list parsedline
                     . #t ; inindent
                     equal? #\_ : peek-char port
                     . #f ; incomment
                     . 0
                     . '()
                     if : line-empty? parsedline
                       1+ emptylines
                       . 0
               : equal? #t incomment
                 read-char port ; remove one comment character
                 loop 
                   . indent-and-symbols
                   . #f ; inindent 
                   . #f ; inunderscoreindent 
                   . #t ; incomment
                   . currentindent
                   . currentsymbols
                   . emptylines
               : equal? #\space next-char ; remove whitespace when not in indent
                 read-char port ; remove char
                 loop 
                   . indent-and-symbols
                   . #f ; inindent
                   . #f ; inunderscoreindent
                   . #f ; incomment
                   . currentindent
                   . currentsymbols
                   . emptylines
                        ; | cludge to appease the former wisp parser
                        ; | which had a prblem with the literal comment
                        ; v char.
               : equal? (string-ref ";" 0) next-char
                 loop 
                   . indent-and-symbols
                   . #f ; inindent 
                   . #f ; inunderscoreindent 
                   . #t ; incomment
                   . currentindent
                   . currentsymbols
                   . emptylines
               else ; use the reader
                 loop 
                   . indent-and-symbols
                   . #f ; inindent
                   . #f ; inunderscoreindent
                   . #f ; incomment
                   . currentindent
                   ; this also takes care of the hashbang and leading comments.
                   append currentsymbols : list : read port
                   . emptylines

define : line-append-n-parens n line
         . "Append N parens at the end of the line"
         let loop : (rest n) (l line)
           cond
             : = 0 rest 
               . l
             else
               loop (1- rest) (append l '(")"))

define : line-prepend-n-parens n line
         . "Prepend N parens at the beginning of the line, but after the indentation-marker"
         let loop : (rest n) (l line)
           cond
             : = 0 rest 
               . l
             else
               loop 
                 1- rest
                 append 
                   list : car l
                   . '("(")
                   cdr l

; TODO: process inline colons

define : wisp-indentation-to-parens lines
         . "Add parentheses to lines and remove the indentation markers"
         let loop
           : processed '()
             current-line : car lines
             unprocessed : cdr lines
             indentation-levels '(0)
           cond
             ; the recursion end-condition
             : and (null? current-line) (null? unprocessed) (not (null? indentation-levels))
               throw 'wisp-programming-error "The current-line is null but there are indentation-levels: Something returned a broken line as new current-line."
             : and (not (null? current-line)) (null? indentation-levels)
               throw 'wisp-programming-error "The indentation-levels are null but the current-line is null: Something killed the indentation-levels."
             ; the recursion end-condition
             : and (null? current-line) (null? unprocessed)
               . processed
             ; now care for the last step
             : null? unprocessed 
               ; current is the last line
               cond
                 : line-continues? current-line
                   loop
                     append processed 
                       list
                         line-append-n-parens 
                           1- : length indentation-levels
                           . current-line
                     . '() ; current-line empty: required end condition 1
                     . '() ; unprocessed empty: required end condition 2
                     . '() ; indentation-levels: There is nothing more to process
                 else
                   loop
                     append processed
                       list
                         line-prepend-n-parens 1 
                           line-append-n-parens
                             length indentation-levels
                             . current-line
                     . '() ; current-line empty: required end condition 1
                     . '() ; unprocessed empty: required end condition 2
                     . '() ; indentation-levels: There is nothing more to process
             else ; now we come to the line-comparisons and indentation-counting.
               let*
                 : next-line : car unprocessed
                 cond
                   : line-empty-code? current-line
                     ; We cannot process indentation without
                     ; code. Just switch to the next line. This should
                     ; only happen at the start of the recursion.
                     loop
                       . processed
                       . next-line
                       cdr unprocessed
                       . indentation-levels
                   : line-empty-code? next-line
                     loop
                       . processed
                       . current-line
                       cdr unprocessed
                       . indentation-levels
                   : = (line-indent current-line) (line-indent next-line)
                     if : line-continues? current-line ; no parens needed
                          loop
                            append processed : list current-line
                            . next-line
                            cdr unprocessed
                            . indentation-levels
                          loop
                            append processed 
                              list 
                                line-prepend-n-parens 1 
                                  line-append-n-parens 1 
                                    . current-line
                            . next-line
                            cdr unprocessed
                            . indentation-levels
                   else
                     throw 'wisp-not-implemented "Still need to implement the line comparisons."
             
             
             

define : wisp-scheme-read-chunk port
         . "Read and parse one chunk of wisp-code"
         let : : lines : wisp-scheme-read-chunk-lines port
             ; TODO: process indentation.
             wisp-indentation-to-parens lines

define : wisp-scheme-read-all port
         . "Read all chunks from the given port"
         let loop 
           : tokens '()
           cond
             : eof-object? : peek-char port
               ; TODO: Join as string.
               . tokens
             else
               append tokens : wisp-scheme-read-chunk port

define : wisp-scheme-read-file path
         call-with-input-file path wisp-scheme-read-all

define : wisp-scheme-read-string str
         call-with-input-string str wisp-scheme-read-all


display  
  wisp-scheme-read-string  "  foo ; bar\n  ; nop \n\n; nup\n; nup \n  \n\n\n  foo : moo \"\n\" \n___ . goo . hoo"
newline 
display : wisp-scheme-read-file "wisp-scheme.w"
newline 
; This correctly throws an error.
; display
;   wisp-scheme-read-string  "  foo \n___. goo . hoo"
; newline


