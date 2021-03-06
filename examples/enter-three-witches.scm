#!/usr/bin/env bash
# -*- wisp -*-
exec -a "$0" guile -L $(dirname $(dirname $(realpath "$0"))) -e '(examples enter-three-witches)' -c '' "$@"
; !#

(define-module (examples enter-three-witches)
    #:export (introduced-names ->string show colortable color say-words say-name say Speak Speak-indirect Enter Scene main))

(use-modules (ice-9 optargs)
              (srfi srfi-1)
              (system syntax)
              (ice-9 rdelim))

;; FIXME: This version currently does not allow using the same first
;; name several times. It will need a more refined macro generator
;; which first gathers all the necessary definitions and then builds
;; them.

(define introduced-names '())

(define (->string x)
       (cond
         ((symbol? x)
           (symbol->string x))
         ((number? x)
           (format #f "~a" x))
         (else
           (format #f "~A" x))))


(define (show str)
      (let lp ((chars (string->list str)))
          (cond 
            ((null? chars)
              #t)
            (else
              (display (car chars))
              (usleep 60000)
              (lp (cdr chars))))))


(define colortable
    `(
      (#f . "\x1b[0m")
      (black . "\x1b[1;30m")
      (red . "\x1b[1;31m")
      (green . "\x1b[1;32m")
      (yellow . "\x1b[1;33m")
      (blue . "\x1b[1;34m")
      (magenta . "\x1b[1;35m")
      (cyan . "\x1b[1;36m")
      (white . "\x1b[1;37m")))
      

(define (color col)
       "helper function to colorize the input"
       (cond
         ((assoc col colortable)
           (format #t (assoc-ref colortable col))
           #f)
         (else
           (format #t (assoc-ref colortable #f))
           #f)))

(define-syntax say-words 
    (lambda (x)
        (syntax-case x (fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0)
            ((_ (((word words ...))) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0 (() lines ...))
              ;; TODO: move out to a helper macro
              #`(begin
                 (let ((w `word))
                   (cond
                     ((equal? w #f)
                       #f)
                     ((equal? w '..)
                       (show "."))
                     (else
                       (show " ")
                       (show (->string w)))))
                 (say-words (((words ...))) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0 (() lines ...))))
            ((_ ((())) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0  (() lines ...))
              #`(begin
                 (usleep 200000)
                 (newline)
                 (say-words (lines ...))))
            ;; lines of form ,(...)
            ((_ ((unq (word words ...)) lines ...))
              #`(if (equal? 'unquote `unq ));; FIXME: This guard seems to not actually work
              #`(begin ; add an extra level of parens
                 (show " ")
                 (say-words (((unq (word words ...)))) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0 (() lines ...))))
            ((_ (((unq word)) lines ...))
              #`(if (equal? 'unquote-splicing `unq ));; FIXME: This guard seems to not actually work
              #`(begin ; include the unquoting without extra level of parentheses
                 ;; TODO: clean this up. This duplicates logic in the first case, and duplicates it again internally.
                 (show " ")
                 (apply
                     (?? (unq x)
                        (cond
                          ((equal? 'unquote-splicing unq)
                            (map (?? (x) (show " ")(show x))
                                (if (pair? x)
                                     (map ->string x)
                                     x)))
                          ((equal? 'unquote unq)
                            (cond
                              ((equal? x #f)
                                #f)
                              ((equal? x '..)
                                (show "."))
                              (else
                                (show " ")
                                (show (->string x)))))
                          (else
                            (cond
                              ((equal? unq #f)
                                #f)
                              ((equal? unq '..)
                                (show "."))
                              (else
                                (show " ")
                                (show (->string unq))))
                            (cond
                              ((equal? x #f)
                                #f)
                              ((equal? x '..)
                                (show "."))
                              (else
                                (show " ")
                                (show (->string x)))))))
                     (list 'unq word))
                 (say-words ((())) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0 (() lines ...))))
            ((_ ((word words ...) lines ...) ); start of a line
              #`(begin
                 (show " ")
                 (say-words (((word words ...))) fdb6c10f-f8bf-4bb5-82c5-d3a5cd37b7c0 (() lines ...))))
            ((_ (() lines ...) ); finished showing the line, show the next one
              #`(say-words (lines ...)))
            ((_ (lines ...))
              #`(begin
                 (newline))))))


(define (say-name nameparts)
       (let 
         ;; symbols starting with : are not treated as part of the
         ;; name. They can be used as actor instructions
         ((pure-name (remove (lambda (x) (string-prefix? ":" (symbol->string x))) (remove pair? nameparts))))
         (if (not (member pure-name introduced-names))
              (error 
                (format #f "Tried to use ~A who did not Enter. Introduced names: ~A" 
                  pure-name introduced-names))))
       (format #t "~A\n"
         (string-join (map symbol->string nameparts))))


(define-syntax say
  (lambda (x)
    (syntax-case x ()
      ((_ nameparts lines)
        #`(begin
           (say-name nameparts)
           (say-words lines))))))


(define-syntax Speak
 (lambda (x)
  (with-ellipsis :::
   (syntax-case x ()
     ;; Support form for modifiers: enclose by double parens (used later)
     ((_ (((name :::))) ((mod :::)) (word :::) line :::)
         #`(begin
            (say-name (quasiquote (name ::: mod :::)))
            (say-words ((word :::) line :::))))
     ;; extend mod keywords
     ((_ (((name :::))) ((mod :::)) modifier line :::)
         ;; extend the modifier keyword list
         #`(Speak (((name :::))) ((mod ::: modifier)) line :::))
     ;; say form without modifier
     ((_ (((name :::))) (word :::) line :::)
         #`(Speak (((name :::))) (()) (word :::) line :::))
     ;; first modifier keyword after the name
     ((_ (((name :::))) modifier line :::)
         ;; append to mod helper form
         #`(Speak (((name :::))) ((modifier)) line :::))
     ;; Strip the name from lines with empty arguments
     ((_ (((name :::))) symbol :::)
         #`(begin #t symbol :::))))))


(define-syntax Speak-indirect
    (lambda (x)
        (syntax-case x ()
            ;; Adjust name and lines for Speak for the case where I
            ;; cannot match for the whole name.
            ;; input: (((name1 name2 ... (word ...) ...)))
            
            ;; grab the lines one by one from the back
            ((_ (((symbols ... (word ...)))) lines ...)
              #`(Speak-indirect (((symbols ...))) (word ...) lines ...))
            ;; start with the last line
            ((_ (((symbols ... (word ...)))))
              #`(Speak-indirect (((symbols ...))) (word ...)))
            ;; no more lines remain at the end: the rest must be the name
            ((_ (((name ...))) lines ...)
              #`(Speak (((name ...))) lines ...)))))


(define-syntax Enter
 (lambda (x)
  (syntax-case x ()
   ((_ (name more ...) b ...)
     ; new binding: only create it if the binding is not already a macro
     (not (eq? 'macro (syntax-local-binding (syntax name))))
     #'(begin
       ;; process the name: define special syntax for this name (only
       ;; for the first word of the name, the correctness of the rest
       ;; of the words is checked at runtime in the say procedure)
       (define-syntax name
        (lambda (y)
         (with-ellipsis :::
          (syntax-case y (more ...)
           ; just forward matching rules to Speak
           ((_ more ... symbol :::)
             #'(Speak (((name more ...))) symbol :::))
           ((_ symbols :::)
               ; this does not correctly make the second name part of
               ; the name, preventing differentiation between name and
               ; modifier
               #`(Speak-indirect (((name symbols :::)))))))))
       ;; process the rest of the names
       (Enter b ...)
       ;; record that the name was introduced. I do not see a way to do
       ;; this directly in the compiler, therefore it is checked later
       ;; during runtime.
       (set! introduced-names (cons '(name more ...) introduced-names))))
       ;; add debug output, must be added it here, not in front
       ; write 
       ;   quote : list Enter (name more ...) b ...
       ; newline
   ((_ (name more ...) b ...)
     ; existing binding: Just allow using this.
     #'(begin
        (Enter b ...)
        (set! introduced-names (cons '(name more ...) introduced-names))))
   ((_ b ...)
     #'(begin #t)))))


(define-syntax Scene
  (lambda (x)
    (syntax-case x ()
      ((_ thisscene args ...)
        (with-syntax ((c (datum->syntax x (module-name (current-module)))))
          #`(begin ; FIXME: this currently requires the Scene identifier to be a valid symbol -> cannot use "Scene 1"
             (module-re-export! (current-module)
               (module-map (?? (x y) x)
                  (module-import-interface (current-module) 'Scene ))); ensure that all symbols remain available
             (define-module (scene thisscene))
             (import c)
             #t))))))


(define (main args)
  (Enter (First Witch)
          (Second Witch)
          (Third Witch)
          (First Eldritch))

  (First Witch
      (When shall we three meet again)
      (In ,(color 'cyan) thunder, ,(color #f) ,(color 'white) lightning, ,(color #f) or in ,(color 'blue) rain? ,(color #f)))
  
  (Second Witch :resolute
      (When the hurlyburly's done, (we ,(+ 1 2)) ); inline-code is allowed!
      (When the ,(color 'red) battle's ,(color #f) 
         lost and won. )); ,(read-char) ; and executed when the word is shown

  (Third Witch
      (That will be ere the set of ,(color 'yellow) sun ,(color #f) ..))
      ; .. can be used for a . without preceding space. It MUST be
      ; used to get a trailing .

  (First Eldritch :crazy
      (,(color 'magenta) gnignigni! ,(color #f)))

  (Enter (Second Eldritch))
  
  (Second Eldritch :quick
      (,(color 'black) Guh!)
      ; . :goo ; invalid ??? would be an error
      ; . foo ; invalid ??? would be an error
      (Moo ,(color #f))))

;; Making the name longer throws an Error, but only at runtime:
;  Second Eldritch shoo
;      Guh!
;; ??? ERROR: Tried to use (Second Eldritch shoo) who did not Enter. Introduced names: ((Second Eldritch) (First Witch) (Second Witch) (Third Witch) (First Eldritch))

;; Adding one who did not enter throws an Error, but only at runtime:
;  Third Eldritch
;      Guh!
;; ??? ERROR: Tried to use (Third Eldritch) who did not Enter. Introduced names: ((Second Eldritch) (First Witch) (Second Witch) (Third Witch) (First Eldritch))






