;;; wisp-mode.el --- Tools for wisp: the Whitespace-to-Lisp preprocessor

;; Copyright (C) 2013--2016  Arne Babenhauserheide <arne_bab@web.de>
;; Copyright (C) 2015--2016  Kevin W. van Rooijen — indentation and tools
;;               from https://github.com/kwrooijen/indy/blob/master/indy.el

;; Author: Arne Babenhauserheide <arne_bab@web.de>
;; Version: 0.4.0
;; Keywords: languages, lisp, scheme
;; Homepage: http://www.draketo.de/english/wisp
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use, add wisp-mode.el to your Emacs Lisp path and add the following
;; to your ~/.emacs or ~/.emacs.d/init.el
;; 
;; (require 'wisp-mode)
;; 
;; For details on wisp, see
;; https://www.draketo.de/english/wisp
;;
;; If you came here looking for wisp the lisp-to-javascript
;; compiler[1], have a look at wispjs-mode[2].
;; 
;; [1]: http://jeditoolkit.com/try-wisp
;; 
;; [2]: http://github.com/krisajenkins/wispjs-mode
;; 
;; ChangeLog:
;;
;;  - 0.4.0: provide wisp--eval-block (C-M-x)
;;           to send the current block to a buffer nammed *shell*.
;;           wisp--find-begin-and-end-of-block-around-region can cope with empty lines.
;;           wisp--eval-with-geiser converts wisp to regular scheme (with wisp2lisp) and pushes it to geiser.
;;           thanks to cwebber!
;;  - 0.3.0: provide wisp-color-indentation-minor-mode
;;           that highlights the indentation levels, following wisp-semantics (period and colon)
;;  - 0.2.9: enabled imenu - thanks to Greg Reagle!
;;  - 0.2.8: use electric-indent-inhibit instead of electric-indent-local-mode
;;           rename gpl.txt to COPYING for melpa
;;           use the variable defined by define-derived-mode
;;  - 0.2.7: dependency declared, always use wisp--prefix, homepage url
;;  - 0.2.6: remove unnecessary autoloads
;;  - 0.2.5: backtab chooses existing lower indentation values from previous lines.
;;  - 0.2.4: better indentation support:
;;           cycle forward on tab,
;;           cycle backwards on backtab (s-tab),
;;           keep indentation on enter.
;;  - 0.2.1: Disable electric-indent-local-mode in wisp-mode buffers.
;;  - 0.2: Fixed the regular expressions.  Now org-mode HTML export works with wisp-code.
;; 
;;; Code:

(require 'scheme)
(require 'comint)

; see http://www.emacswiki.org/emacs/DerivedMode

; font-lock-builtin-face 	font-lock-comment-delimiter-face
; font-lock-comment-face 	font-lock-constant-face
; font-lock-doc-face 	font-lock-fic-author-face
; font-lock-fic-face 	font-lock-function-name-face
; font-lock-keyword-face 	font-lock-negation-char-face
; font-lock-preprocessor-face 	font-lock-reference-face
; font-lock-string-face
; font-lock-type-face 	font-lock-variable-name-face
; font-lock-warning-face

; note: for easy testing: emacs -Q wisp-mode.el -e eval-buffer wisp-guile.w -e delete-other-windows


(defvar wisp-builtin '("and" "char=?" "define" "define-syntax" "define-syntax-rule" "defun" "if" "let" "let*" "not" "or" "set!" "set!" "set" "setq" "syntax-case" "syntax-rules" "when" "while")) ; alphabetical order

; TODO: Add special treatment for defun foo : bar baz ⇒ foo = function, bar and baz not.
; TODO: Add highlighting for `, , and other macro-identifiers.
; TODO: take all identifiers from scheme.el
(defvar wisp-font-lock-keywords
  `((
     ("\\`#!.*" . font-lock-comment-face) ; initial hashbang
     ("\"\\.\\*\\?" . font-lock-string-face) ; strings (anything between "")
     ("[{}]" . font-lock-string-face)      ; emphasize curly infix
     ; ("^_+ *$" . font-lock-default-face) ; line with only underscores
                                           ; and whitespace shown as
                                           ; default text. This is just
                                           ; a bad workaround.
                                           ; Which does not work because
                                           ; *-default-face is not guaranteed
                                           ; to be defined.
     ("^\\(?:_* +\\| *\\): *$" . font-lock-keyword-face) ; line with only a : + whitespace, not at the beginning
     ("^\\(?:_* +\\| *\\): \\| *\\. " . font-lock-keyword-face) ; leading : or .
     ( ,(regexp-opt wisp-builtin 'symbols) . font-lock-builtin-face) ; generic functions
     ;                                 v there is a tab here.
     ("^\\(?:_*\\)\\(?: +\\)\\([^:][^ 	]*\\)" . font-lock-function-name-face) ; function calls as start of the line
     ;                     v there is a tab here.
     ("^\\(?: *\\)[^ :][^ 	]*" . font-lock-function-name-face) ; function calls as start of the line
     (" : " "\\=\\([^ 	]+\\)" nil nil (1 font-lock-function-name-face)) ; function calls with inline :
     ("[^']( *" "\\=\\([^ 	)]+\\)" nil nil (1 font-lock-function-name-face)) ; function calls with (
     ("#[tf]"  . font-lock-constant-face) ; #t and #f
     ("#\\\\[^ 	]+"  . font-lock-constant-face) ; character literals
     (";" . 'font-lock-comment-delimiter-face)
     ; TODO: Doublecheck this regexp. I do not understand it completely anymore.
     ("\\_<[+-]?[0-9]+\\_>\\|\\_<[+-][0-9]*\\.[0-9]*\\(e[+-]?[0-9]+\\)?\\_>" . font-lock-constant-face) ; numbers
     ("'()" . font-lock-constant-face) ; empty list
     ("[ 	]'[^	 ]+" . font-lock-constant-face) ; 'name
     ; FIXME: This is too general (it will capture a . 'b, making it
     ; impossible to have 'b highlighted)
     (" : \\| \\. " . font-lock-keyword-face) ; leading : or .
     ))
  "Default highlighting expressions for wisp mode.")
(defun wisp--prev-indent ()
  "Get the amount of indentation spaces of the previous line."
  (save-mark-and-excursion
    (forward-line -1)
    (while (wisp--line-empty?)
      (forward-line -1))
    (back-to-indentation)
    (current-column)))

(defun wisp-prev-indent-lower-than (indent)
  "Get the indentation which is lower than INDENT among previous lines."
  (save-mark-and-excursion
    (forward-line -1)
    (while (or (wisp--line-empty?)
               (and (>= (wisp--current-indent) indent)
                    (> (wisp--current-indent) 0)))
      (forward-line -1))
    (back-to-indentation)
    (current-column)))

(defun wisp--line-empty? ()
  "Check if the current line is empty."
  (string-match "^\s*$" (wisp--get-current-line)))

(defun wisp--get-current-line ()
  "Get the current line as a string."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun wisp--current-indent ()
  "Get the amount of indentation spaces if the current line."
  (save-mark-and-excursion
    (back-to-indentation)
    (current-column)))

(defun wisp--fix-num (num)
  "Make sure NUM is a valid number for calculating indentation."
  (cond
   ((not num) 0)
   ((< num 0) 0)
   (t num)))

(defun wisp--indent (num)
  "Indent the current line by the amount of provided in NUM."
  (let ((currcol (current-column))
        (currind (wisp--current-indent)))
    (unless (equal currind num)
      (let ((num (max num 0)))
        (indent-line-to num))
      (unless (<= currcol currind)
        (move-to-column (wisp--fix-num (+ num (- currcol currind))))))))

(defun wisp--tab ()
  "Cycle through indentations depending on the previous line.

If the current indentation is equal to the previous line,
   increase indentation by one tab,
if the current indentation is zero,
   indent up to the previous line
if the current indentation is less than the previous line,
   increase by one tab, but at most to the previous line."
  (interactive)
  (let* ((curr (wisp--current-indent))
         (prev (wisp--prev-indent))
         (width
          (cond
           ((equal curr prev) (+ prev tab-width))
           ((= curr 0) prev)
           ((< curr prev) (min prev (+ curr tab-width)))
           (t  0))))
    (wisp--indent width)))

(defun wisp--backtab ()
  "Cycle through indentations depending on the previous line.

This is the inverse of 'wisp--tab', except that it jums from 0 to
prev, not to prev+tab."
  (interactive)
  (let* ((curr (wisp--current-indent))
         (prev (wisp--prev-indent))
         (width
          (cond
           ((<= curr prev)
            (wisp-prev-indent-lower-than curr))
           ((= curr 0) prev)
           ((> curr prev) prev)
           (t  0))))
    (wisp--indent width)))

(defun wisp--return ()
  "Enter a newline while keeping indentation."
  (interactive)
  (let* ((curr (wisp--current-indent))
         (prev (wisp--prev-indent)))
    (newline)
    (wisp--indent curr)))


(defvar wisp--eval-process-target nil)
(defun wisp--eval-block (arg)
  "Send the current block to a target buffer (by default *shell*).

Set ARG \\<mapvar> & \\[command] to select the target buffer.

Similar to `eval-defun'."
  (interactive "P")
  (if (or arg
          (not wisp--eval-process-target) ;; unset
          (not (process-live-p (get-buffer-process
                                wisp--eval-process-target))))
      (setq wisp--eval-process-target
            (completing-read
             "Shell: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list)))))
  (if (not (member wisp--eval-process-target (mapcar #'buffer-name (buffer-list))))
      (error (concat "There is no buffer named \"" wisp--eval-process-target "\": cannot send the command.
To eval the current block, please use M-x shell and open a REPL there"))
    (save-mark-and-excursion
      
      (let* ((begin-and-end (wisp--find-begin-and-end-of-block-around-region (point) (point)))
             (block (string-trim (buffer-substring-no-properties (car begin-and-end) (cdr begin-and-end)))))
        (process-send-string wisp--eval-process-target block)
        (process-send-string wisp--eval-process-target "\n\n")))))

; use this mode automatically
;;;###autoload
(define-derived-mode wisp-mode
  emacs-lisp-mode "Wisp"
  "Major mode for whitespace-to-lisp files.

  \\{wisp-mode-map}"
  ;; :group wisp
  (set (make-local-variable 'indent-tabs-mode) nil)
  (setq comment-start ";")
  (setq comment-end "")
  ;; delimiters from https://docs.racket-lang.org/guide/symbols.html
  ;; ( ) [ ] { } " , ' ` ; # | \
  (setq imenu-generic-expression
    '((nil "^define\\(/contract\\)? +:? *\\([^[ \n(){}\",'`;#|\\\]+\\)" 2)))
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults) wisp-font-lock-keywords)
  (set (make-local-variable 'mode-require-final-newline) t)
  ;; bind keys to \r, not (kbd "<return>") to allow completion to work on RET
  (define-key wisp-mode-map (kbd "C-c i") '("imenu" . imenu))
  (define-key wisp-mode-map (kbd "<tab>") '("indent line" . wisp--tab))
  (define-key wisp-mode-map (kbd "<backtab>") '("unindent line" . wisp--backtab))
  (define-key wisp-mode-map "\r" '("wisp newline" . wisp--return))
  (define-key wisp-mode-map (kbd "C-M-x") '("eval block in *shell* buffer" . wisp--eval-block)))

; use this mode automatically
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.w\\'" . wisp-mode))
;;;###autoload
(add-hook 'wisp-mode-hook
          (lambda ()
            (setq electric-indent-inhibit t)))


(defcustom wisp--brighter-colors
  '(
    "#DDDDDD" "#eeeeee"
    "#BBCCEE" "#ccddff"
    "#CCEEFF" "#ddf3ff"
    "#CCDDAA" "#ddeebb"
    "#EEEEBB" "#ffffcc"
    "#FFCCCC" "#ffdddd"
    "#BCCCEE" "#cdddff"
    "#CDEEFF" "#def3ff"
    "#CDDDAA" "#deeebb"
    "#EFEEBB" "#ffffcd"
    "#FFCDCC" "#ffdedd"
    "#BBCDEE" "#ccdeff"
    "#CCEFFF" "#ddf5ff"
    "#CCDEAA" "#ddefbb"
    "#EEEFBB" "#ffffce"
    "#FFCCCD" "#ffddde"
    "#BBCCEF" "#ccdfff"
    "#CDEFFF" "#def3ff"
    "#CCDDAB" "#ddeebc"
    "#EEEEBC" "#ffffcf"
    "#FFCDCD" "#ffdede"
    "#BCCDEE" "#cddeff"
    "#CDEFFF" "#def5ff"
    "#CDDEAA" "#deefbb"
    "#EFEFBB" "#ffffd0"
    "#FFCECD" "#ffdede"
    )
  "Highlight-colors for the current level: First the matching color then the replacement."
  :group 'wisp
  :type 'plist)

(defcustom wisp--bg-colors
  '( ;; paul tol's pale scheme, the cycled ones become slightly
     ;; brighter to allow for identification of indentation level by
     ;; color.
    "#DDDDDD" ;; -1: . foo at toplevel
    "#BBCCEE"
    "#CCEEFF"
    "#CCDDAA"
    "#EEEEBB"
    "#FFCCCC"
    "#BCCCEE"
    "#CDEEFF"
    "#CDDDAA"
    "#EFEEBB"
    "#FFCDCC"
    "#BBCDEE"
    "#CCEFFF"
    "#CCDEAA"
    "#EEEFBB"
    "#FFCCCD"
    "#BBCCEF"
    "#CDEFFF"
    "#CCDDAB"
    "#EEEEBC"
    "#FFCDCD"
    "#BCCDEE"
    "#CDEFFF"
    "#CDDEAA"
    "#EFEFBB"
    "#FFCECD"
    )
  "Background-colors to show the indentation."
  :group 'wisp
  :type 'list)

(defun wisp--add-indentation-levels-before (indent levels)
  "Add the indentation level with INDENT or less to the LEVELS."
  (if (= 0 indent)
      levels
    (wisp--add-indentation-levels-before (wisp-prev-indent-lower-than indent) (+ levels 1))))

(defun wisp--current-indentation-level (indent)
  "Get the indentation level at the INDENT — the number of indentation levels defined before it."
  (wisp--add-indentation-levels-before indent 1))

(defvar-local wisp--highlight-indentation-overlays '()
  "Overlays set by wisp indentation highlighting in the current
  buffer.")

(defvar-local wisp--current-wisp-highlight-overlays-at-point '()
  "The overlay the point was in in the last time the
  wisp--highlight-current-indentation-level was called.")

(defvar-local wisp--original-colors-dynamic '()
  "The inverse of wisp--brighter-colors: the original-colors
  mapped too the brighter ones, filled when colors are
  replaced.")

(defvar-local wisp--current-highlight-brighter-color nil
  "The color of currently highlighted overlays.")


(defun wisp--highlight-indentation (&optional begin end length)
  "Colorize a buffer or the region between BEGIN and END up to LENGTH."
  (interactive)
  (wisp--highlight-indentation-region (point-min) (point-max)))

(defun wisp--find-begin-and-end-of-block-around-region (begin end)
  "Search around the current region (BEGIN and END) and return the wisp-block around it."
  (let ((begin (if (not begin)
                   (point-min)
                 begin))
        (end (if (not end)
                 (point-max)
               end)))
    (save-mark-and-excursion
      (goto-char begin)
      (backward-paragraph)
      (forward-line 1)
      ;; move backwards until the first line has indentation zero
      (while (not (= 0 (wisp--current-indent)))
        (forward-line -1)
        (backward-paragraph)
        (forward-line 1))
      (setq begin (point))
      (goto-char end)
      (forward-paragraph)
      ;; move forwards until the first line has indentation zero
      (forward-line 1)
      (while (not (= 0 (wisp--current-indent)))
        (forward-paragraph)
        (forward-line 1))
      (setq end (point))
      (goto-char begin))
    (cons begin end)))

(defun wisp--find-begin-and-end-of-lines-with-same-indentation (position)
  "Search around the current region and return the wisp-block around it."
  (save-mark-and-excursion
    (let* ((begin (if (not position)
                      (point)
                    position))
           (end begin))
      (goto-char position)
      (let ((indentation (wisp--current-indent)))
        (while (and (> (point) (point-min)) (equal indentation (wisp--current-indent)))
          (setq begin (point-at-bol))
          (forward-line -1))
        (forward-line 1)
        (while (and (< (point) (point-max)) (equal indentation (wisp--current-indent)))
          (setq end (point-at-eol))
          (forward-line 1))
        (cons begin end)))))




(defun wisp--find-begin-and-end-of-subtree (position)
  "Search around the current POSITION and return the wisp-block around it."
  (save-mark-and-excursion
    (let* ((begin (if (not position)
                      (point)
                    position))
           (end begin))
      (goto-char begin)
      (back-to-indentation)
      (let ((indentation (wisp--current-indent))
            (allow-same-indent (looking-at ". ")))
        (forward-line -1)
        (while (and (> (point) (point-min)) (if allow-same-indent (>= indentation (wisp--current-indent)) (> indentation (wisp--current-indent))))
          (setq begin (point-at-bol))
          (setq indentation (wisp--current-indent))
          (forward-line -1))
        (goto-char end)
        (forward-line 1)
        (while (and (< (point) (point-max)) (< indentation (wisp--current-indent)))
          (setq end (point-at-eol))
          (setq indentation (wisp--current-indent))
          (forward-line 1))
        (cons begin end)))))


(defun wisp--highlight-indentation-region (&optional begin end length)
  "Colorize a buffer or the region between BEGIN and END up to LENGTH."
  (interactive "r")
  (let* ((region (wisp--find-begin-and-end-of-block-around-region begin end))
         (begin (car region))
         (end (cdr region)))
    (save-mark-and-excursion
      (with-silent-modifications
        ;; change all affected blocks
        (goto-char begin)
        (backward-paragraph)
        (setq begin (point))
        (goto-char end)
        (forward-paragraph)
        (setq end (point))
        (goto-char begin)
        ;; delete our overlays that are fully inside the region, cut others short
        (mapc (lambda (overlay)
                (cond
                 ;; delete leftovers
                 ((not (overlay-start overlay))
                  (setq wisp--highlight-indentation-overlays
                        (delete overlay wisp--highlight-indentation-overlays)))
                 ;; delete all that touch
                 ((and (> (overlay-start overlay) begin)
                       (< (overlay-end overlay) end))
                  (delete-overlay overlay)
                  (setq wisp--highlight-indentation-overlays
                        (delete overlay wisp--highlight-indentation-overlays))))
                nil)
              wisp--highlight-indentation-overlays)
	    (while (< (point) end)
          (back-to-indentation)
	      (let* ((start (point))
                 (period (looking-at "\\. "))
                 (colon (looking-at ": "))
                 (empty-line (looking-at ": *$"))
                 (raw-level (wisp--current-indentation-level (wisp--current-indent)))
                 (level (if period (- raw-level 1) raw-level)))
	        (end-of-line)
            (let* ((line-end (point)))
              (back-to-indentation)
              (let ((overlay (make-overlay (point) line-end)))
                (push overlay wisp--highlight-indentation-overlays)
	            (overlay-put overlay
				             'face
				             `(:background
				               ,(nth level wisp--bg-colors)))
                (unless empty-line
                  (while (string-match ": " (buffer-substring (point) line-end))
                    (forward-char (match-beginning 0))
                    (when (null (nth 8 (syntax-ppss))) ;; not within string or comment
                      (let ((overlay (make-overlay (point) line-end)))
                        (push overlay wisp--highlight-indentation-overlays)
                        (setq level (+ level 1))
	                    (overlay-put overlay
				                     'face
				                     `(:background
				                       ,(nth level wisp--bg-colors)))
	                    (overlay-put overlay
                                     'priority
                                     level)))
                    (forward-char 1))))
              (forward-line 1))))))))

;;;###autoload
(define-minor-mode wisp-color-indentation-minor-mode
  "Mode to colorize the indentation level according to wisp-semanttics."
  nil nil nil
  :group 'wisp
  :after-hook (if wisp-color-indentation-minor-mode
                  (progn
                    (wisp--highlight-indentation)
                    (add-hook 'after-change-functions 'wisp--highlight-indentation-region nil t))
                (progn
                  (mapc 'delete-overlay wisp--highlight-indentation-overlays)
                  (mapc (lambda (o) (pop wisp--highlight-indentation-overlays))
                          wisp--highlight-indentation-overlays)
                  (remove-hook  'after-change-functions 'wisp--highlight-indentation-region t))))

;;;###autoload
(define-minor-mode wisp-color-highlight-current-indentation-minor-mode
  "Mode to colorize the indentation level according to wisp-semanttics. THIS IS A WORK IN PROGRESS."
  nil nil nil
  :group 'wisp
  :after-hook (if wisp-color-highlight-current-indentation-minor-mode
                  (progn
                    (wisp-color-indentation-minor-mode t)
                    (add-hook 'post-command-hook 'wisp--highlight-current-indentation-level nil t))
                (progn
                  (remove-hook  'post-command-hook 'wisp--highlight-current-indentation-level))))


;;;###autoload
(define-minor-mode wisp-color-highlight-current-subtree-minor-mode
  "Mode to colorize the indentation level according to wisp-semanttics. THIS IS A WORK IN PROGRESS."
  nil nil nil
  :group 'wisp
  :after-hook (if wisp-color-highlight-current-subtree-minor-mode
                  (progn
                    (wisp-color-indentation-minor-mode t)
                    (add-hook 'post-command-hook 'wisp--highlight-subtree nil t))
                (progn
                  (remove-hook  'post-command-hook 'wisp--highlight-subtree))))


(defun wisp--highlight-overlay-color (overlay)
  "Replace the background color of the OVERLAY with a lighter color from wisp--brighter-colors."
  (let ((color (plist-get (plist-get (overlay-properties overlay) 'face) :background)))
    (let ((new-color (lax-plist-get wisp--brighter-colors color)))
      (when new-color
        (unless (lax-plist-get wisp--original-colors-dynamic new-color)
          (setq wisp--original-colors-dynamic
                (lax-plist-put wisp--original-colors-dynamic new-color color)))
        (setq wisp--current-highlight-brighter-color new-color)
        (overlay-put overlay 'face `(:background ,new-color))))))

(defun wisp--overlay-background-color (overlay)
  (plist-get (plist-get (overlay-properties overlay) 'face) :background))

(defun wisp--restore-overlay-color (overlay)
  "Replace the background color of the OVERLAY with its original color from wisp--brighter-colors."
  (let ((color (wisp--overlay-background-color overlay)))
    (let ((original-color (lax-plist-get wisp--original-colors-dynamic color)))
      (when original-color
        (overlay-put overlay 'face `(:background ,original-color))))))

(defun wisp--highlight-current-indentation-level ()
  "Highlight the current indentation level by using a brighter color.

If the var wisp--brighter-colors defines a color for the current
color of the overlay, the mapped color is set instead."
  (interactive)
  (with-silent-modifications
    (let* ((overlays-with-bg-at-point
            (remove-if-not
             (lambda (overlay)
               (let ((color (wisp--overlay-background-color overlay)))
                 (or (lax-plist-get wisp--brighter-colors color)
                     (lax-plist-get wisp--original-colors-dynamic color))))
             (overlays-at (point) t)))
           (wisp-overlay-at-point
            (if overlays-with-bg-at-point
                (list (car overlays-with-bg-at-point))
              (list)))
           (highlighted-overlays (mapc 'wisp--highlight-overlay-color wisp-overlay-at-point))
           (region (wisp--find-begin-and-end-of-block-around-region (point) (point)))
           (begin (car region))
           (end (cdr region))
           (overlays-in-region (overlays-in begin end))
           (current-highlighting-color
            (lax-plist-get wisp--original-colors-dynamic
                           wisp--current-highlight-brighter-color))
           (overlays-with-same-color
            (remove-if-not
             (lambda (overlay)
               (equalp current-highlighting-color
                       (wisp--overlay-background-color overlay)))
             overlays-in-region))
           (removed-overlays
            (remove-if
             (lambda (overlay)
               (equalp wisp--current-highlight-brighter-color
                       (wisp--overlay-background-color overlay)))
             wisp--current-wisp-highlight-overlays-at-point)))
      ;; restore all no longer highlighted overlays
      (mapc 'wisp--restore-overlay-color removed-overlays)
      ;; remember all highlighted overlays
      (setq wisp--current-wisp-highlight-overlays-at-point
            (delete-dups
             (append
              wisp--current-wisp-highlight-overlays-at-point
              highlighted-overlays
              (mapc 'wisp--highlight-overlay-color overlays-with-same-color))))
      ;; remove no longer highlighted overlays
      (dolist (overlay removed-overlays)
        (setq wisp--current-wisp-highlight-overlays-at-point
              (delq overlay wisp--current-wisp-highlight-overlays-at-point))))))


(defun wisp--highlight-subtree ()
  "Highlight the current indentation level by using a brighter color.

If the var wisp--brighter-colors defines a color for the current
color of the overlay, the mapped color is set instead."
  (interactive)
  (with-silent-modifications
    (let* ((overlays-with-bg-at-point
            (remove-if-not
             (lambda (overlay)
               (let ((color (wisp--overlay-background-color overlay)))
                 (or (lax-plist-get wisp--brighter-colors color)
                     (lax-plist-get wisp--original-colors-dynamic color))))
             (overlays-at (point) t)))
           (wisp-overlay-at-point
            (if overlays-with-bg-at-point
                (list (car overlays-with-bg-at-point))
              (list)))
           (highlighted-overlays (mapc 'wisp--highlight-overlay-color wisp-overlay-at-point))
           (region (wisp--find-begin-and-end-of-subtree (point)))
           (begin (car region))
           (end (cdr region))
           (overlays-in-region (overlays-in begin end))
           (current-highlighting-color
            (lax-plist-get wisp--original-colors-dynamic
                           wisp--current-highlight-brighter-color))
           (overlays-with-same-color
            (remove-if-not
             (lambda (overlay)
               (equalp current-highlighting-color
                       (wisp--overlay-background-color overlay)))
             overlays-in-region))
           (removed-overlays
            (remove-if
             (lambda (overlay)
               (equalp wisp--current-highlight-brighter-color
                       (wisp--overlay-background-color overlay)))
             wisp--current-wisp-highlight-overlays-at-point)))
      ;; restore all no longer highlighted overlays
      (mapc 'wisp--restore-overlay-color removed-overlays)
      ;; remember all highlighted overlays
      (setq wisp--current-wisp-highlight-overlays-at-point
            (delete-dups
             (append
              wisp--current-wisp-highlight-overlays-at-point
              highlighted-overlays
              (mapc 'wisp--highlight-overlay-color overlays-with-same-color))))
      ;; remove no longer highlighted overlays
      (dolist (overlay removed-overlays)
        (setq wisp--current-wisp-highlight-overlays-at-point
              (delq overlay wisp--current-wisp-highlight-overlays-at-point))))))


;; (add-hook 'post-command-hook 'wisp--highlight-current-indentation-level nil t)


(defun wisp--wisp2lisp ()
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (save-excursion
      (get-buffer-create "*wisp2lisp*")
      (set-buffer "*wisp2lisp*")
      (erase-buffer)
      (scheme-mode))
    (call-process "wisp2lisp" nil "*wisp2lisp*" nil (buffer-file-name))
    (when (called-interactively-p)
      (switch-to-buffer-other-window "*wisp2lisp*")
      (beginning-of-buffer)
      (forward-line (- current-line 1)))))

(define-key wisp-mode-map (kbd "C-c C-w") 'wisp--wisp2lisp)

(defun wisp--eval-with-geiser ()
  (interactive)
  (require 'geiser)
  (wisp--wisp2lisp)
  (save-excursion
    (set-buffer "*wisp2lisp*")
    (run-geiser 'guile)
    (geiser-eval-buffer)))

(define-key wisp-mode-map (kbd "C-c C-b") 'wisp--eval-with-geiser)



(provide 'wisp-mode)
;;; wisp-mode.el ends here
