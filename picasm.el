;; PIC ASM editing mode
;; supports PIC10, PIC12 and PIC16 instruction sets
;;
;; Daniel Debertin <dan@mapcar.org>
;;
;; Features:
;;
;; Syntax highlighting
;; Indentation
;; Electric comment
;; Bundled PIC chip database
;; Assembler/Linker support for GNU gputils and Microchip's MPASM
;; Programmer support for the pk2cmd program
;; Delay loop calculation and code generation

(require 'picasm-external)
(require 'picasm-loops)
(require 'picasm-db)

(defconst picasm-mode-synthetic-instructions
  '("pagesel"
    "banksel"))

(defconst picasm-mode-font-lock-instruction-re
  "\\<\\(?:A\\(?:DD\\(?:LW\\|WF\\)\\|ND\\(?:LW\\|WF\\)\\)\\|B\\(?:CF\\|SF\\|TFS[CS]\\)\\|C\\(?:ALL\\|LR\\(?:WDT\\|[FW]\\)\\|OMF\\)\\|DECF\\(?:SZ\\)?\\|GOTO\\|I\\(?:NCF\\(?:SZ\\)?\\|OR\\(?:LW\\|WF\\)\\)\\|MOV\\(?:F\\|LW\\|WF\\)\\|NOP\\|R\\(?:ET\\(?:FIE\\|LW\\|URN\\)\\|[LR]F\\)\\|S\\(?:LEEP\\|UB\\(?:LW\\|WF\\)\\|WAPF\\)\\|XOR\\(?:LW\\|WF\\)\\|\\(?:BANK\\|PAGE\\)SEL\\)\\>")


; This RE picks up the canonical GPASM number syntaxes as well as legacy MPASM syntaxes for binary,
; octal, decimal and hexadecimal number literals.
(defconst picasm-mode-number-literal-re
  "\\(?:[bB]'[01]+'\\|[oO]'[0-7]+'\\|[dD]'[0-9]+'\\|[Hh]'[0-9A-Fa-f]+'\\|0[xX][0-9A-Fa-f]+\\|[01]+[Bb]\\|[Qq]'[0-7]+'\\|[0-7]+[oO]\\|[0-7]+[Qq]\\|[0-9]+[Dd]\\|\\.[0-9]+\\|[0-9A-Fa-f]+[Hh]\\)")

(defconst picasm-mode-pp-directive-re
  "\\(list\\|equ\\|constant\\|res\\|MACRO\\|ENDM\\|#\\(?:include\\|define\\|if\\|else\\|endif\\|ifn?def\\)\\|__CONFIG\\)")

(defconst picasm-mode-section-marker-re
  "\\(?:UDATA\\(?:_SHR\\)?\\|CODE\\|END\\)")

(defconst picasm-mode-identifier-re
  "[[:alnum:]_,<>]+")

(defconst picasm-mode-font-lock-syntheticop-keyword-re
  "\\<\\(?:\\(?:BANK\\|PAGE\\)SEL\\)\\>")

(defconst picasm-mode-font-lock-keywords
  (list `(,picasm-mode-font-lock-instruction-re . font-lock-keyword-face)
 	`(,picasm-mode-font-lock-syntheticop-keyword-re . font-lock-builtin-face)
 	`(,picasm-mode-number-literal-re . font-lock-constant-face)
 	`(,picasm-mode-pp-directive-re . font-lock-preprocessor-face)
 	`(,picasm-mode-section-marker-re . font-lock-keyword-face)
 	`(,picasm-mode-identifier-re . font-lock-variable-name-face)))

(defcustom picasm-instruction-column 6
  "Number of spaces to indent instruction lines"
  :type 'integer :group 'picasm)

(defcustom picasm-argument-column 14
  "The column to which to indent arguments after instructions."
  :type 'integer :group 'picasm)

(defcustom picasm-left-comment-column 6
  "Column to where comment lines (without instructions) should be indented."
  :type 'integer :group 'picasm)

(defcustom picasm-section-marker-column 8
  "Number of spaces to indent section markers"
  :type 'integer :group 'picasm)

(defcustom picasm-right-comment-column 40
  "Column where comments start after instructions."
  :type 'integer :group 'picasm)

(defcustom picasm-require-comment t
  "Whether to require a comment on every line (even if empty)"
  :type 'boolean :group 'picasm)

(defun strip-trailing-whitespace ()
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defmacro picasm-looking-at-cond (&rest clauses)
  "A macro that runs the CDRs of `clauses', depending on whether
we are `looking-at' something matching their first elements. If a
clause is keyed with true, it always matches."
  (cons 'cond
        (mapcar (lambda (clause)
                  `(,(or (eq (car clause) t)
                         `(looking-at ,(car clause)))
                    ,@(cdr clause))) clauses)))

(defun picasm-line-pieces ()
  "Analyse a line that is being indented. Returns a list of
triples (FROM TO STR), where FROM is the column that STR
currently starts at, TO is a (minimum) column number to put it at
and STR is the string to place. When point is at the end of line
and not yet in a comment, it adds one more empty or '; ' STR at
the next correct tab stop column."
  (save-excursion
    ;; The following regex for the comment picks up any trailing space before
    ;; the semicolon (but ignores it), and keeps trailing whitespace. To use it,
    ;; make sure the previous regex has a non-greedy match for characters
    ;; including [:space:] as its last part. This match will pick up trailing
    ;; whitespace if there isn't a comment, but not if there is.
    (let ((comment-regex "\\(?:[[:space:]]*\\(;.*?\\)\\)?$")
          (bol (point))
          (eolp (looking-at "[[:space:]]*$")))
      (beginning-of-line)
      (picasm-looking-at-cond
       ((concat "[[:space:]]*"
                "\\(\\(?:UDATA\\(?:_SHR\\)?\\|CODE\\|__CONFIG\\|END\\).*?\\)"
                comment-regex)
        (cons (list (- (match-beginning 1) bol)
                    picasm-section-marker-column (match-string 1))
              (if (match-beginning 2)
                  (list (list (- (match-beginning 2) bol)
                              picasm-right-comment-column (match-string 2)))
                (when eolp
                  (list (list (- (line-end-position) bol)
                              picasm-right-comment-column "; "))))))
       ;; A label has no indent and is alphanumeric, although it might end with
       ;; a colon. Grab the whole thing and "indent it to 0" (not that this will
       ;; actually do anything), and deal with a trailing comment as usual.
       ((concat "\\([[:alnum:]_]+:?[[:space:]]*?\\)" comment-regex)
        (cons (list (- (match-beginning 1) bol) 0 (match-string 1))
              (if (match-beginning 2)
                  (list (list (- (match-beginning 2) bol)
                              picasm-right-comment-column (match-string 2)))
                (when eolp
                  (list (list (- (line-end-position) bol)
                              picasm-right-comment-column "; "))))))
       ;; A comment line starts with a colon.
       ("[[:space:]]*\\(;.*\\)"
        (list (list (- (match-beginning 1) bol)
                    picasm-left-comment-column (match-string 1))))
       ;; This matches an instruction, which must have a nonzero indent, then
       ;; possibly arguments consisting of alnum, _, - (for ERRORLEVEL commands),
       ;; commas and apostrophes (for b'xxx'), then some space and an optional
       ;; comment.
       ((concat "^[[:space:]]+\\([[:alnum:]_]+[:space:]*?\\)"
                "\\(?:[[:space:]]+\\([[:alnum:]_[:space:]'-,]*?\\)\\)?"
                comment-regex)
        (cons (list (- (match-beginning 1) bol)
                    picasm-instruction-column (match-string 1))
              (append
               (when (and (match-beginning 2) (< 0 (length (match-string 2))))
                 (list (list (- (match-beginning 2) bol)
                             picasm-argument-column (match-string 2))))
               (when (match-beginning 3)
                 (list (list (- (match-beginning 3) bol)
                             picasm-right-comment-column (match-string 3))))
               (when (and eolp (not (match-beginning 3)))
                 (if (not (match-beginning 2))
                     (list (list (- (line-end-position) bol)
                                 picasm-argument-column ""))
                   (list (list (- (line-end-position) bol)
                               picasm-right-comment-column "; ")))))))
       ;; An empty line is treated as an instruction-in-waiting...
       ("[[:space:]]*$" `((0 ,picasm-instruction-column "")))
       ;; I don't know anything about this line, so grab the whole lot and
       ;; indent it to zero (won't do anything).
       (".*"
        `((0 0 ,(match-string 0))))))))

(defun picasm-indent-line ()
  "Indent a line correctly for PICASM-MODE. The line is analysed
and split apart into strings that should appear starting at
various columns (see variables like `picasm-instruction-column'),
then these pieces are inserted, hopefully not messing up the
position of the cursor."
  (interactive)
  (let ((pieces (picasm-line-pieces))
        (point-col (current-column))
        (new-col nil)
        (previous-piece nil) (previous-insertion-col nil))
    ;; Clear the current line
    (delete-region (line-beginning-position) (line-end-position))
    ;; Insert the pieces, shifting COL if necessary.
    (dolist (piece pieces)
      ;; PIECE is (OLD-COL NEW-COL STR)
      (when (> (second piece) 0)
        (indent-to (second piece) (unless (looking-back " ") 1)))
      (when (and (not new-col) (< point-col (first piece)))
        ;; This is the first piece that follows the old point. Since I know I've
        ;; inserted enough stuff now, update CURRENT-COLUMN to the correct
        ;; position. If point is before the first insert, it should be jumped
        ;; forward.
        (setq new-col
              (if previous-piece
                  (min (current-column)
                       (+ (- point-col (first previous-piece))
                          previous-insertion-col))
                (current-column))))
      (setq previous-insertion-col (current-column)
            previous-piece piece)
      (insert (third piece)))
    (unless new-col
      (setq new-col
            (min (current-column)
                 (+ (- point-col (first previous-piece))
                    previous-insertion-col))))
    ;; Move point to the correct column
    (goto-char (+ (line-beginning-position) new-col))))

(defun picasm-electric-comment (arg)
  "Insert a comment at EOL, move point to it. If there is already
a comment there, move point to it. Otherwise, insert a
semicolon. The prefix arg can be used to insert multiple
semicolons like self-insert-command. Comments after the text are
placed in `picasm-right-comment-column'."
  (interactive "p")
  (let ((p (point)))
    (beginning-of-line)
    (cond
     ((looking-at "[ \t]*\\([[:alpha:]]\\)[^;]*$")
      ;; There is code in this line, but no comment. If ';' was hit before
      ;; anything non-whitespace in the line, the user probably just wants to
      ;; comment out the line, so we let him or her do so. Otherwise, jump to
      ;; the comment column at the end of the line.
      (if (<= p (match-beginning 1))
          (dotimes (k arg) (insert ";"))
        (strip-trailing-whitespace)
        (end-of-line)
        (dotimes (i (- picasm-right-comment-column (current-column)))
          (insert " "))
        (dotimes (k arg) (insert ";"))
        (insert " ")))
     ;; There is a comment in this line already. If there is also non-comment
     ;; text, and point is before the start of that or if point is after the
     ;; first semicolon, then just insert the semicolon. Otherwise, jump to the
     ;; start of the comment if we haven't got there yet.
     ((looking-at "[[:space:]]*\\([^;]?\\).*?\\(;+\\)[ ]*")
      (cond
       ((or (and (not (string= (match-string 1) ""))
                 (< p (match-beginning 1)))
            (> p (match-beginning 2)))
        (goto-char p)
        (dotimes (k arg) (insert ";")))
       (t
        (goto-char (match-end 0)))))
     ;; Otherwise, just insert the point as requested
     (t
      (goto-char p)
      (dotimes (k arg) (insert ";"))))))

(defvar picasm-chip-select "PIC16F84A")

(defvar picasm-mode-syntax-table 
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\; "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    tab))

(defvar picasm-mode-map (make-keymap))

(defcustom picasm-use-default-keybindings t
  "Whether to assume you want to use my keybindings (recommended!)"
  :type 'boolean :group 'picasm)

(defun picasm-setup-default-keybindings ()
  (define-key picasm-mode-map ";" 'picasm-electric-comment)
  (define-key picasm-mode-map "\C-c\C-c" 'picasm-assemble)
  (define-key picasm-mode-map "\C-c\C-l" 'picasm-link)
  (define-key picasm-mode-map "\C-c\C-p" 'picasm-pk2cmd-program)
  (define-key picasm-mode-map "\C-c\C-e" 'picasm-pk2cmd-erase)
  (define-key picasm-mode-map "\C-c\C-v" 'picasm-pk2cmd-verify)
  (define-key picasm-mode-map "\C-c\C-r" 'picasm-pk2cmd-read)
  (define-key picasm-mode-map [134217741] '(lambda () 
					     (interactive)
					     (end-of-line)
					     (newline)
					     (picasm-indent-line))))

(defcustom picasm-mode-hook nil
  "Hook run when picasm-mode is initialized"
  :type 'hook :group 'picasm)

(defun picasm-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table picasm-mode-syntax-table)
  (use-local-map picasm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(picasm-mode-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'picasm-indent-line)
  (setq major-mode 'picasm-mode)
  (setq mode-name (format "PICasm [%s]" picasm-chip-select))
  (set (make-local-variable 'font-lock-keywords)
       '(picasm-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) ";")
  (if picasm-use-default-keybindings
      (picasm-setup-default-keybindings))
  (unless picasm-chip-db (picasm-chip-read-db))
  (picasm-select-chip)
  (run-hooks 'picasm-mode-hook))

(defun picasm-guess-chip ()
  "Try to guess the chip used, given the current buffer. Looks
for lines like #include <pXXXX.inc>"
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#include[[:blank:]]+<p\\([[:alnum:]]+\\)" nil t)
      (concat "PIC" (upcase (match-string 1))))))

(defun picasm-select-chip ()
  (interactive)
  (setq picasm-chip-select 
        (or (picasm-guess-chip)
            (upcase (read-string "Select chip: "))))
  (setq mode-name (format "PICasm [%s]" picasm-chip-select))
  (force-mode-line-update))

(provide 'picasm)


