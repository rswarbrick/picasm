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

(defcustom picasm-instruction-indent-spaces 6
  "Number of spaces to indent instruction lines"
  :type 'integer :group 'picasm)

(defcustom picasm-section-marker-indent-spaces 8
  "Number of spaces to indent section markers"
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-argument-indent-tabs 2
  "Number of tabs to insert after instructions, before arguments"
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-comment-indent-tabs 2
  "Number of tabs to indent comments"
  :type 'integer :group 'picasm)

(defcustom picasm-require-comment t
  "Whether to require a comment on every line (even if empty)"
  :type 'boolean :group 'picasm)

(defun strip-trailing-whitespace ()
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defun picasm-mode-indent-instruction-line ()
  "Indent an instruction"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (cond ((looking-at "^[ \t]*\\(UDATA\\(?:_SHR\\)?\\|CODE\\|__CONFIG\\|END\\)")
	   (progn
	     (indent-line-to picasm-section-marker-indent-spaces)
	     (end-of-line)))
	  ((looking-at "^[[:alnum:]_]:?")   ; label
	   (progn
	     (indent-line-to 0)
	     (end-of-line)))
	  ((looking-at "^\s*$")   ; line is empty, assume we want to enter an ins
	   (indent-line-to picasm-instruction-indent-spaces))
	  ; instruction, but no arg. advance to argument position.
	  ((looking-at "^[ \t]+[[:alpha:]]+$")
	   (progn 
	     (end-of-line)
	     (dotimes (i picasm-instruction-argument-indent-tabs)
	       (insert "\t"))))
	  ; at argument position, erase any tabs and re-tabify
	  ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+$")
	   (progn
	     (strip-trailing-whitespace)
	     (forward-word 1)
	     (save-excursion
	       (dotimes (i picasm-instruction-argument-indent-tabs)
		 (insert "\t")))
	     (end-of-line)))
	  ; complete instruction/argument pair. re-indent and leave point at eol or comment if enabled.
	  ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]*$")
	   (progn
	     (strip-trailing-whitespace)
	     (end-of-line)
	     (if picasm-require-comment
		 (progn
		   (dotimes (i picasm-instruction-comment-indent-tabs)
		     (insert "\t"))
		   (insert "; "))
	       (progn
		 (indent-line-to picasm-instruction-indent-spaces)
		 (end-of-line)))))
	  ;; complete instruction/argument pair with comment; leave point at eol
	  ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]+;.*$")
	   (end-of-line))
	  ((looking-at (concat "^[ \t]*" picasm-mode-font-lock-syntheticop-keyword-re))
	   (progn
	     (indent-line-to picasm-instruction-indent-spaces)
	     (end-of-line)))
	  ((looking-at (concat "[ \t]*" picasm-mode-pp-directive-re))
	   (progn
	     (indent-line-to 0)
	     (end-of-line)))
	  (t (message "don't know how to indent this line")))))
    	
(defun picasm-electric-comment ()
  "Insert a comment at EOL, move point to it. If there is already a comment there, move point to it. Otherwise, insert a semicolon."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (cond ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]*$")
	   (progn
	     (strip-trailing-whitespace)
	     (end-of-line)
	     (dotimes (i picasm-instruction-comment-indent-tabs)
	       (insert "\t"))
	     (insert "; ")))
	  ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]+;.*$")
	   (end-of-line))
	  (t (progn
	       (goto-char p)
	       (insert ";"))))))
      
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
  (define-key picasm-mode-map "\t" 'picasm-mode-indent-instruction-line)
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
					     (picasm-mode-indent-instruction-line))))

(defcustom picasm-mode-hook nil
  "Hook run when picasm-mode is initialized"
  :type 'hook :group 'picasm)

(defun picasm-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table picasm-mode-syntax-table)
  (use-local-map picasm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(picasm-mode-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'picasm-mode-indent-instruction-line)
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


