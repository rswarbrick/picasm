;; Interfaces to external programs and output parsers.

;; Running *asm uses compilation mode.
(require 'compile)

(defcustom picasm-assembler-program 'gpasm
  "Select either GPASM or MPASMX to assemble source files"
  :type 'symbol
  :options '(gpasm mpasmx)
  :group 'picasm-external)

(defcustom picasm-gpasm-program "gpasm"
  "Location of the gpasm executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-gplink-program "gplink"
  "Location of the gplink executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-mpasmx-program "/opt/microchip/mplabx/mpasmx/mpasmx"
  "Location of the mpasmx executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-mplinkx-program "/opt/microchip/mplabx/mpasmx/mplink"
  "Location of the (native) mplink executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-output-format "inhx32" 
  "Output format for HEX files"
  :options '("inhx8m" "inhx8s" "inhx16" "inhx32") :group 'picasm-external)

(defcustom picasm-includes (list ".")
  "List of include directories"
  :type '(list string) :group 'picasm-external)

(defcustom picasm-default-radix "HEX"
  "Default radix for assembling files"
  :options '("BIN" "DEC" "OCT" "HEX") :group 'picasm-external)

(defcustom picasm-pk2cmd-program "/usr/local/bin/pk2cmd"
  "Location of the pk2cmd executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-show-assembler-output nil
  "Whether to display assembler output in a new window"
  :type 'boolean :group 'picasm)

;; Defined in picasm.el, but redeclare here to shut up the byte compiler.
(defvar picasm-chip-select)

(defun picasm-stripped-chip-name (chip-name)
  "Return an lower case version of `CHIP-NAME' with any prefix of
P or PIC stripped away. MPASMX expects this and GPASM doesn't
mind."
  (downcase
   (cond
    ((string= "PIC" (upcase (substring chip-name 0 3)))
     (substring chip-name 3))
    ((char-equal ?P (upcase (aref chip-name 0)))
     (substring chip-name 1))
    (t chip-name))))

(defvar picasm-object-file-directives
  '("UDATA" ".ResetVector")
  "A list of directives that only occur if we should generate an
object file (rather than absolute hex)")

(defun picasm-make-object-file-p ()
  (catch 'pmofp
    (dolist (directive picasm-object-file-directives nil)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp
               (format "^[[:space:]]*%s" directive) nil t)
          (throw 'pmofp t))))))

(defun picasm-assembler-case-funcall (assoc &rest args)
  "Run the correct function based on assoc, depending on the
value of `picasm-assembler-program'. Called with `ARGS'."
  (let ((hit (assq picasm-assembler-program assoc)))
    (unless hit
      (error "No match for picasm-assembler-program '%s'."
             picasm-assembler-program))
    (eval (cons (second hit) args))))

(defun picasm-gpasm-command-pieces (file chip)
  "See `picasm-assemble-command-pieces'."
  `(,picasm-gpasm-program
    ,@(let ((acc))
        (dolist (dir picasm-includes acc)
          (push dir acc) (push "-I" acc)))
    "-p" ,(picasm-stripped-chip-name chip)
    "-r" ,picasm-default-radix "-c" ,file))

;; MPASMX demonstrates how well designed it is by
;;  (1) Not making use of exit status for error
;;  (2) Not outputting any warnings or errors to the console
;;  (3) Redirecting the listfile to stderr? Why would you every want to do that?
;;
;; As such, we'll use a harness for it that assumes a Posix shell and does funky
;; stuff with temporary files to get what one might actually expect. Joy.
(defun picasm-mpasmx-command-pieces (file chip)
  "See `picasm-assemble-command-pieces'."
  (list
   (expand-file-name "mpasmx-harness.sh"
                     (file-name-directory
                      (symbol-file 'picasm-mpasmx-command-pieces)))
   picasm-mpasmx-program
   file
   (concat (file-name-sans-extension file) ".o")
   (picasm-stripped-chip-name chip)
   picasm-default-radix
   picasm-output-format
   (if (picasm-make-object-file-p) "false" "true")))

;; Compilation is based on Emacs' compilation infrastructure. We provide an
;; explicit compile-command to run the assembler correctly. Since
;; compile-command is actually a shell command (eugh!), assemble it using
;; combine-and-quote-strings. Also bind a compilation error regex to spot
;; MPLABX-style error messages. (It would be lovely to do this as a dynamic
;; variable, but fontification happens after assemble-file has finished).
(defun picasm-assemble ()
  (interactive)
  (unless (assq 'picasm compilation-error-regexp-alist-alist)
    (push
     `(picasm
       ,(concat
         "^\\(?:\\(Error\\)\\|\\(Warning\\)\\)\\[[0-9]+\\][[:space:]]+"
         "\\(.+?\\)[[:space:]]+\\([0-9]+\\)"
         "[[:space:]]+:[[:space:]]+\\(.*\\)")
       3 4 nil (2 . 1) 5)
     compilation-error-regexp-alist-alist)
    (pushnew 'picasm compilation-error-regexp-alist))
  (compile
   (combine-and-quote-strings
    (mapcar #'shell-quote-argument
            (picasm-assembler-case-funcall
             '((mpasmx picasm-mpasmx-command-pieces)
               (gpasm  picasm-gpasm-command-pieces))
             (buffer-file-name (current-buffer))
             picasm-chip-select)))))

(defun picasm-gplink-command-pieces (o-file hex-file chip)
  "See `picasm-link-command-pieces'."
  (list picasm-gplink-program "-o" hex-file "-a" picasm-output-format o-file))

(defun picasm-find-mplink-script (chip)
  "Search for a LKR directory in the directory containing the
mplink program and then look inside it for an appropriate linker script."
  (let ((dir (concat (file-name-directory picasm-mplinkx-program) "/LKR"))
        (short-chip (picasm-stripped-chip-name chip)))
    (unless (file-directory-p dir)
      (error "Can't find a directory at %s (containing linker scripts)" dir))
    (let ((hits (directory-files dir t (concat "^" short-chip ".*\\.lkr$"))))
      (unless hits
        (error "Can't find a linker script for chip %s in %s."
               short-chip dir))
      (unless (= 1 (length hits))
        (error "Found too many matching linker scripts: %s" hits))
      (first hits))))

(defun picasm-mplink-command-pieces (o-file hex-file chip)
  "See `picasm-link-command-pieces'."
  (list picasm-mplinkx-program (picasm-find-mplink-script chip) o-file
        (concat "-o" hex-file)
        (concat "-a" (upcase picasm-output-format))
        (concat "-p" (picasm-stripped-chip-name chip)) "-d"))

(defun picasm-link (&optional filename)
  "Run the final link stage to generate the HEX file from an object file"
  (interactive)
  (unless filename
    (let ((asm-name (buffer-file-name)))
      (unless asm-name
        (error "Cannot find a filename to link (buffer has no filename)."))
      (setq filename (concat (file-name-sans-extension asm-name) ".hex")))
    (let ((o-file (concat (file-name-sans-extension filename) ".o"))
          (hex-file (concat (file-name-sans-extension filename) ".hex")))
      (unless (file-exists-p o-file)
        (error "The object file %s doesn't exist." o-file))
      (compile
       (combine-and-quote-strings
        (mapcar #'shell-quote-argument
                (picasm-assembler-case-funcall
                 '((mpasmx picasm-mplink-command-pieces)
                   (gpasm  picasm-gplink-command-pieces))
                 o-file hex-file picasm-chip-select)))))))


(defun picasm-insert-delay (label seconds clock-mhz)
  "Insert a routine at point, using LABEL as a name component,
that will cause a delay of SECONDS length assuming a clock
running at CLOCK-MHZ."
  (interactive "Mlabel: \nnSeconds: \nnClock (MHz): ")
  (let ((counters (picasm-calculate-picloops seconds clock-mhz)))
    (cond ((= (length counters) 1)
           (apply #'picloops-loop-1 label counters))
          ((= (length counters) 2)
           (apply #'picloops-loop-2 label counters))
          (t
           (apply #'picloops-loop-3 label counters)))))

;; Interface to the pk2cmd command-line PIC programmer

(defun picasm-pk2cmd-erase ()
  (interactive)
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
				    (list "-E")))))
    (if (stringp (string-match "Succeeded" output))
	(message "chip erased"))))

(defun picasm-pk2cmd-read ()
  ; Read the entire PIC, regardless of how much program mem it has
  (interactive)
  (compare-chips)
  (if (not (string-equal (buffer-name (current-buffer)) "*Chip Contents*"))
      (progn
	(split-window-vertically)
	(other-window 1)))
  (if (get-buffer "*Chip Contents*")
	(switch-to-buffer "*Chip Contents*")
    (switch-to-buffer (get-buffer-create "*Chip Contents*")))
  (erase-buffer)
  (let ((output
	 (run-pk2cmd (append (list "-P" picasm-chip-select)
			     (list "-GP" "0-FFFFFFF")))))
    (dolist (line (split-string output "\n"))
      (if (string-match "^[[:digit:]A-Fa-f]" line)
	  (insert (concat line "\n"))))))

(defun picasm-pk2cmd-verify (file)
  (interactive "fVerify file (HEX format): ")
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
				    (list (concat "-F" (expand-file-name file)))   ; space causes problems (??)
				    (list "-Y")))))
    (if (string-match "Verify Succeeded." output)
	(message "Chip verify succeeded")
      (message "Chip verify failed"))))

(defun picasm-pk2cmd-program (file)
  (interactive "fProgram file (HEX format): ")
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
				    (list (concat "-F" (expand-file-name file)))
				    (list "-MP")))))
    (if (string-match "Program Succeeded." output)
	(message "Chip progamming succeeded")
      (message "Chip programming failed"))))

(defun run-pk2cmd (args)
  (shell-command-to-string (mapconcat #'(lambda (x) x) `(,picasm-pk2cmd-program ,@args) " ")))
  
(defun get-programmer-chip ()
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
				    (list "-I")))))
    (string-match "Device Name = \\([^ \t\n]+\\)" output)
    (match-string-no-properties 1 output)))

(defun compare-chips ()
  (let ((phy-dev (get-programmer-chip)))
    (if (not (string-equal phy-dev picasm-chip-select))
	(error (format "Chip selected does not match programmer (Have %s, want %s)." phy-dev picasm-chip-select))
      t)))
      

(provide 'picasm-external)
