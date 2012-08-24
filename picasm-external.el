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

(defcustom picasm-pk2cmd-program "pk2cmd"
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

(defun picasm-picced-chip-name (chip-name)
  "Return an upper case version of `CHIP-NAME', prefixed by PIC
if it starts with a 1. (So, for example p16f1824 => PIC16F1824
and 12F510 => PIC12F510 but dsPIC33FJ12MC201 => DSPIC33FJ12MC201)"
  (let ((stripped (picasm-stripped-chip-name chip-name)))
    (upcase
     (if (char-equal ?1 (aref stripped 0))
         (concat "PIC" stripped)
       stripped))))

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

(defun picasm-run-pk2cmd (&rest args)
  "Run pk2cmd with the given `args', and also a -P argument for
the chip given by `picasm-chip-select'."
  (with-temp-buffer
    (apply 'call-process picasm-pk2cmd-program nil (current-buffer) nil
           (concat "-P" (picasm-picced-chip-name picasm-chip-select)) args)
    (buffer-string)))
  
(defun picasm-get-programmer-chip ()
  "Run pk2cmd to find any PIC currently attached. Raises an error
if there isn't a PicKit2 available."
  (let ((output
         (picasm-run-pk2cmd
          (concat "-P" (picasm-picced-chip-name picasm-chip-select)) "-I")))
    (if (string-match "Device Name = \\([^ \t\n]+\\)" output)
        (match-string-no-properties 1 output)
      (error "Couldn't find an attached PicKit2."))))

(defun picasm-compare-chips ()
  "Raises an error if there isn't a PIC attached to the computer
matching the value of `picasm-chip-select'."
  (let ((phy-dev (picasm-get-programmer-chip))
        (guess (picasm-picced-chip-name picasm-chip-select)))
    (unless (string-equal phy-dev guess)
      (error "Chip selected does not match programmer (Have %s, want %s)."
             phy-dev guess))))

(defun picasm-pk2cmd-erase ()
  "Erase the attached PIC."
  (interactive)
  (picasm-compare-chips)
  (when (string-match "Succeeded" (picasm-run-pk2cmd "-E"))
    (message "Chip erased.")))

(defun picasm-pk2cmd-read ()
  "Read the entire contents of the attached PIC, displaying the
results in a new buffer named *Chip Contents*."
  (interactive)
  (picasm-compare-chips)
  (unless (string-equal (buffer-name (current-buffer)) "*Chip Contents*")
    (switch-to-buffer-other-window (get-buffer-create "*Chip Contents*"))
    (erase-buffer)
    (dolist (line (split-string (picasm-run-pk2cmd "-GP" "0-FFFFFFF") "\n"))
      (if (string-match "^[[:digit:]A-Fa-f]" line)
	  (insert (concat line "\n"))))))

(defun picasm-find-hex-file ()
  "Find a hex file from the current buffer, by replacing the
suffix of the current buffer's filename with .hex and trying
that. Raises an error if the resulting file doesn't exist."
  (let ((filename
         (concat (file-name-sans-extension (or (buffer-file-name) ""))
                 ".hex")))
    (unless (file-readable-p filename)
      (error "Hex file '%s' cannot be read." filename))
    filename))

(defun picasm-pk2cmd-verify (&optional filename)
  "Verify the contents of the attached chip match a hex file. If
`filename' is given, use that to check against. Otherwise tries
`picasm-find-hex-file'."
  (interactive)
  (unless filename (setf filename (picasm-find-hex-file)))
  (picasm-compare-chips)
  (if (string-match "Verify Succeeded."
                    (picasm-run-pk2cmd
                     (concat "-F" (expand-file-name filename)) "-Y"))
      (message "Chip verify succeeded")
    (message "Chip verify failed")))

(defun picasm-pk2cmd-program (&optional filename)
  "Program the attached chip with a hex file. Either use
`filename', or find a file using `picasm-find-hex-file'."
  (interactive)
  (unless filename (setf filename (picasm-find-hex-file)))
  (picasm-compare-chips)
  (if (string-match "Program Succeeded."
                    (picasm-run-pk2cmd
                     (concat "-F" (expand-file-name filename)) "-MP"))
      (message "Chip progamming succeeded")
    (message "Chip programming failed")))

(provide 'picasm-external)
