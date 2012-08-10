;; Code to talk to the chip database.

(require 'url)

(defcustom picasm-db-file "~/.emacs.d/picasm-db.el"
  "Location of the PIC chip database (in ELISP format)"
  :type 'string :group 'picasm)

(defvar picasm-chip-db nil
  "This is an alist, keyed by chip name (capitalised). The
contents for a given chip are themselves an alist. When read from
the Microchip website, they will have (string) keys for the
various parameters, together with 'chip-url and 'datasheet-url
keys.")

(defvar picasm-uchip-search-url
  "http://www.microchip.com/search/searchapp/searchparts.aspx?q=%s&id=3"
  "This should be a valid format string with a single %s, which
runs a search on the Microchip website for a chip by name.")

(defvar picasm-uchip-result-regex
  "<a[^>]+href=['\"]\\([^'\"]+\\)['\"].*?>\\[More Info\\]"
  "This should be a regex with a single capture that picks out
the results from Microchip's chip search.")

(defvar picasm-uchip-refresh-regex
  "<META HTTP-EQUIV=['\"]?REFRESH['\"]?[^>]+CONTENT.*?;URL=\\(.*?\\)['\"]"
  "A regex to extract a META refresh from a Microchip webpage")

(defvar picasm-uchip-table-start-regex
  "dvParams[[:ascii:][:nonascii:]]*?<table"
  "A regex to find the start of the parameters table.")

(defvar picasm-uchip-table-end-regex
  "</table"
  "A regex to find the end of the parameters table.")

(defvar picasm-uchip-datasheet-regex
  "Data Sheets</td>.*?<a .*?href='\\(.*?\\)'"
  "A regex to find the datasheet.")

(defvar picasm-uchip-table-element-regex
  "<td align='Left'>&nbsp;\\(.*?\\)<"
  "A regex that grabs an element from the table.")


;; Some utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun picasm-all-matches (regexp)
  "Return all the matches for REGEXP in the current
buffer. REGEXP is assumed to have at least one element of match
data, and the first is returned for each match."
  (save-excursion
    (goto-char (point-min))
    (let ((acc))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) acc))
      (nreverse acc))))

(defun picasm-list-to-pairs (list)
  "Take a list and return a list of cons pairs of its
elements. If `LIST' has odd length, the last pair will be of the
form (x . NIL)."
  (let ((acc))
    (while list
      (push (cons (first list) (second list)) acc)
      (setq list (cddr list)))
    (nreverse acc)))

(defmacro picasm-awhen (test &rest body)
  "Anaphoric when macro."
  `(let ((it ,test))
     (when it (progn ,@body))))

(defun picasm-alist-set (param value alist)
  "Return an alist, which is like `alist' but with (`param'
. `value') as well. If `alist' already had a value for `param',
it's replaced."
  ;; Sadly, this can't use assq-delete-all, since that tests with
  ;; eq. Bleurgh. And elisp doesn't have a find-if. Grr.
  (let ((acc))
    (catch 'found
      (while alist
        (let ((pair (first alist)))
          (when (equal param (car pair))
            (throw 'found
                   (cons (cons param value)
                         (nconc (nreverse acc) (rest alist)))))
          (push pair acc)
          (setq alist (rest alist))))
      (cons (cons param value) (nreverse acc)))))

;; Functions to communicate with the Microchip website ;;;;;;;;;;;;;;;

(defun picasm-uchip-find-url (name)
  "Run a search on the Microchip website for a chip by name. If
one is found, this returns its url."
  (picasm-awhen
   (url-retrieve-synchronously (format picasm-uchip-search-url name))
   (with-current-buffer it
     ;; Grab first hit. Hopefully it's the right one!
     (let ((hits (picasm-all-matches picasm-uchip-result-regex)))
       (if (= 1 (length hits))
           (first hits)
         ;; Oh dear, we can't make up our mind. Ask the user to input the
         ;; correct URL.
         (read-string
          "Cannot parse search results. Enter chip homepage: "))))))

(defun picasm-uchip-really-get-page (url)
  "Synchronously fetch the page at `URL'. Unfortunately, the
Microchip website sometimes uses <meta http-equiv=REFRESH>
tags, so this code does the rest of the job!"
  (catch 'buffer
    (dotimes (i 4 nil)
      (let ((buffer (url-retrieve-synchronously url)))
        (unless buffer (throw 'buffer nil))
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (re-search-forward picasm-uchip-refresh-regex nil t)
              (setq url (format
                         "http://www.microchip.com%s" (match-string 1)))
            (throw 'buffer (current-buffer))))))))

(defun picasm-uchip-read-datasheet-url ()
  "The current buffer should contain the main page for some
Microchip chip. This extracts the url for the datasheet."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward picasm-uchip-datasheet-regex nil t)
      (match-string 1))))

(defun picasm-uchip-read-params ()
  "The current buffer should contain the main page for some
Microchip chip. This extracts the table of parameters for the
part and returns its contents as a list of pairs."
  (save-excursion
    (goto-char (point-min))
    (let ((start (re-search-forward picasm-uchip-table-start-regex nil t))
          (end (re-search-forward picasm-uchip-table-end-regex nil t)))
      (when (and start end)
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (picasm-list-to-pairs
           (picasm-all-matches picasm-uchip-table-element-regex)))))))

(defun picasm-uchip-get-params (name)
  "Get the parameters for a given chip by name from the Microchip
website."
  (let ((url (picasm-uchip-find-url name)))
    (when url
      (picasm-awhen
       (picasm-uchip-really-get-page url)
       (with-current-buffer it
         (append
          (list (cons 'chip-url url))
          (picasm-awhen
           (picasm-uchip-read-datasheet-url)
           (list (cons 'datasheet-url it)))
          (picasm-uchip-read-params)))))))

(defun picasm-chip-read-db ()
  "Read in the PIC database. No meaningful return value."
  (if (not (file-readable-p picasm-db-file))
      (message "No chip database at %s." picasm-db-file)
    (message "Reading PIC database file...")
    (load picasm-db-file))
  nil)

(defun picasm-chip-write-db ()
  "Write out the current value of the PIC database."
  (with-current-buffer (get-buffer-create " *Picasm-db*")
    (setq buffer-file-name picasm-db-file)
    (setq default-directory (file-name-directory buffer-file-name))
    (buffer-disable-undo)
    (erase-buffer)
    (let ((standard-output (current-buffer))
          (print-quoted t)
          (print-readably t)
          (print-escape-multibyte nil)
          (print-escape-nonascii t)
          (print-length nil)
          (print-level nil)
          (print-circle nil)
          (print-escape-newlines t))
      (princ "\
;; -*- mode:emacs-lisp; coding: utf-8; -*-
;; PICasm database file.
;; You probably don't want to edit this by hand.\n\n")
      (princ "\n(setq picasm-chip-db '")
      (prin1 picasm-chip-db)
      (princ ")\n"))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun picasm-maybe-download-chip-info (chip-name)
  "Ask the user whether to download chip information. If so,
return that info (after prepending it to the DB). If not, return
nil."
  (when (y-or-n-p
         (format "Chip '%s' unknown. Get data from Microchip's website? "
                 chip-name))
    (let ((data (picasm-uchip-get-params chip-name)))
      (unless data
        (error "Data fetch failed."))
      (push (cons (upcase chip-name) data) picasm-chip-db)
      (picasm-chip-write-db)
      (first picasm-chip-db))))

(defun picasm-chip-alist (chip-name)
  "Get the stored alist for CHIP-NAME, prompting the user to get
info from Microchip's website if there isn't
one. Returns (`chip-name' . alist)."
  (or (assoc-string chip-name picasm-chip-db t)
      (picasm-maybe-download-chip-info chip-name)))

(defun picasm-chip-parameter (param &optional chip-name)
  "Get the value of `PARAM' the chip called `CHIP-NAME' (defaults
to value of `picasm-chip-select')"
  (unless chip-name (setq chip-name picasm-chip-select))
  (unless chip-name (error "No chip name chosen."))
  (picasm-awhen (cdr (picasm-chip-alist chip-name))
                (assoc param it)))

(defun picasm-chip-set-parameter (param value &optional chip-name)
  "Set the `param' to `value' for the given chip (defaults to
`picasm-chip-select'). If the chip doesn't yet have any data
stored, it won't actually do anything since that would screw up
the automatic downloading feature."
  (unless chip-name (setq chip-name picasm-chip-select))
  (unless chip-name (error "No chip name chosen."))
  (let ((alist (picasm-chip-alist chip-name)))
    (unless alist
      (error "Won't set parameter, as no data yet stored for chip %s."
             chip-name))
    (setf (cdr alist) (picasm-alist-set param value (cdr alist)))
    nil))

(defun picasm-describe-chip (&optional chip-name)
  (interactive "P")
  (unless chip-name (setq chip-name picasm-chip-select))
  (unless (stringp chip-name) (setq chip-name (read-string "Describe which chip? ")))
  (with-output-to-temp-buffer (format "*chip (%s) description*" chip-name)
    (set-buffer standard-output)
    (fundamental-mode)
    (let ((alist (cdr (picasm-chip-alist chip-name)))
          (old-pt))
      (when alist
        (insert chip-name)
        (put-text-property (point-min) (point) 'face
                           '(:weight bold :height 1.2))
        (insert "\n\n")

        (when (and (assoc 'chip-url alist)
                   (require 'browse-url nil t))
          (insert-button
           "Microchip webpage"
           'action `(lambda (button)
                     (browse-url ,(cdr (assoc 'chip-url alist))))
           'follow-link "\C-m"
           'help-echo "Go to the Microchip web page for the chip."))
        (insert "\n\n")

        (dolist (pair alist)
          (let ((key (car pair))
                (value (cdr pair)))
            (when (stringp key)
              (setq old-pt (point))
              (insert (format "%s:\n" key))
              (put-text-property old-pt (point) 'face 'bold)
              (setq old-pt (point))
              (insert (format "%s\n" value))
              (indent-region old-pt (point) 2)
              (fill-region old-pt (point)))))))
    (toggle-read-only 1)))

(provide 'picasm-db)
