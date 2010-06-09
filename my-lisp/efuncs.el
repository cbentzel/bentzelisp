;;; efuncs.el:  misc utility functions
;;;
;;; Includes a bunch of commands I use frequently, and a few
;;; non-interactive functions I use in the *scratch* buffer.
;;;
;;; The file is organized as follows:
;;;
;;; Programming:  misc stuff I use for elisp programming
;;; File/Package:  useful commands for manipulating files
;;; Queries: commands for asking about emacs and open buffers
;;; Editing:  commands for moving text around
;;; Navigation:  commands for moving cursor around
;;; Window/Frame:  commands operating on the user interface
;;; Network:  some commands for doing web queries
;;;
;;; Original Author:  Steve Yegge (steve.yegge@gmail.com)

(require 'cl)
(require 'rect)
(require 'thingatpt)

;;;
;;; Programming utilities, some used by functions below.
;;;

(defun reload ()
  "Reloads the .emacs file."
  (interactive)
  (load "~/.emacs"))

(defun log-msg (buf msg &rest args)
  "Writes a log message into BUF.  MSG is a format string.
Optional ARGS are the arguments to `format'.  The output buffer
is created if necessary, but not cleared.  Message is appended,
followed by a newline.  If BUF is nil, output buffer is `*log*'."
  (save-excursion
    (set-buffer (get-buffer-create (or buf "*log*")))
    (end-of-buffer)
    (insert (apply #'format msg args))
    (insert "\n")))

(defmacro narrow-to-string (string &rest forms)
  "Execute FORMS in the context of STRING, and return result string.
The string is placed in a temp buffer, which becomes the current
buffer for FORMs.  The point starts at (point-min).  After the
body forms are evaluated, the buffer contents are returned as a
string with no text properties."
  `(save-excursion
     (with-temp-buffer
       (insert ,string)
       (beginning-of-buffer)
       ,@forms
       (buffer-substring-no-properties
        (point-min) (point-max)))))


(defun unbind (args)
  "Calls `makunbound' on each symbol name in string ARGS.
Calls `split-string' to extract the names from ARGS."
  (interactive "sSymbols to make unbound: ")
  (mapc #'unintern
         (mapcar #'intern (split-string args)))
  (message "Unbound '(%s)" args))


(defun char-at (str pos)
  "Just like Java's String.charAt()"
  (string (elt str pos)))


(defun get-current-line ()
  "Returns the text of the current line as a string."
  (buffer-substring-no-properties
   (point-at-bol) (point-at-eol)))


(defun buffer-lines (&optional buf)
  "Returns a list of all lines in buffer BUF.
If BUF is nil, uses current buffer."
  (save-excursion
    (if buf (set-buffer buf))
    (goto-char (point-max))
    (let ((lines (list (get-current-line))))
      (while (not (minusp (forward-line -1)))
        (cl-push (get-current-line) lines))
      lines)))

(defun print-list (list &optional sort)
  "Prints LIST, one item per line.  If SORT, sorts first.
Sorting is alphabetical by prin1 representation of the object."
  (interactive "vList: ")
  (save-excursion
    (mapcar (lambda (x)
              (princ x)
              (terpri))
            (if sort (sort (copy-list list)
                           #'(lambda (s1 s2)
                               (string<
                                (if (stringp s1) s1 (prin1-to-string s1))
                                (if (stringp s2) s2 (prin1-to-string s2)))))
              list))
    nil))

(defmacro with-buffer (buf form)
  "Executes FORM in buffer BUF.
BUF can be a buffer name or a buffer object.
If the buffer doesn't exist, it's created."
  `(let ((buffer (gentemp)))
    (setq buffer
	  (if (stringp ,buf)
	      (get-buffer-create ,buf)
	    ,buf))
    (save-excursion
      (set-buffer buffer)
      ,form)))

(put 'with-buffer 'lisp-indent-function 1)

;;;
;;; File and directory elisp utilities.
;;;

(defun parent-directory (path)
  "Returns the parent directory of PATH (a file or directory).
If path is a relative filename, prepends current directory."
  (let ((fullpath (expand-file-name path)))
    (if (file-directory-p fullpath)
	(setq fullpath (directory-file-name fullpath)))
    (expand-file-name (concat fullpath "/.."))))

(defun directory-name-sans-path (path)
  "Returns the last component of a directory path,
with no separators."
  (car-safe (last
	     (split-string
	      (directory-file-name path)
	      "[/\\]"))))

;;;
;;; File and package commands.
;;;

(defun delete-this-file ()
  "Delete the file associated with the current buffer, if any."
  (interactive)
  (if buffer-file-name
      (delete-file buffer-file-name)
    (error "No file associated with this buffer.")))


(defun dos2unix ()
  "Converts current buffer's line-endings to unix-style (LF)."
  (interactive)
  (set-buffer-file-coding-system 'unix))


(defun unix2dos ()
  "Converts current buffer's line-endings to windows-style (CRLF)."
  (interactive)
  (set-buffer-file-coding-system 'dos))


(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))


(defun find-file-shell-command (cmd)
  "Passes CMD to the shell to retrieve the filename to visit.
For instance, M-x find-file-shell-command which ediff."
  (interactive "sCommand to locate filename: ")
  (find-file
   (narrow-to-string
    (shell-command-to-string cmd)
    (end-of-buffer)
    (if (minusp (skip-chars-backward "\r\n"))
        (delete-region (point) (point-max))))))


(defun find-library (library)
  "Find the requested library AND read the file into a buffer.
It has to try pretty frigging hard to provide a reasonable guess,
since `symbol-at-point' doesn't return anything, ironically, if
the point is on a symbol."
  (interactive (list
                (read-string
                 "Locate library: "
                 (let ((guess (symbol-at-point)))
                   (if guess (symbol-name guess)
                     (save-excursion
                       (backward-sexp)
                       (if (looking-at "'\\([a-z-]+\\)")
                           (buffer-substring-no-properties
                            (match-beginning 1)
                            (match-end 1)))))))))
  (let ((file (locate-library library)))
    (if file
        (find-file
         (if (string-match "\\.elc$" file)
             (substring file 0 -1)
           file))
      (message "Couldn't find it."))))


(defun byte-compile-directory (dir)
  "Byte-compiles all the .el files in DIR."
  (interactive "DDirectory: ")
  (let ((files (mapcar 'car (directory-files-and-attributes dir t "\.el$"))))
    (mapcar (lambda (file)
              (byte-compile-file file))
            files)))

(defun try-to-load-file (f)
  (when (file-exists-p f) (load-file f)))

;; I really want to get this working...

(defun tail-file (file)
  "Shows additions to FILE with tail -f in its own buffer."
  (interactive "fFile: " )
  (let* ((name (concat "tail-" file))
         (bufname (concat "*" name "*"))
         (buffer (get-buffer bufname))
         process)
    (if (and buffer (get-buffer-process buffer))
        (pop-to-buffer bufname)
      (pop-to-buffer
       (make-comint name "tail" "-f" file))
      (setq process (get-buffer-process (current-buffer)))

      ;; start reading ye olde output into the buffer
      (accept-process-output process 4))))

;;;
;;; Date and time utilities.
;;;

(defconst month-alist '((Jan . "01") (Feb . "02") (Mar . "03")
                        (Apr . "04") (May . "05") (Jun . "06")
                        (Jul . "07") (Aug . "08") (Sep . "09")
                        (Oct . "10") (Nov . "11") (Dec . "12"))
"An alist mapping month names to numeric strings.")

(defun date-string (&optional short-form-p)
  "Converts the current date to a short string for src templates.
If short-form-p it non-nil, the format is \"Aug 11, 1997\", else
it's \"08/11/97\"."
  (let ((time (current-time-string)))   ;; "Tue Aug 12 14:12:07 1997"
    (cond
     (short-form-p
      (concat (cdr (assq
                    (car (read-from-string (substring time 4 7)))
                    month-alist))
              "/"
              (substring time 8 10)
              "/"
              (substring time 22 24)))
     (t
      (concat (substring time 4 7)
              " "
              (substring time 8 10)
              ", "
              (substring time 20 24))))))

(defun year-string ()
  "Convert the current year to a short string for src templates."
    (substring (current-time-string) 20 24))

;;;
;;; Informational/queries.
;;;

(defun what-column ()
  "Prints which column the point is in.  Left column is column 1."
  (interactive)
  (message (prin1-to-string (1+ (current-column)))))


(defun what-char ()
  "Prints character position of cursor in current buffer.  1-indexed."
  (interactive)
  (message (prin1-to-string (point))))


(defun count-chars-region (start end)
  "Prints the length in chars of the region."
  (interactive "r")
  (let ((hd (min start end))
        (tl (max start end)))
    (if (interactive-p)
        (message "%s" (- tl hd))
      (- tl hd))))


(defun count-non-whitespace-chars (start end)
  "Counts the number of non-whitespace chars occurring in region."
  (interactive "r")
  (let* ((str (buffer-substring-no-properties start end))
         (len (length (replace-regexp-in-string "[ \t\r\n]" "" str))))
    (if (interactive-p)
         (message "%d non-whitespace chars (%d chars total)" len (length str)))
    len))


(defun line-stats (start end)
  "Prints out stats about lines in current region."
  (interactive "r")
  (message "implement me"))


(defun line-iterator (func &optional start end buf)
  "Calls FUNC for each line beteen START and END in BUF.
START and END are line numbers, not buffer positions.
START and END are swapped END is greater than START.
If START is nil, starts iteration at line 1.  If END
is nil, goes to end of buffer.  If BUF is nil, uses
`current-buffer'.  FUNC is a function that takes 2 args:
the text of the current line, and its line number.

Example usage:

;; print line numbers and lengths of lines > 80 chars
(let (msgs)
  (line-iterator
   (lambda (line num)
     (if (> (length line) 80)
         (cl-push (format \"Line %s:  %s chars\"
                          num (length line)) msgs)))
   nil nil nil)
  (dolist (msg (nreverse msgs))
    (insert msg \"\n\")))
"
  (save-excursion
    (set-buffer (or buf (current-buffer)))
    (setq start (or start 1)
          end (or end (point-max)))
    (unless (and (plusp start)
                 (plusp end))
      (error "START (%s) and END (%s) must be positive numbers."
             start end))
    (goto-line start)
    (let ((count start))
      (while (and (not (eobp))
                  (< count end))
        (funcall func (buffer-substring-no-properties
                       (point-at-bol)
                       (point-at-eol))
                 count)
        (incf count)
        (next-line 1)))))


(defun compute-line-stats (&optional start end buf)
  "Compute statistics about lines between START and END.
START and END are line numbers.  They default to the
first and last lines of BUF, respectively.  BUF defaults
to the current buffer.

Returns stats an alist of (name . value) pairs,
where name can be 'lines, 'longest, 'shortest, 'avg,
'chars, or 'blanks."
  (let ((lines 0)
        (chars 0)
        (blanks 0)
        longest shortest len)
    (line-iterator
     (lambda (line num)
       (incf lines)
       (setq len (length line))
       (incf chars len)
       (or (null shortest)
           (< len shortest)
           (setq shortest len))
       (or (null longest)
           (> len longest)
           (setq longest len))
       (if (string-match "^[ \t\r\n]*$" line)
           (incf blanks)))
     start end nil)
    (list (cons 'total lines)
          (cons 'chars chars)
          (cons 'longest longest)
          (cons 'shortest shortest)
          (cons 'blanks blanks)
          (cons 'avg (/ chars lines)))))


(defun count-words-region (start end)
  "Counts the words in the current region.  Like, duh."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (let ((count 0))
        (while (forward-word 1)
          (incf count))
        (message (prin1-to-string count))
        count))))


(defun date ()
  "Echoes the current date and time."
  (interactive)
  (message (current-time-string)))
(global-set-key "\C-cd" 'date)


(defun insert-date ()
  "Inserts the current date as, e.g., \"Jun 08, 2003\""
  (interactive)
  (insert (format-time-string "%b %d, %Y")))


(defun insert-datetime ()
  "Inserts the current date as, e.g., \"Jun 08, 2003 10:45am\""
  (interactive)
  (insert (format-time-string "%b %d, %Y %I:%M%p")))


(defun hostname ()
  "Displays the HOSTNAME environment variable."
  (interactive)
  (message (getenv "HOSTNAME")))


(defun list-not-matching-lines (expr)
  "Show all lines in the current buffer that don't match REGEXP.
Note that it doesn't (yet) accept partial matches, although that
could maybe be added as an optional argument."
  (interactive "sSkip lines not matching regexp: ")
  (let* ((curbuf (current-buffer))
         (count 0)
         (occur (get-buffer "*Occur*"))
         (regex expr)
         (output
          (progn
            (if occur (kill-buffer occur))
            (get-buffer-create "*Occur*"))))

    (save-excursion
      (set-buffer output)
      (make-local-variable 'next-line-add-newlines)
      (setq next-line-add-newlines nil)
      (erase-buffer)
      (set-buffer curbuf)

      ;; go through each line in input, matching the regex
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (if (not (looking-at regex))

            ;; this line doesn't match => keep it
            (progn
              (let* ((start
                      (save-excursion
                        (beginning-of-line)
                        (point)))
                     (end
                      (save-excursion
                        (end-of-line)
                        (point)))
                     (line (buffer-substring start end)))
                (set-buffer output)
                (setq count (1+ count))
                (insert line)
                (insert "\n")
                (set-buffer curbuf))))
        (next-line 1)
        (end-of-line))

      ;; switch to output buffer
      (set-buffer output)
      (beginning-of-buffer)
      (insert
       (concat (prin1-to-string count)
               " lines not matching \""
               expr
               "\" in buffer "
               (buffer-name curbuf)
               "\n\n"))
      (set-buffer-modified-p nil))

    (display-buffer output)))


(defun load-path ()
  (interactive)
  (print-list load-path))


(defun list-features ()
  "Shows all the libraries currently loaded."
  (interactive)
  (with-output-to-temp-buffer "*Features*"
    (princ "Emacs-lisp features currently loaded:\n")
    (mapcar (lambda (f) (prin1 f) (terpri))
            (sort (copy-sequence features) 'string<))))


(defun list-grep (regexp list-var &optional literal)
  "Returns items in LIST-VAR whose print form matches REGEXP.
Optional LITERAL means to search literally, not for a regexp.
Skips any elements of LIST-VAR that are conses.
Called interactively, prompts for regexp and list var,
and displays the output in a temp buffer.

Example:  M-x list-grep <RET> google <RET> features
shows all the symbols in `features' that contain google.
"
  (interactive "sRegexp to look for: \nXList: ")
  (unless (listp list-var)
    (funcall
     (if (interactive-p) 'message 'error
         (format "%s is not a list variable" list-var))))
  (if literal
      (setq regexp (regexp-quote regexp)))
  (let (results
        value)
    (dolist (item list-var
                  (setq results (nreverse results)))
      (setq value
            (cond
             ((symbolp item) (symbol-name item))
             ((stringp item) item)
             ((atom item) (prin1-to-string item))
             (t "")))
      (if (string-match regexp value)
          (cl-push item results)))
    (if (interactive-p)
        (if (null results)
            (message "No matches found.")
          (with-output-to-temp-buffer "*list-grep*"
            (princ (format "list-grep matches for \"%s\":\n"
                           regexp))
            (dolist (result results)
              (print result))))
      results)))


(defun show-trailing-whitespace ()
  "Toggles showing trailing whitespace (in current buffer)."
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace))
  (message "Showing trailing whitespace is %s"
           (if show-trailing-whitespace "enabled" "disabled")))

(defun line-length-current-line ()
  "Returns the length of whatever line we're on."
  (interactive)
  (let ((len (- (point-at-eol) (point-at-bol))))
    (if (interactive-p)
        (message "%s" len)
      len)))


(defun longest-line (&optional buf)
  "Tells you the length of the longest line in the buffer."
  (interactive)
  (let ((longest
         (apply #'max
                (mapcar #'length
                        (buffer-lines (or buf (current-buffer)))))))
    (if (interactive-p)
        (message (format "longest line: %d" longest)))
    longest))

;;;
;;; Editing utilities.
;;;

;;  Gene Andersen function - invaluable.

(defun indent-for-reply ()
  "Inserts '> ' at the beginning of each line in region."
  (interactive)
  (save-excursion
    (let ((e (make-marker)))
      (set-marker e (point))
      (goto-char (mark))
      (while (< (point) e)
        (progn
          (beginning-of-line)
          (insert-string "> ")
          (next-line 1))))))


(defun reverse-line ()
  "Reverses the current line."
  (interactive)
  (save-excursion
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (contents (buffer-substring-no-properties start end)))
      (beginning-of-line)
      (kill-line nil)
      (insert (concat (reverse (string-to-list contents)))))))

(defun reverse-region-horizontally ()
  "Reverses the current region around the Y axis,
by reversing each line in the region."
  (interactive)
  (save-restriction
    (let* ((line 1)
           (start (region-beginning))
           (end (region-end))
           (last (count-lines start end)))
    (narrow-to-region start end)
    (goto-char start)
    (while (not (= line last))
      (reverse-line)
      (forward-line 1)
      (incf line))
    (reverse-line))))


(defun kill-whole-line ()
  "Kills to end of current line, including the newline."
  (interactive)
  (let ((beg (point))
        (end (save-excursion
               (next-line 1)
               (point-at-bol))))
    (kill-region beg end)))


(defun num-seq (count)
  "Inserts a column of COUNT numbers, increasing sequentially."
  (interactive "nCount: ")
  (save-excursion
    (let ((curcol (current-column))
          (num 1))
      (while (> count 0)
        (insert (prin1-to-string num))
        (setq num (1+ num))
        (next-line 1)
        (move-to-column-force curcol)
        (setq count (1- count))))))


(defun insert-column (count string)
  "Inserts a column of COUNT instances of STRING at the point."
  (interactive "nCount: \nsString: ")
  (save-excursion
    (let ((curcol (current-column)))
      (while (> count 0)
        (insert string)
        (next-line 1)
        (move-to-column-force curcol)
        (setq count (1- count))))))


(defun prepend-region (str)
  "prepend a string to the beginning of every line in region."
  (interactive "sString to prepend: ")
    (save-excursion
      (let ((e (make-marker)))
        (set-marker e (point))
        (goto-char (mark))
        (while (< (point) e)
            (progn
                (beginning-of-line)
                (insert-string str)
                (next-line 1))))))

;;;
;;; Navigation and editing utilities and shortcuts.
;;;

;; rebind M-n and M-p in *Occur* buffers to keep viewing the
;; buffer where M-x occur was run originally.

(defadvice occur-next (after occur-go-there nil activate)
  "If called interactively, actually visit the occurrence.
Keep the focus in the *Occur* buffer, though."
  (when (interactive-p)
       (occur-mode-follow)))


(defadvice occur-prev (after occur-go-there nil activate)
  "If called interactively, actually visit the occurrence.
Keep the focus in the *Occur* buffer, though."
  (when (interactive-p)
    (occur-mode-follow)))


(defun occur-mode-follow ()
  "Keeps original buffer window in sync with the *Occur* selection."
  (if (string= mode-name "Occur")
       (let ((buf (current-buffer)))
         (occur-mode-goto-occurrence)
         (pop-to-buffer buf))))

;; why didn't I just use move-to-column?  Hmmmm...

(defun goto-column (arg)
  "Goto column ARG, counting from column 1 at beginning of line.
Goes to end of line if line is shorter than ARG."
  (interactive "NGoto column: ")
  (setq arg (prefix-numeric-value arg))
  (beginning-of-line 1)
  (let ((len (line-length-current-line)))
    (if (< len arg)
        (end-of-line 1)
      (forward-char arg))))


(defun goto-longest-line ()
  "Puts cursor at the longest line in the current buffer."
  (interactive)
  (let ((max-len -1)
        (max-line -1)
        (cur-line 1)
        (last (count-lines (point-min) (point-max))))
    (beginning-of-buffer)
    (while (< cur-line last)
      (let ((len (line-length-current-line)))
        (when (> len max-len)
          (setq max-len len)
          (setq max-line cur-line)))
      (forward-line 1)
      (incf cur-line))
    (when (>= max-len 0)
      (goto-line max-line)
      (message "Longest line: %d (%d characters)" max-line max-len))))

(defun camel-to-underscore (string)
  "Change a CamelCase STRING to lower_underscore, by brute force.
Bug:  if STRING ends in an underscore, and no replacement happens,
then the trailing underscore will be removed.  Fixable, but I'm lazy."
  (let ((result
         (downcase
          (replace-regexp-in-string "\\([A-Z][a-z]+\\)" "\\1_" string))))
    ;; remove trailing underscore
    (if (= (aref result (1- (length result))) ?_)
        (substring result 0 -1)
      result)))

(defun query-replace-camel-with-underscore ()
  "Query-replace CamelCase expressions with lower_underscore versions.
E.g. `ExternalCustomerId' will change to `external_customer_id'.
Highlights and prompts you for changing each match.  It's not as nice
as `query-replace-regexp' yet, in terms of the options it gives you.
Is currently undo-able in one step -- eventually each step should be
independently undoable."
  (interactive)
  (let ((case-fold-search nil)        ; don't ignore case
        (camel "\\([A-Z][a-z]+\\)+")  ; look for StuffLikeThis (or This)
        (highlight (make-overlay (point-max) (point-max))))
    (overlay-put highlight 'face 'highlight)
    ;; make sure overlay is deleted, even if an error is thrown
    (unwind-protect
        (progn
          (beginning-of-buffer)
          (while (search-forward-regexp camel nil t)
            (move-overlay highlight (match-beginning 0) (match-end 0))
            ;; quick-and-dirty prompter, really needs to be better
            (if (y-or-n-p "Convert this match? ")
                (replace-match (camel-to-underscore (match-string 0)) t t))))
      (when highlight (delete-overlay highlight)))))

;;;
;;; Misc window and frame utilities.
;;;

;; someday might want to rotate windows if more than 2 of them

(defun swap-windows ()
  "If you have 2 windows open, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2))
    (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))


(defun shell-frame ()
  "Makes a new frame with its own shell buffer."
  (interactive)
  (let* ((shell-frame-params
          '((name . "Emacs Shell")
            (background-color . "LemonChiffon")
            (cursor-color . "SeaGreen")
            (font . linux-font)
            (height . 70)
            (width . 80)))
         (frame1 (selected-frame))
         (buf (current-buffer))
         (frame2))
    ;; make the new frame and go to shell buffer
    (setq frame2 (make-frame shell-frame-params))
    (select-frame frame2)
    (if (null (get-buffer "*shell*"))
        (shell))
    (switch-to-buffer "*shell*")
    (rename-buffer "*Emacs Shell*")

    ;; go back to the original frame and buffer, and make a new shell.
    (select-frame frame1)
    (shell)
    (switch-to-buffer buf)))


(defun client-frame ()
  "squeezes our frame over to the other side."
  (interactive)
  (let* ((new-frame-params
          '((name . "Second Emacs")
            (background-coor . "LemonChiffon")
            (cursor-color . "SeaGreen")
            (font . linux-font)
            (height . 70)
            (width . 80))))
    (modify-frame-parameters (selected-frame) new-frame-params)
    (set-frame-position (selected-frame) 20 20)))


(defun pick-font ()
  "Puts up a dialog that lets you choose the font.  Win32 only."
  (interactive)
  (if (equal window-system 'w32)
      (let ((font (w32-select-font)))
        (if font (set-default-font font)))
    (message "This is only supported under Emacs for Win95/NT.")))


;; (defun color-region (name)
;;   "Sets the current text region to the specified color name."
;;   (interactive "sColor: ")
;;   (let ((immutable buffer-read-only))
;;     (if immutable (toggle-read-only))
;;     (if (not (color-defined-p name))
;;      (message (concat "Invalid color: " name))
;;       (put-text-property (point) (mark) 'face (list :foreground name)))
;;     (if immutable (toggle-read-only))))

;; ;; This really ought to go look up the hex value from the color name,
;; ;; if a color name is specified.
;; (defun color-region (start end color)
;;   "Prompts for a color, and puts a font tag in that color around
;; the current region."
;;   (interactive "r\nscolor name or rgb value: ")
;;   (let ((prefix (concat "<font color=\"" color "\">")))
;;     (save-excursion
;;       (goto-char start)
;;       (insert prefix))
;;     (save-excursion
;;       (goto-char (+ end (length prefix)))
;;       (insert "</font>"))))

;;;
;;; Network utilities, and functions that require a network
;;; connection (usually for http requests).
;;;

(defun browse-buffer-netscape ()
  "Loads the current buffer into your preferred browser.
Buffer must be visiting a file."
  (interactive)
  (browse-url-netscape (buffer-file-name)))

;; I'd love to parse the html and put it in an Emacs buffer, but after quite a few
;; attempts and lots of investigation, gave up for now.  I hate screen-scraping,
;; especially when the HTML is hideous.  And neither Emacs, Ruby nor Python has a
;; decent HTML-parsing library.  Basically the results-in-buffer approach has to
;; wait until I can find a free Web Service to hit.

(defun lookup-word (word)
  "Looks up a word by making the request to dictionary.com via browse-url-netscape."
  (interactive "sWord to look up: ")
  (let ((browse-url-netscape-program "C:\\Program Files\\Mozilla Firefox\\firefox.exe"))
    (browse-url-netscape
     (concat "http://dictionary.reference.com/search?q=" word))))


(defun thesaurus (word)
  "Looks up a word in the online thesaurus."
  (interactive "sWord to fetch from thesaurus: ")
  (let ((browse-url-netscape-program "C:\\Program Files\\Mozilla Firefox\\firefox.exe"))
    (browse-url-netscape
     (concat "http://thesaurus.reference.com/search?q=" word))))


(eval-when-compile
  (require 'net-utils)
  (defvar efuncs-host-address nil "Cached host address."))

;; great example of a lisp function I wrote that worked on the first try


(defun get-host-address ()
  "Tries to fetch the host IP address of the current machine.
Returns nil if we can't find it."
  (interactive)
  (require 'net-utils)
  (if (boundp 'efuncs-host-address)
      (if (interactive-p)
          (message "%s" efuncs-host-address)
        efuncs-host-address)
    (let ((bufname "**efuncs-ifconfig-tmp**")
          (addr-re
           (concat
            (if (eq system-type 'windows-nt)
                "IP Address[\\. ]+:\\s-*"
              "inet addr\\s-*:\\s-*")
            "\\("
            (replace-regexp-in-string
             "o" "[0-9]\\{1,3\\}" "o.o.o.o" nil t)
            "\\)")))
      (apply 'call-process
             ipconfig-program
             nil  ; no input to program
             bufname
             nil  ; don't display buffer
             ipconfig-program-options)
      (set-buffer (get-buffer bufname))
      (goto-char (point-min))
      (if (re-search-forward addr-re
                             (point-max)
                             t)
          (setq efuncs-host-address
                (match-string 1))
        (setq efuncs-host-address nil))
      (kill-buffer bufname)
      (if (interactive-p)
          (message "%s" efuncs-host-address)
        efuncs-host-address))))

;; someone wanted this on the emacs-users mailing list at Google...
(defun biff-c-electric-functions ()
  "Run this at Emacs startup to disable c-mode automatic re-indentation.
It will turn it off for java-mode, c++-mode, and other c-derived modes too."
  (interactive)
  (mapc (lambda (sym)
          (when (and (fboundp sym)
                     ;; you can nuke these with (c-toggle-hungry-state -1)
                     (not (string-match "\\(backspace\\|delete\\)"
                                        (symbol-name sym))))
            (let* ((fun (symbol-function sym))
                   (arity (cond
                           ((subrp fun) (subr-arity sym))
                           ((compiled-function-p fun) 
                            (if (fboundp 'compiled-function-arglist)
                                (compiled-function-arglist fun)
                              2)) ;; punt for older Emacs versions
                           (t (length (aref (symbol-function sym) 0))))))
              (case arity
                ;; no args (e.g. c-electric-continued-statement): do nothing
                (0 (fset sym (lambda () nil)))
                ;; 1 arg (e.g. c-electric-paren): insert the arg literally
                (1 (fset sym (symbol-function 'self-insert-command)))
                ;; don't know how to handle it for sure, but let's hope
                ;; self-insert-command works. :-)
                (t (fset sym (symbol-function 'self-insert-command)))))))
  (apropos-internal "^c-electric")))

(defun restore-c-electric-functions ()
  "Put them all back!  Please!"
  (interactive)
  (load-library "cc-cmds"))

;;-----------------------------------------------------------------------------
;; Functions for rotating windows from Howard Yeh <hayeah@gmail.com>
;;-----------------------------------------------------------------------------

(defun rotate-list-right (lst)
  (if lst
      (let* ((lst (reverse lst))
         (tail (car lst))
         (lst (cdr lst))
         (lst (reverse lst)))    
    (cons tail lst))
    nil))

(defun rotate-list-left (lst)
  (if lst
      (append (cdr lst) (list (car lst)))
    nil))

(defun mapc* (function &rest seqs)
  "Apply FUNCTION to successive cars of all ARGS. Values not accumulated"
  (if (not (memq 'nil seqs))
      (progn (apply function (mapcar 'car seqs))
         (apply 'mapc* function (mapcar 'cdr seqs)))))

;(rotate-list-right '(1 2 3 4 5))
;(rotate-list-left '(1 2 3 4 5))

(defun rotate-windows (fn)
  "Rotate windows in some direction."
  (let* ((w-list (window-list))
     (b-list (funcall fn
          (mapcar 'window-buffer w-list)))
     (s-list (funcall fn
          (mapcar 'window-start w-list))))
    (mapc* (lambda (w b s)
         (set-window-buffer w b)
         (set-window-start w s))
       w-list b-list s-list)))

(defun rotate-windows-down ()
  "Window 1 => Window 2 => Window 3 ... => Window N => Window 1"
  (interactive)
  (rotate-windows 'rotate-list-right))

(defun rotate-windows-up ()
  "Window 1 <= Window 2 <= Window 3 ... <= Window <= Window 1"
  (interactive)
  (rotate-windows 'rotate-list-left))

;;; end of Howard's code

;;-----------------------------------------------------------------------------
;; A little stopwatch application
;;-----------------------------------------------------------------------------

(defvar stopwatch-timer nil "Timer used for stopwatch.")
stopwatch-timer

(defun stopwatch-start ()
  "Starts a stopwatch timer.  1-second granularity up to 45 days."
  (interactive)
  (setq stopwatch-timer (cadr (current-time)))
  (message "Started the stopwatch timer."))


(defun stopwatch-stop ()
  "Stops the stopwatch timer, if running, and reports elapsed time."
  (interactive)
  (if stopwatch-timer
      (let ((now (cadr (current-time))))
        (message "Elapsed time: %d seconds" (- now stopwatch-timer))
        (setq stopwatch-timer nil))
    (message "The stopwatch timer is not running.")))

(global-set-key [f11] 'stopwatch-start)
(global-set-key [f12] 'stopwatch-stop)

;;; End stopwatch application


(defun toggle-cc-h-buffers (&optional no-bury)
  "If visiting a .cc/.h, flip to the corresponding .h/.cc, respectively.
If the other file isn't open, looks for it in the current directory.
If called with no arguments, it automatically buries the current buffer.
With a prefix arg, it does NOT bury the current buffer."
  (interactive "p")
  (unless (and (buffer-file-name)
               (string-match (concat "\\." 
                                     (regexp-opt '("cc" "cpp" "c" "h") t)
                                     "$")
                             (buffer-file-name)))
    (error "Not visiting a .c, .cc or .h file"))
  (let* ((path (expand-file-name
                (buffer-file-name)))            ; /foo/bar/blah.cc
         (file (file-name-sans-extension
                (file-name-nondirectory path))) ; blah 
         (ext (file-name-extension path))       ; cc
         (dir (file-name-directory path))       ; /foo/bar/
         (newext
          (if (string= ext "h")
              (or (find-if (lambda (ext)
                             (file-exists-p (concat file ext)))
                           '(".cc" ".cpp" ".c"))
                  ".cc")
            ".h"))
         (newname (concat dir file (or newext ""))))
    (if (and newext (file-exists-p newname))
        (progn
          ;; bury the current buffer, on the assumption that if we want
          ;; to get back to it, we'll use toggle-cc-h-buffers again.
          ;; This makes C-x b slightly more useful, IMO.
          (unless (and current-prefix-arg
                       (numberp no-bury)
                       (plusp no-bury))
               (bury-buffer))
          (find-file newname))
      (error "%s doesn't seem to exist" newname))))

(defun swap-strings (s1 s2)
  "Replaces all instances of S1 with S2 and vice-versa.
Operates from the point to the end of the buffer."
  (interactive "ss1: \nss2: ")
  (labels ((maketmp () (downcase (symbol-name (gentemp))))
           (repl (a b) (save-excursion
                         (while (search-forward a nil t)
                           (replace-match b nil t)))))
    (let ((tempstring (maketmp))
          (tries 0))
      (while (save-excursion
               (search-forward tempstring nil t))
        (if (> (incf tries) 100)
            (error "Couldn't generate a unique temp string."))
        (setq tempstring (maketmp)))
      (repl s1 tempstring)
      (repl s2 s1)
      (repl tempstring s2))))

(defun sort-chars-in-region (beg end)
  "Sorts the characters in the given region."
  (interactive "r")
  (let ((sorted
         (concat
          (sort
           (string-to-list (buffer-substring-no-properties beg end))
           #'<))))
    (delete-region beg end)
    (insert sorted)))
  
(defun reverse-chars-in-region (beg end)
  "Reverses the order of the characters from BEG to END."
  (interactive "r")
  (let ((chars (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (concat (reverse (string-to-list chars))))))


(defun reopen-all-buffers ()
  "Reopen all non-dirty buffers associated with a file.
Useful after you've done a g4 sync or g4 submit from the shell."
  (interactive)
  (let ((buffers (buffer-list))
        (count 0)
        (temp-buffer-show-function
         (lambda (tempbuf)
           (if (zerop count)
               (message "No files were reopened.")
             (set-buffer (get-buffer "*reopened*"))
             (insert (format "Reopened %d files\n" count))
             (pop-to-buffer "*reopened*")))))
    (with-output-to-temp-buffer "*reopened*"
      (dolist (buf buffers)
        (let ((name (buffer-file-name buf)))
          (and name
               (file-exists-p name)
               (not (buffer-modified-p buf))
               (not (verify-visited-file-modtime buf))
               (progn
                 (set-buffer buf)
                 (princ (format "reopened %s\n" name))
                 (find-alternate-file name)
                 (incf count))))))))


(provide 'efuncs)

;;; efuncs.el ends here
