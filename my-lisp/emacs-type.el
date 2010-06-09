;;;
;;; emacs-type.el --
;;; 
;;; This file defines constants and small functions which are useful for
;;; keeping files compatible with emacs versions 18 and 19 and between
;;; versions of different operating systems.

(setq emacs20 (string< "GNU Emacs 20" (emacs-version)))
(if emacs20
    (setq emacs-type 'emacs-20))

(setq emacs21 (string< "GNU Emacs 21" (emacs-version)))
(if emacs21
    (setq emacs-type 'emacs-21))

(setq emacs22 (string< "GNU Emacs 22" (emacs-version)))
(if emacs22
    (setq emacs-type 'emacs-22))

;; e.g. "21.3.2"
(setq emacs-version-string
      (car (cdr (cdr (split-string (emacs-version))))))

(unless (boundp 'running-xemacs)
  (setq running-xemacs nil))

(defvar running-x (eq window-system 'x)
  "True if running under X-Windows")

(defmacro if-terminal (&rest then-forms)
  "If we're running in terminal-mode, evaluate THEN-FORMS."
  `(if (not window-system)
       (progn ,@then-forms)))

(defmacro if-not-terminal (&rest then-forms)
  "If we're running in windowed-mode, evaluate THEN form.
Otherwise evaluate optional ELSE forms."
  `(if window-system
       (progn ,@then-forms)))

(defmacro if-not-xemacs (&rest args)
  "Evaluate forms in ARGS iff this is FSF Emacs."
  `(if (not running-xemacs)
       (progn ,@args)))

(defmacro if-xemacs (&rest args)
  "Evaluate forms in ARGS iff this is XEmacs."
  `(if running-xemacs
       (progn ,@args)))

;; not sure what system-type is for NT XEmacs...
(defmacro if-win32 (&rest args)
  "Evaluate forms in ARGS iff this is NT Emacs."
  `(if (eq system-type 'windows-nt)
       (progn ,@args)))

(defmacro if-not-win32 (&rest args)
  "Evaluate forms in ARGS iff this is not NT Emacs."
  `(unless (eq system-type 'windows-nt)
       (progn ,@args)))

(defvar win32 (eq system-type 'windows-nt))
(defvar terminal (eq window-system nil))

;;;; setenv (as a command)
(defun setenv (name value)
  (interactive "sVariable name? \nsValue? ")
  (let* ((pattern (concat "^" name "="))
	 (newval (concat name "=" value))
	 (e process-environment)
	 done)
    (setq done (catch 'done
		 (while e
		   (if (string-match pattern (car e))
		       (progn
			 (setcar e newval)
			 (throw 'done t)))
		   (setq e (cdr e)))))
    (if (not done)
	(setq process-environment (cons newval process-environment)))))

;; computer names
(defvar hostname (or (getenv "HOSTNAME")
		     ;; really need a (string-trim) function
		     (car (split-string (shell-command-to-string "hostname")))
		     ""))

;; true across any system of these types
(defvar cygwin (eq system-type 'cygwin))
(defvar winnt (eq system-type 'windows-nt))
(defvar linux (eq system-type 'gnu/linux))

(defvar macos (eq system-type 'darwin))
(defvar darwin macos)

;; set system-name variable based on domain
(setq system-name hostname)

(provide 'emacs-type)

;;; emacs-type ends here
