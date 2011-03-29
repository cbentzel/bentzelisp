;; Chris Bentzel's .emacs
;; Tries to be cross platform (at least Ubuntu, Win7, and OSX)
;; as well as working on GNU Emacs > 22.
;; A good chunk of this is taken from Steve Yegge's elisp.

(require 'cl)

;; Load host specific configuration first.
(defun try-to-load-config-file (f)
  (when (file-exists-p f) (load-file f)))
(try-to-load-config-file "~/.local_emacs_config.el")

;; All of the real code is contained in a directory named "bentzelisp"
;; in my home directory. Try to find it across the different platforms.
(defvar elisp-root (concat (file-name-as-directory (getenv "HOME"))
			   "bentzelisp")
  "My home directory -- the root of my personal emacs load-path.")

;; Add all the elisp directories under ~/elisp to my load path
(defun add-to-path (p)
  (let ((full-path (concat (file-name-as-directory elisp-root) p)))
	(unless (memq full-path load-path)
	  (add-to-list 'load-path full-path))))
(add-to-path "my-lisp")
(add-to-path "site-lisp")
(add-to-path "color-theme-6.6.0")

;; Import the files.
(require 'efuncs) ; helper functions
(require 'emacs-type) ; Setup for different platforms/environments
(require 'my-config) ; Emacs-wide configurations
(require 'ekeys) ; Key bindings
(require 'modes) ; Mode-specific configurations
(require 'myfont) ; Font/color settings
(require 'psvn) ; SVN integration
(require 'erc-conf) ; ERC configuration
