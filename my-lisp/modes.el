;;; modes.el - set up and binding of multiple modes.

(require 'cl)
(require 'emacs-type)
(require 'google-c-style)

;;; first, a few utilities for the rest of the file

(defun add-auto-mode (mode regexp &optional noquote)
  "Shortcut for adding entries to `auto-mode-alist'
MODE is a symbol naming an emacs major mode.
REGEXP is the filename pattern to match, per `auto-mode-alist'.
NOQUOTE means not to treat the regexp as a filename extension.
If NOQUOTE is nil, puts puts \\. and $ around REGEXP."
  (unless noquote
    (setq regexp (concat "\\." regexp "$")))
  (add-to-list 'auto-mode-alist
               (cons regexp mode)))

(defun regexp-choice (&rest regexps)
  "Turns REGEXPS into a single regexp that matches any of them."
  (concat "\\(?:" (mapconcat #'identity regexps "\\|") "\\)"))


;;;; NOTE: These are needed for emacs22 and beyond.
(custom-set-faces
 '(font-lock-warning-face
   ((t (:background "black" :foreground "red" :weight bold)))))

(defun add-font-lock-width-warning (&optional mode-name)
  (font-lock-add-keywords
   mode-name
     '(("^[^\n]\\{80\\}\\(.*\\)$" 1
        font-lock-warning-face prepend))))

(defun warn-about-80-lines ()
  (add-font-lock-width-warning))

(defun make-newline-indent ()
  (define-key c-mode-base-map [ret] 'newline-and-indent))

;;; Autoloads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C/C++ configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove .c binding from auto-mode-alist (replace w/ c++)
(setq auto-mode-alist
 (mapcar
   (lambda (x)
     (if (eq (cdr x) 'c-mode)
       (cons (car x) 'c++-mode)
       x))
   auto-mode-alist))
(add-hook 'c-mode-common-hook 'warn-about-80-lines)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'show-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Java configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook 'warn-about-80-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-auto-mode 'python-mode "py")
(add-hook 'python-mode-hook 'warn-about-80-lines)
(add-hook 'python-mode-hook 'make-newline-indent)
(add-hook 'python-mode-hook 'show-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Shell configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Text configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; wrap lines in text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wrap it up
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modes)
