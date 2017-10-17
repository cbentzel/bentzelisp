;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file has my personal settings for a bunch of Emacs
;;; customization variables.  It mostly pertains to settings that are
;;; native to Emacs, as opposed to add-on packages, which I generally
;;; configure in "modes.el".
;;;
;;; Author:  Chris Bentzel (from Steve Yegge)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Chris Bentzel")

;; useful for shorter functions
(defun call-if-bound (func &rest args)
  "Calls FUNC passing it ARGS if func is fbound."
  (if (fboundp func)
      (apply func args)))

(call-if-bound 'transient-mark-mode t) ;; show the mark visually
(call-if-bound 'show-paren-mode t) ;; match parentheses visually
(call-if-bound 'menu-bar-mode -1) ;; turn off the menu bar
(call-if-bound 'tool-bar-mode -1) ;; turn off the tool bar
(call-if-bound 'scroll-bar-mode -1) ;; turn off the scroll bar
(call-if-bound 'line-number-mode t) ;; show line numbers
(call-if-bound 'column-number-mode t) ;; show column numbers
(call-if-bound 'delete-selection-mode 1) ;; auto-replace text when marked up
(call-if-bound 'global-font-lock-mode t) ;; color the fonts
(call-if-bound 'windmove-default-keybindings) ;; quick keyboard moves
(setq confirm-kill-emacs 'yes-or-no-p) ;; Don't exit emacs without asking
(setq visible-bell t) ;; I hate beeps
(setq require-final-newline t) ;; add final newline if missing
(setq truncate-partial-width-windows nil)  ;; line-wrap all long lines
(setq inhibit-startup-message t) ; turn off initial splashscreen
(setq-default indent-tabs-mode nil) ; don't use tabs for indentation
(setq message-log-max 10000) ; increase message log size
(setq history-length 1000)  ; increase history size (default is 30)
(setq completion-ignore-case t) ;; ignore case when completing names
(setq read-file-name-completion-ignore-case t) ;; emacs22
(setq history-delete-duplicates t)  ;; emacs22
(put 'scroll-left 'disabled nil)

;; Put backup files in a single directory to prevent tilde explosion
(add-to-list 'backup-directory-alist '("." . "~/bak")) 

;; give buffers unique names
(defun uniquify-buffers ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
(uniquify-buffers)

;; Force git pager to NULL so it works well within shell-mode
(setenv "GIT_PAGER" "")

;; Set up emacsserver/emacsclient
(server-start)
;; TODO(cbentzel): This needs to change based on which version of emacs is being run.
(if (eq system-type 'darwin)
  (setenv "EDITOR" "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
  (if (eq system-type 'windows-nt)
    (setenv "EDITOR" "emacsclient.exe")
    (setenv "EDITOR" "emacsclient")))

;; Set up exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'my-config)
