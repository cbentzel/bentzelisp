;;; ekeys.el - Key bindings for emacs, and some one-off functions

(require 'emacs-type)

;; I'm very used to C-z for undo, don't want to sleep emacs
(global-set-key "\C-z" 'undo)

;; I always type C-o to open files
(global-set-key "\C-o" 'find-file)

;; split three way
(defun split-three-ways ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

;; (global-set-key "\C-x w" 'split-three-ways)

;;
;; I hate typing Ctrl-x 5 0 to kill a frame, so I'm rebinding
;; the kill-emacs key to close the current frame if it's not
;; the only one.
;;
(defun kill-frame-or-emacs ()
  (interactive)
  (if (= (length (frame-list)) 1)
      (save-buffers-kill-emacs)
    (delete-frame (selected-frame))))

(global-set-key "\C-x\C-c"	'kill-frame-or-emacs)

(provide 'ekeys)
