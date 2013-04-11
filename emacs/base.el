;; Base configuration goes here. Any sections which get unruly should
;; be broken out into their own file. Override settings for a
;; particular machine with a hostname.el file in the ~/.emacs.d
;; directory.

;; Font configuration
(if (eq window-system 'x)
    (set-default-font "Liberation Mono 10"))

;; Don't allow lines > 79 character in length.
(setq fill-column 79)

;; color-theme settings
(color-theme-molokai)

;; The fingers you have used to exits are too fat.
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)
