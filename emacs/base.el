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
(color-theme-zenburn)
