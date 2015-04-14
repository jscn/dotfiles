;; Base configuration goes here. Any sections which get unruly should
;; be broken out into their own file. Override settings for a
;; particular machine with a hostname.el file in the ~/.emacs.d
;; directory.

;; Turn off mouse interface early in startup to avoid momentary display.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Initialize cask for handling installation of required
;; packages. This needs to be done early so that we can be sure all
;; the required packages are installed.
(require 'cask "~/.cask/cask.el")
(cask-initialize)


;; Tweak the visual aspects of the UI.

(setq inhibit-startup-message t) ;; No splash screen.

;; Set the font.
(if (eq window-system 'x)
    (set-default-font "Liberation Mono 10"))

(load-theme 'solarized-light t) ;; Set the theme.

(global-hl-line-mode +1) ;; Always highlight the current line.
(global-linum-mode 1)    ;; Always display the line number in the left margin.
(column-number-mode 1)   ;; Always display the column number in the modeline.

(setq fill-column 79) ;; Don't allow lines > 79 character in length.

;; I'm always accidentally hitting the exit sequence, so I want to be
;; asked to confirm before exiting.
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)


;; Prevent saving of backup (*~) files.
(setq make-backup-files nil)


;; ido
(require 'ido)
(ido-mode t)

;; autoapair
(require 'autopair)
(autopair-global-mode t)


;; never use tabs, always use spaces.
(setq-default indent-tabs-mode nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
