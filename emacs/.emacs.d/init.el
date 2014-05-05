;; Base configuration goes here. Any sections which get unruly should
;; be broken out into their own file. Override settings for a
;; particular machine with a hostname.el file in the ~/.emacs.d
;; directory.

;; Turn off mouse interface early in startup to avoid momentary display.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen.
(setq inhibit-startup-message t)

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

;; Don't allow lines > 79 character in length.
(setq fill-column 79)

;; Set up package repos
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; ido
(require 'ido)
(ido-mode t)
(autopair-global-mode t)

;; The fingers you have used to exits are too fat.
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; No need for backup files, thanks.
(setq make-backup-files nil)
