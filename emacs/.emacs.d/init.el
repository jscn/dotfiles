;;; package ---- Summary
;;;
;;; Commentary:
;;;
;;; This is my init.el.
;;; There are many like it, but this one is mine.

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Add melpa to our list of sources for packages.
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

;; Added by Package.el. This must come before configurations of installed
;; packages. Don't delete this line. If you don't want it, just comment it out
;; by adding a semicolon to the start of the line.
(package-initialize)

;; Bootstrap `use-package` to automatically install missing packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Initialise other packages. Again, do this as early as possible to
;; ensure that the packages are loaded and ready before we apply any
;; of our own customisations.

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode t))

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package elm-mode
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package geiser
  :ensure t)

(use-package ido
  :ensure t
  :config
  (ido-mode t))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package tramp
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package go-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package paredit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package protobuf-mode
  :ensure t)

(defconst evnex-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "evnex-style" evnex-protobuf-style t)))

(use-package prettier-js
  :ensure t)

(defun setup-tide-mode()
  "Configure tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (local-set-key (kbd "C-c t i") 'tide-organize-imports)
  (local-set-key (kbd "C-c t s") 'tide-rename-symbol))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck exec-path-from-shell)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Documents/.journal/"
        org-journal-date-format "%A, %d %B %Y"))


;; Scala setup

;;;; Enable scala-mode for highlighting, indentation and motion commands

(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;;;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
   )

(use-package lsp-mode
  :ensure t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         :config (setq lsp-prefer-flymake nil))

;;;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t
  :config (setq lsp-metals-treeview-show-when-views-received t))

;;;; Enable nice rendering of documentation on hover
(use-package lsp-ui
  :ensure t)

;;;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak the visual aspects of the UI. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ;; No splash screen.

;; Set the font.
(if (eq window-system 'x)
    (set-frame-font "Liberation Mono 10"))

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

;; Store backups and auto-saved files in TEMPORARY-FILE-DIRECTORY
;; (which defaults to /tmp on Unix), instead of in the same directory
;; as the file.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; never use tabs, always use spaces.
(setq-default indent-tabs-mode nil)

;; enable <Shift+arrow> to move between windows.
(windmove-default-keybindings)

;; Attempt to be able to use tramp to access files on a docker container
;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
;; Note: this came from http://www.emacswiki.org/emacs/TrampAndDocker
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" return a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))


;; Configure web-mode
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vsl?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-php-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; javascript mode
(setq js-indent-level 4)

;; Relatedly, use the local installation of `standard` for flychecking JavaScript
;; stolen from https://github.com/lunaryorn/old-emacs-configuration/blob/27719411de8207e51690f14baffbab27a5b26f66/init.el
;; (defun jscn-use-js-executables-from-node-modules ()
;;   "Set executables of JS checkers from local node modules."
;;   (-when-let* ((file-name (buffer-file-name))
;;                (root (locate-dominating-file file-name "node_modules"))
;;                (module-directory (expand-file-name "node_modules" root)))
;;     (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
;;                                            (javascript-eslint . "eslint")
;;                                            (javascript-standard . "standard")
;;                                            (javascript-jscs   . "jscs")))
;;       (let ((package-directory (expand-file-name module module-directory))
;;             (executable-var (flycheck-checker-executable-variable checker)))
;;         (when (file-directory-p package-directory)
;;           (set (make-local-variable executable-var)
;;                (expand-file-name (concat ".bin/" module)
;;                                  package-directory)))))))
;; (jscn-use-js-executables-from-node-modules)

;; Actually, just run this, replacing the path with the one you want:
;; (setq flycheck-javascript-standard-executable "/home/jscn/work/self-manage/node_modules/.bin/standard")

;; org mode customisations

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-hide-emphasis-markers t) ;; hide '/' and '*' around emphasised text

;; Add GTD files to the agenda.
(setq org-agenda-files '("~/org/gtd/inbox.org"
                         "~/org/gtd/gtd.org"
                         "~/org/gtd/tickler.org"))

;; C-c c adds todos to inbox.org
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

;; C-c C-w refiles inbox.org to gtd, someday or tickler.
(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; bullets!

;; use • in place of * and -
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; org-babel language support

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (sh . t)
   (python . t)
   (sqlite . t)
   ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(elm-format-on-save t)
 '(package-selected-packages
   (quote
    (lsp-metals sbt-mode scala-mode protobuf-mode prettier-js org-journal exec-path-from-shell exec-path-from-shell-initialize dap-typescript elm-mode yasnippet company-lsp company company-mode dap-java lsp-java dap-mode lsp-treemacs helm-lsp lsp-mode lsp-ui geiser rust-mode paredit cider flycheck nginx-mode go-mode org-bullets graphviz-dot-mode dockerfile-mode markdown-mode apropospriate-theme git-timemachine feature-mode yaml-mode web-mode use-package solarized-theme projectile magit autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
