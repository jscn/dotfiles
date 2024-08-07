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

;; golang configuration. see https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


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

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))
;; enable typescript-tslint checker
;;(flycheck-add-mode 'typescript-tslint 'web-mode)

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

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package tramp
  :ensure t)

;; (use-package web-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(use-package nginx-mode
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

(use-package prettier
  :ensure t
  :config (global-prettier-mode))

(add-hook 'after-init-hook #'global-prettier-mode)

(defun setup-tide-mode()
  "Configure tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;;(tide-hl-identifier-mode +1)
  (company-mode +1)
  (prettier-mode +1))

(use-package tide
  :ensure t
  :after (company flycheck exec-path-from-shell)
  :hook ((typescript-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (tsx-ts-mode . tide-setup))
  :bind (("C-c t i" . tide-organize-imports)
         ("C-c t s" . tide-rename-symbol)
         ("C-c t r" . tide-references)
         ("C-c t c" . tide-refactor)
         ("C-c t j" . tide-jsdoc-template)
         ("C-c t f" . tide-fix)))

(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Documents/.journal/"
        org-journal-date-format "%A, %d %B %Y"))

(require 'go-mode)
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter Config ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode))
	;; (yaml-mode . yaml-ts-mode)
	;; (bash-mode . bash-ts-mode)
	;; (js2-mode . js-ts-mode)
	;; (json-mode . json-ts-mode)
	;; (css-mode . css-ts-mode)
	;; (python-mode . python-ts-mode))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweak the visual aspects of the UI. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ;; No splash screen.

;; Set the font.
(if (eq window-system 'x)
    (set-frame-font "Liberation Mono 12"))

(global-hl-line-mode +1) ;; Always highlight the current line.
(global-display-line-numbers-mode)    ;; Always display the line number in the left margin.
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

;; Store backups in TEMPORARY-FILE-DIRECTORY (which defaults to /tmp
;; on Unix), instead of in the same directory as the file.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Completely disable auto-save, it causes build processes which watch
;; a project (e.g., JavaScripte/TypeScript) to break.(
(setq auto-save-default nil)
;; Same for lockfiles
(setq create-lockfiles nil)

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

;; Move current line up or down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default))
 '(elm-format-on-save t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(prettier typescript-mode unicode-fonts add-node-modules-path tide lsp-metals sbt-mode scala-mode protobuf-mode org-journal exec-path-from-shell exec-path-from-shell-initialize dap-typescript elm-mode yasnippet company-lsp company company-mode dap-java lsp-java dap-mode lsp-treemacs helm-lsp lsp-mode lsp-ui geiser rust-mode paredit cider flycheck nginx-mode go-mode org-bullets graphviz-dot-mode dockerfile-mode markdown-mode apropospriate-theme git-timemachine feature-mode yaml-mode web-mode use-package solarized-theme projectile magit autopair auto-complete)))
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
