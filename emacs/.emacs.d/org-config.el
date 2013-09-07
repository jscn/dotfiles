;; org-mode configuration

(org-remember-insinuate)
(setq org-use-fast-todo-selection t)
(setq org-hide-leading-star t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAIT" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-agenda-files (list "~/Dropbox/org/gtd/todo.org"
                             "~/Dropbox/org/gtd/thecut.org"))

;; remember mode config
; TODOs will be added to projects.org "Tasks" section
;("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" 
; "/home/jscn/Dropbox/org/projects.org" "Tasks")
