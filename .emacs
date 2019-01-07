

(ido-mode)

(setq org-src-tab-acts-natively t)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(set-frame-font "Inconsolata 18")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(fringe-mode 0)

(load-theme 'tsdh-dark)

(setq org-directory "~/Dropbox/Tasks")

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-tag-alist (quote ((:startgroup)
			    ("@office" . ?o)
			    ("@personal" . ?p)
			    ("@activity" . ?a)			    
			    ("@important" . ?i)
			    (:endgroup)
			    ("crypt" . ?c)
			    ("WAITING" . ?w)
			    ("HOLD" . ?h)
			    ("NOTE" . ?n)
			    ("CANCELLED" . ?C)
			    ("FLAGGED" . ??))))

(setq org-agenda-files (list
			(expand-file-name "inbox.org" org-directory)
			(expand-file-name "gtd.org" org-directory)
			(expand-file-name "tickler.org" org-directory)))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(setq org-capture-templates (backquote(
				    ("t" "TODO" entry (file+headline ,(expand-file-name "inbox.org" org-directory) "TASKS") "* TODO %i%?")
				    ("T" "TICKLER" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "TICKLER" "* %i%?%U")))))

(setq org-refile-use-outline-path 'file)
(setq org-refile-targets (backquote (
				 (,(expand-file-name "gtd.org" org-directory) :maxlevel . 3)
				 (,(expand-file-name "tickler.org" org-directory) :level . 1)
				 (,(expand-file-name "someday.org" org-directory) :maxlevel . 2))))


(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Set autosave location
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
			    
