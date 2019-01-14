;; Emacs configuration
;;
;; Author: harsha
;; Version 0.1.0
;; Installation
;; **** TODO *****
;; Notes
;; **** TODO *****
;;
;;

(defun setup-package-archives ()
  ;; This function configure the package repositories and packages
  (setq package-archives (quote (("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages")
                                 ("org" . "https://orgmode.org/elpa/")
                                 ("marmalade" . "https://marmalade-repo.org/packages/"))))
  (package-initialize))

(defun setup-ido-mode ()
  ;; This configures the ido mode
  (ido-mode))

(defun setup-startup ()
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)
  (require 'bookmark)
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*"))

(defun setup-meta-key ()
  ;; This configures meta key
  (when (eq system-type 'darwin)
    (setq mac-command-modifier (quote meta))
    (setq mac-option-modifier (quote super))))

(defun setup-encoding ()
  "Configures encoding"
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8))

(defun setup-shell-path ()
  ;; Adds `PATH` environment variable to emacs session (i.e. This is for shells)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun setup-fonts ()
  ;; Configures the font
  (when (memq window-system '(x))
    (set-frame-font "Inconsolata 12"))
  (when (memq window-system '(mac ns))
    (set-frame-font "Inconsolata 18")))

(defun setup-appearance ()
  ;; Configure the appearance
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (tool-bar-mode -1)
    (scroll-bar-mode 0)
    (fringe-mode 0)
    (load-theme 'tsdh-dark)))

(setup-appearance)

(defun setup-directory ()
  ;; Configure directory for backup and auto save
  (setq backup-directory-alist (backquote ((".*" . ,temporary-file-directory))))
  (setq auto-save-file-name-transforms (backquote ((".*" ,temporary-file-directory)))))

(defun setup-hooks ()
  ;; Setup hooks
  ;; This is before save hook, it deletes trailing whitespace & tabs before saving file
  (add-hook (quote before-save-hook) (lambda ()
                                      (unless (eq major-mode (quote fundamental-mode))
                                        (delete-trailing-whitespace)
                                        (untabify (point-min) (point-max))))))

(defun setup-org-mode ()
  ;; Configures org mode
  ;; @ -> note, ! -> timestamp, @/! -> note with timestamp
  ;; Directory where we keep .org files
  (setq org-directory "~/Dropbox/SecondBrain")
  ;; Diary file
  (setq diary-file (expand-file-name "diary" org-directory))

  ;; List of todo keywords
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  ;; Tags
  (setq org-tag-alist (quote ((:startgrouptag)(:grouptags)("@office" . ?o)("@personal" . ?p)(:endgrouptag)
                              (:startgrouptag)(:grouptags)("@development" . ?d)("@debugging" . ?D)("@research" . ?r)("@study" . ?s)("actvity" . ?a)("@entry" . ?e)(:endgrouptag)
                              (:startgrouptag)(:grouptags)("@high" . ?h)("@low" . ?l)("@medium" . ?m)(:endgrouptag)
                              (:startgrouptag)(:grouptags)("WAITING" . ?x)("HOLD" . ?x)("CANCELLED" . ?x)("ATTACH" . ?x)("FLAGGED" . ??)(:endgrouptag))))

  ;; Tag faces
  (setq org-tag-faces (quote (
                       ("@office" . (:foreground "GoldenRod" :weight bold))
                       ("@personal" . (:foreground "GoldenRod" :weight bold))
                       ("@high" . (:foreground "IndianRed1" :weight bold)))))

  ;; Todo faces
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

  ;; This does the following
  ;; Moving a task to CANCELLED adds a CANCELLED tag
  ;; Moving a task to WAITING adds a WAITING tag
  ;; Moving a task to HOLD adds WAITING and HOLD tags
  ;; Moving a task to a done state removes WAITING and HOLD tags
  ;; Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
  ;; Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
  ;; Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; (setq org-capture-templates `(
;;      ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;      ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;         "* %? [[%:link][%:description]] \nCaptured On: %U")
;;      ))
  ;; Capture templates
  (setq org-capture-templates (backquote(
                                         ("t" "TODO" entry (file+headline ,(expand-file-name "inbox.org" org-directory) "TASKS") "* TODO %i%?")
                                         ("T" "TICKLER" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "TICKLER") "* %i%?%U")
                                         ("a" "ARTICLE" plain (file capture-article-file) "#+TITLE: %^{Title}\n#+DATE: %<%Y-%m-%d>")
                                         ("p" "PROTOCOL" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "INBOX")
                                          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                         ("L" "PROTOCOL LINK" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "INBOX")
                                          "* %?[[%:link][%:description]] \nCaptured on: %U"))))


  (setq org-use-speed-commands t)
  ;; Refile
  (setq org-refile-use-outline-path (quote file))

  ;; Refile targets
  (setq org-refile-targets (backquote ((,(expand-file-name "gtd.org" org-directory) :maxlevel . 3)
                                       (,(expand-file-name "tickler.org" org-directory) :level . 1)
                                       (,(expand-file-name "someday.org" org-directory) :maxlevel . 2))))

  ;; Agenda files
  (setq org-agenda-files (list
                          (expand-file-name "inbox.org" org-directory)
                          (expand-file-name "gtd.org" org-directory)
                          (expand-file-name "tickler.org" org-directory)))

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        (quote (("o" "Office" ((agenda "" ((org-agenda-span 1))) (tags "@office") (todo "TODO"))))))

  ;; Don't show scheduled tasks which is done
  (setq org-agenda-skip-scheduled-if-done t)
  ;; Don't show deadline tasks which is done
  (setq org-agenda-skip-deadline-if-done t)
  ;; Compact blocks
  (setq org-agenda-compact-mode t)

  ;; Org babel uses tab from the lang
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)
  ;; Don't ask for confirmation while evaluating babel block
  (setq org-confirm-babel-evaluate nil)

  (setq org-blog-directory-name "articles")
  (setq org-blog-directory (expand-file-name org-blog-directory-name org-directory))

  ;; This function is used for capturing blog
  (defun capture-article ()
    (let* ((title (read-string "Slug: "))
           (slug (replace-regexp-in-string "[^a-z]+" "-" (downcase title)))
           (dir (format "%s/%s" org-blog-directory (format-time-string "%Y" (current-time)))))
      (if (equal (file-directory-p dir) nil)
          (if (equal (expand-file-name org-blog-directory-name org-directory) nil)
              (make-directory (expand-file-name org-blog-directory-name org-directory))
              )
          (make-directory dir))
      (let ((file (format "%s/%s.org" dir slug)))
            (expand-file-name file))))

  ;; Save all org files after refile
  (advice-add (quote org-refile) :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))

  (require 'org-crypt)
  ;; Encrypt before save
  (org-crypt-use-before-save-magic)
  ;; Encrypt todo's which is having crypt tag
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;; Mark in dairy
  (setq calendar-mark-diary-entries-flag t)

  ;; hooks
  (add-hook (quote org-finalize-agenda-hook) (lambda ()
                                              (setq appt-message-warning-time 10
                                                    appt-display-diary nil
                                                    appt-display-mode-line t
                                                    appt-display-format (quote window)
                                                    calendar-mark-diary-entries-flag t)
                                              (org-agenda-to-appt)
                                              (appt-activate 1)))

  ;; Include events from diary
  (setq org-agenda-include-diary t)

  ;; Include calfw
  (require 'calfw)
  (require 'calfw-org)

  ;; Org babel
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((ledger . t)
           (emacs-lisp . t)))))

(defun setup-org-protocol ()
  (require 'server)
  (unless (server-running-p)
    (server-start))
  (require 'org-protocol))

(defun setup-bindings ()
  "Key bindings"
  (global-set-key (kbd "C-`") (quote multi-term))
  (global-set-key (kbd "C-c 4") (quote insert-rupee))
  (global-set-key (kbd "C-c a") (quote org-agenda))
  (global-set-key (kbd "C-c c") (quote org-capture)))

(defun setup-term ()
  "Configures the term NOTE: not working"
  (add-hook (quote term-mode-hook) (lambda ()
                                     (local-set-key (kbd "C-p") (quote term-up))
                                     (local-set-key (kbd "C-n") (quote term-down)))))

;; (defun setup-alias ()
;;   )

(defun setup-company ()
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (global-company-mode 1))

;; (defun setup-git ()
;;   )

(defun setup-spell ()

  (add-hook (quote text-mode-hook) (lambda ()
                                     (flyspell-mode 1)
                                     ))

  (add-hook (quote conf-unix-mode-hook) (lambda ()
                                         (flyspell-prog-mode)))

  (add-hook (quote org-mode-hook) (lambda ()
                                    (flyspell-mode 1)))

  (add-hook (quote web-mode-hook) (lambda ()
                                    (flyspell-prog-mode))))

(defun setup-calendar ()
  ;; Calendar setup
  (setq calendar-latitude 12.9)
  (setq calendar-longitude 77.5)
  (setq calendar-location-name "Bengaluru, IN")  )

(defun setup-common ()
  (desktop-save-mode 1)
  (winner-mode 1)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

(defun setup-gpg ()
  ;; TODO More config required
  (require 'epa-file)
  (epa-file-enable))

(defun setup-file-extensions ()
  ;; Rest client
  (add-to-list (quote auto-mode-alist) (quote ("\\.rest\\'" . rest-client-mode)))
  ;; config file
  (add-to-list (quote auto-mode-alist) (quote ("\\.*rc$" . conf-unix-mode)))

  (dolist (ele (quote ("css" "scss" "less" "html" "xhtml" "dhtml" "js" "ts" "jsx" "tag" "json")))
    (let ((f (format "\\.%s\\'" ele)))
      (setq temp-var (backquote (,f . web-mode)))
      (add-to-list (quote auto-mode-alist) (backquote ,temp-var)))))

(defun setup-web-mode ()
  (add-hook (quote web-mode-hook) (lambda ()
                                    (setq web-mode-enable-comment-interpolation t)
                                    (setq web-mode-code-indent-offset 2)
                                    (setq web-mode-css-indent-offset 2)
                                    (setq web-mode-markup-indent-offset 2)
                                    (if (equal web-mode-content-type "javascript")
                                        (web-mode-set-content-type "jsx"))
                                    (add-to-list (quote web-mode-comment-formats) (quote ("jsx" . "/*")))
                                    (setq web-mode-comment-keywords
                                          (regexp-opt (append (cdr (assoc "comment" web-mode-extra-keywords))
                                                              (quote ("FIXME" "TODO" "BUG" "HACK")))))))
  )

(defun insert-rupee ()
  ;; Interactive function for inserting rupee symbol
  (interactive)
  (insert "₹"))

(defun setup-ledger ()
  ;; Configure ledger
  (add-hook (quote ledger-mode-hook) (lambda ()
                                       (local-set-key (kbd "$") (quote insert-rupee)))))

(defun setup-plantuml ()
  ;; Configure plantuml
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq org-plantum-jar-path plantuml-jar-path)
  (add-hook (quote plantuml-mode-hook) (lambda ()
                                         (setq tab-width 2))))


(defun setup ()
  (setup-package-archives)
  (setup-ido-mode)
  (setup-meta-key)
  (setup-shell-path)
  (setup-fonts)
  (setup-appearance)
  (setup-directory)
  (setup-hooks)
  (setup-bindings)
  (setup-calendar)
  (setup-common)
  (setup-gpg)
  (setup-file-extensions)
  (setup-web-mode)
  (setup-ledger)
  (setup-plantuml)
  (setup-org-mode)
  (setup-startup)
  (setup-spell)
  (setup-encoding)
  (setup-term)
  (setup-org-protocol))

(add-hook (quote after-init-hook) (quote setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-docview org-gnus org-habit org-info org-irc org-mhe org-protocol org-rmail org-w3m org-bookmark org-checklist org-drill org-learn org-screen)))
 '(package-selected-packages
   (quote
    (powerline yaml-mode web-mode sx plantuml-mode perspective org-pomodoro org-bullets multi-term magit logview ledger-mode json-mode jabber-otr ivy indium htmlize fold-this flymake-json exwm exec-path-from-shell eslint-fix company-web company-tern company-restclient calfw-org calfw-cal calfw borland-blue-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
