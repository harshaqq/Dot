;;; myemacs.el --- My emacs configuration
;; Author: harshaqq
;; Keywords: Emacs configuration
;; Version 0.1.0

;; Setup repository for packages
(setq package-archives '(("gnu" . "https//elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

;; Initialize packages
(package-initialize)

;; Enable evil mode
(evil-mode 1)

;; (with-eval-after-load 'python
;;   (defun python-shell-completion-native-try ()
;;     "Return non-nil if can trigger native completion."
;;     (let ((python-shell-completion-native-enable t)
;;           (python-shell-completion-native-output-timeout
;;            python-shell-completion-native-try-output-timeout))
;;       (python-shell-completion-native-get-completions
;;        (get-buffer-process (current-buffer))
;;        nil "_"))))

;; Fuzzy matching
;; (setq ido-enable-flex-matching t)


(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") 'ispell-word)

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "C-c p f") 'projectile-find-file)

(projectile-mode 1)
(setq projectile-keymap-prefix (kbd "C-c p"))

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;; Increase garbage collection value
(defvar harshaqq/gc-cons-threshold (* 20 gc-cons-threshold))
(setq gc-cons-threshold harshaqq/gc-cons-threshold)

(setq user-emacs-directory "~/.emacd.d/")

;; Startup
;; Disable startup screen
(setq inhibit-startup-screen t)

(setq speedbar-use-images nil)
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Menlo-12")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;(require 'yasnippet)
;; (require 'yasnippets)
;(yas-reload-all)
(add-hook 'ruby-mode-hook (lambda ()
			    ;; (yas-minor-mode)
			    (ruby-electric-mode)
			    (robe-mode)))

;; (require 'auto-complete)

;; (add-hook 'robe-mode-hook 'ac-robe-setup)

(require 'sr-speedbar)

(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 40)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 70)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-console 40)

(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)

  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    (sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
(ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))

;; Disable startup message
(setq inhibit-startup-message t)

;; Show bookmarks
(require 'bookmark)
(bookmark-bmenu-list)
;; (swith-to-buffer "*Bookmark List*")


;; MacOS command key as meta
;; TODO: Better to disable for desktop keyboards, need to add one condition
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
  ;; (setq mac-option-modifier 'super))

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; For macOS emacs doesnt load `PATH` and other environment
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

;; Font settings

(menu-bar-mode -1)

(when (display-graphic-p)
  (set-frame-font "Menlo 12")  
  (tool-bar-mode -1)
  (scroll-bar-mode 0)
  (fringe-mode 0)
  (load-theme 'late-night))

(setq backup-directory-alist (backquote ((".*" . ,temporary-file-directory))))
(setq auto-save-file-name-transforms (backquote ((".*" ,temporary-file-directory))))

;; Configures org mode
;; @ -> note, ! -> timestamp, @/! -> note with timestamp
;; Directory where we keep .org files
(setq org-directory "~/Dropbox/SecondBrain")

(setq org-archive-location (expand-file-name "archives/archive.org::"))

;; Diary file
(setq diary-file (expand-file-name "diary" org-directory))

;; TODO States
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p@/!)" "|" "DONE(d)")
			  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Tags
(setq org-tag-alist '((:startgrouptag)		      ;; The place task belongs to
		      (:grouptags)
		      ("@office" . ?o)
		      ("@home" . ?h)
		      ("@outdoor" . ?O)
		      (:endgrouptag)
		      (:startgrouptag)		      ;; Priority of the task		      
		      (:grouptags)
		      ("@high" . ?H)
		      ("@low" . ?L)
		      ("@medium" . ?M)
		      (:endgrouptag)
		      (:startgrouptag)		      ;; Complexity of the task
		      (:grouptags)
		      ("@easy" . ?e)
		      ("@moderate" . ?m)
		      ("@complex" . ?c)
		      (:endgrouptag)
		      (:startgrouptag)		      ;; Category		      
		      (:grouptags)
		      ("@development" . ?d)
		      ("@debugging" . ?D)
		      ("@communication" . ?y)
		      ("@followup" . ?f)
		      ("@training" . ?t)
		      ("@formality" . ?f)
		      ("@travelling" . ?T)
		      ("@vocabulary" . ?v)
		      ("@programming" . ?p)
		      ("@other" . ?u)
		      ("@issue" . ?i)
		      ("@enhancement" . ?w)
		      ("@note" . ?n)
		      ("@research" . ?r)
		      ("@activity" . ?a)
		      ("@entry" . ?E)
		      ("@lifestyle" . ?l)
		      ("@study" . ?S)
		      ("brain" . ?B)
		      ("crypt" . ?C)
		      ("@catchup" . ?Y)
		      ("@micro" . ?1)
		      ("@bank" . ?2)
		      ("@docs" . ?3)
		      ("taskjuggler_project" . ??)
		      (:endgrouptag)
		      (:startgrouptag)
		      (:grouptags)
		      ("WAITING" . ?x)
		      ("HOLD" . ?x)
		      ("CANCLLED" . ?x)
		      ("ATTACH" . ?x)
		      ("FLAGGED" . ??)
		      (:endgrouptag)))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "DarkOliveGreen1" :weight bold)
	("PROGRESS" :foreground "tomato1" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCLEED" :foreground "forest green" :weight bold)))

;; This does the following
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAITING adds a WAITING tag
;; Moving a task to HOLD adds WAITING and HOLD tags
;; Moving a task to a done state removes WAITING and HOLD tags
;; Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))



(defun generate-random-password-for-template ()
   (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
          (fname (org-hugo-slug title))
	    (mapconcat #'identity
                 `(,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                   ":EXPORT_FILE_NAME: index"
                   ":END:"
                   "%?\n")                ;Place the cursor here finally
                 "\n"))))

(defun capture-password ()
  (concat "* %^{Name} :crypt: \n " (replace-regexp-in-string "\n" "" (shell-command-to-string "pwgen -ncsy 15 1"))))

(defun capture-pin ()
  (setq pin/length (read-string "Length: "))
  (setq pin/commmand (concat "python -c " "'from random import randint;" "print(randint(" (number-to-string (expt 10 (- (string-to-number pin/length) 1))) "," (number-to-string (- (expt 10 (string-to-number pin/length)) 1)) "))'"))
  (concat "* %^{Name} :crypt: \n  " (replace-regexp-in-string "\n" "" (shell-command-to-string pin/commmand))))

(setq org-capture-templates `(
                              ("t" "TODO" entry (file+headline ,(expand-file-name "inbox.org" org-directory) "TASKS")
			       "* TODO %i%?")
                              ("T" "TICKLER" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "TICKLER")
			       "* %i%?%U")
                              ("a" "ARTICLE" plain (file capture-article-file)
			       "#+TITLE: %^{Title}\n#+DATE: %<%Y-%m-%d>")
			      ("r", "PASSWORD" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "SECRETS")
			       (function capture-password))
			      ("b", "BANK" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "SECRETS")
			       (function capture-pin))
			      ("o", "OBSERVATIONS" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "OBSERVATIONS")
			       "* %^{Title} :@note:\n** %^{Description}")
                              ("v" "VOCABULARY" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "VOCABULARY")
                               "* %^{Word} :drill:@note:@vocabulary: \n %t\n %^{Extended word (may be empty)} \n** Answer: \n%^{The definition}")
                              ("f" "FIXNEEDED" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "FIX NEEDED")
                               "* %^{Subject} :@issue: \n** %^{Description}")
                              ("n" "NOTES" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "NOTES")
                               "* %^{Title} :@note: \n** %^{Description}")
                              ("q" "QUESTIONS" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "QUESTIONS")
                               "* %^{Title} :drill:@question: \n  %^{Question} \n** Answer: \n   %^{Answer}")
                              ("p" "PROTOCOL" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "INBOX")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                              ("L" "PROTOCOL LINK" entry (file+headline ,(expand-file-name "tickler.org" org-directory) "INBOX")
                               "* %?[[%:link][%:description]] \nCaptured on: %U")))


;; Speed commands on heading
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

(setq org-agenda-custom-commands '(("x" agenda)
				   ("y" agenda*)
				   ("w" todo-tree "WAITING")
				   ("p" todo "PROGRESS")
				   ("c" todo "CANCELLED")))

(if (> (string-to-number (org-version)) 9.1)
    (require 'org-tempo))

(define-derived-mode web-javascript-mode web-mode "WebJS"
  "Major mode for editing web css templates."
  (web-mode)
  (setq web-mode-content-type "javascript"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ledger . t)
   (plantuml . t)
   (python . t)
   (C . t)
   (js . t)
   (calc . t)
   (go . t)
   (css . t)
   (ditaa . t)
   (shell . t)
   (screen . t)
   (html-chrome . t)
   (ruby . t)
   (io . t)
   (org . t)
   (restclient . t)
   (sass . t)
   (gnuplot . t)
   (css . t)
   (makefile . t)
   (java . t)   
   (emacs-lisp . t)))

;; (require 'org-drill)
;; (defun drill/vocabulary ()
;;   (interactive)
;;   (setq org-drill-question-tag "@vocabulary")p
;;   (setq org-drill-scope (list (expand-file-name "tickler.org" org-directory)))
;;   (call-interactively 'org-drill))

;; (defun drill/programming ()
;;   (interactive)
;;   (setq org-drill-question-tag "@programming")
;;   (setq org-drill-scope  (list (expand-file-name "tickler.org" org-directory)))
;;   (call-interactively 'org-drill))

  ;; Skip entries without toto state
(setq org-agenda-tag-filter-preset (quote ("-drill")))

;; Don't show scheduled tasks which is done
(setq org-agenda-skip-scheduled-if-done t)
;; Don't show deadline tasks which is done
(setq org-agenda-skip-deadline-if-done t)
;; Compact blocks
(setq org-agenda-compact-mode t)

(setq org-enforce-todo-dependencies t)

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
(defun capture-article-file ()
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

(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(setq epa-file-encrypt-to "virtualxi99@gmail.com")

(setq epa-file-select-keys nil)
(require 'org-crypt)
;; (require 'org-drill)
(require 'org-bullets)
;; Encrypt before save
(org-crypt-use-before-save-magic)
;; Encrypt todo's which is having crypt tag
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key "virtualxi99@gmail.com")


;; Mark in dairy
(setq calendar-mark-diary-entries-flag t)

;; Appointment settings
(require 'appt)
(setq appt-time-msg-list nil
      appt-display-diary nil
      appt-display-interval (quote 5)
      appt-display-format (quote window)
      appt-message-warning-time (quote 15)
      appt-display-mode-line nil)
(appt-activate t)
(display-time)

;; Bind org-agenda-to-appt to hook
(add-hook (quote org-agenda-finalize-hook)
	  (quote org-agenda-to-appt)
	  (org-agenda-columns))

;; Include events from diary
(setq org-agenda-include-diary t)

(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 9:30")
                                    ("STYLE_ALL" . "habit"))))

;; Include calfw
(require 'calfw)
(require 'calfw-org)

;; Change parent todo state to done, once all children are done
(add-hook (quote org-after-todo-statistics-hook) (lambda (n-done n-not-done)
                                                   (let (org-log-done org-log-states)   ; turn off logging
                                                     (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

(defun generate-password ()
  (interactive)
  (let ((x (shell-command-to-string "pwgen -y 15 1")))
    (insert x)))

(add-hook 'org-capture-mode-hook (lambda ()
				   (local-set-key (kbd "C-c p") 'generate-password)))



;; (setq org-tag-faces (quote (
;; 			    ("@office" . (:foreground "GoldenRod" :weight bold))
;; 			    ("@home" . (:foreground "GoldenRod" :weight bold))
;; 			    ("@high" . (:foreground "IndianRed1" :weight bold)))))
		      


(require 'server)
(unless (server-running-p)
(server-start))
(require 'org-protocol)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-c i") (quote -toggle-input-method))
(global-set-key (kbd "C-c w") (quote lookup-wiktionary))
(global-set-key (kbd "<f8>") (quote dictionary-lookup-definition))
(global-set-key (kbd "<f9>") (quote ispell-complete-word))
(global-set-key (kbd "C-`") (quote multi-term))
(global-set-key (kbd "C-c 4") (quote insert-rupee))
(global-set-key (kbd "C-c a") (quote org-agenda))
(global-set-key (kbd "C-c c") (quote org-capture))
(global-set-key (kbd "<f1>") 'org-decrypt-entry)
(global-set-key (kbd "<f12>") 'bookmark-bmenu-list)

(add-hook (quote term-mode-hook) (lambda ()
                                   (local-set-key (kbd "C-p") (quote term-up))
                                   (local-set-key (kbd "C-n") (quote term-down))))

(add-hook (quote text-mode-hook) (lambda ()
                                   (flyspell-mode 1)))

(add-hook (quote conf-unix-mode-hook) (lambda ()
                                        (flyspell-prog-mode)))

(add-hook 'python-mode-hook (lambda ()
			      (setq tab-width 2)))

(setq jdee-server-dir "~/jdee-server/")

(require 'eclim)
(setq eclimd-autostart 1)
(defun my-java-mode-hook ()
  (eclim-mode 1))
(add-hook 'java-mode-hook 'my-java-mode-hook)   

(defun harshaqq/conf-unix-mode-hook ()
  (flyspell-prog-mode))

(defun harshaqq/text-mode-hook ()
  (flyspell-mode 1))

(defun harshaqq/term-mode-hook ()
  (local-set-key (kbd "C-p") 'term-up)
  (local-set-key (kbd "C-n") 'term-down))

(defun harshaqq/org-mode-hook ()
  (require 'org-bullets)  
  (org-bullets-mode)
  (flyspell-mode 1)
  (org-tempo-setup))

(defun harshaqq/org-capture-mode-hook ()
  (org-bullets-mode))

(defun harshaqq/go-mode-hook ()
  (setq tab-width 2))

(add-hook 'go-mode-hook 'harshaqq/go-mode-hook)

(add-hook 'org-capture-mode-hook 'harshaqq/org-capture-mode-hook)
(add-hook 'org-mode-hook 'harshaqq/org-mode-hook)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook (quote web-mode-hook) (lambda ()
                                  (add-node-modules-path)
                                  (flyspell-prog-mode)))

(add-hook 'typescript-mode-hook (lambda ()
				  (add-node-modules-path)))



(setq calendar-latitude 12.9)
(setq calendar-longitude 77.5)
(setq calendar-location-name "Bengaluru, IN")

;; * Sunrise
;;   :PROPERTIES:
;;   :CATEGORY: Sunrise
;;   :END:
;; &%%(diary-sunrise-sunset)

(desktop-save-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(powerline-default-theme)
(setq windmove-wrap-around t)

;; Rest client
(add-to-list (quote auto-mode-alist) (quote ("\\.rest\\'" . restclient-mode)))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; config file
(add-to-list (quote auto-mode-alist) (quote ("\\.*rc$" . conf-unix-mode)))

;; Webmode
(dolist (ele (quote ("css" "scss" "less" "html" "xhtml" "dhtml" "js" "ts" "jsx" "tag" "json")))
  (let ((f (format "\\.%s\\'" ele)))
    (setq temp-var (backquote (,f . web-mode)))
    (add-to-list (quote auto-mode-alist) (backquote ,temp-var))))

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

(add-hook (quote ledger-mode-hook) (lambda ()
                                     (local-set-key (kbd "$") (quote insert-rupee))))

(setq plantuml-jar-path "~/plantuml.jar")
(setq org-plantuml-jar-path plantuml-jar-path)
(add-hook (quote plantuml-mode-hook) (lambda ()
                                       (setq tab-width 2)))

(winner-mode 1)
(progn
  (require 'windmove)
  ;; Use Shift+Arrow_Keys to move cursor around plit panes
  (windmove-default-keybindings)
  ;; When cursor is on edge, move to the other side
  (setq windmove-wrap-around t))

(defun insert-rupee ()
  ;; Interactive function for inserting rupee symbol
  (interactive)
  (insert "₹"))

;; true for kannada, nil for english
(defvar toggle-english-kannada-flag nil)

(defun -toggle-input-method ()
  (interactive)
  (defun -input-select-kannada ()
    (setq toggle-english-kannada-flag t)
    (set-input-method "kannada-itrans"))

  (defun -input-select-english ()
    (setq toggle-english-kannada-flag nil)
    (set-input-method "british"))

  (if (eq toggle-english-kannada-flag nil)
      (-input-select-kannada)
    (-input-select-english)))

(setq bookmark-default-file (expand-file-name "bookmarks" org-directory))

(require 'emms-setup)
(emms-standard)
(emms-all)
(emms-default-players)
(setq emms-stream-default-action "play")

(add-hook 'after-init-hook (lambda ()
			      (setq company-dabbrev-downcase 0)
			      (setq company-idle-delay 0)
			      (global-company-mode 1)
			      (add-to-list 'company-backends 'company-tern)
			      (add-to-list 'company-backends 'company-robe)
			      (require 'flycheck)

			      ;; turn on flychecking globally
			      (global-flycheck-mode)
			      
			      ;; disable jshint since we prefer eslint checking
			      (setq-default flycheck-disabled-checkers
					    (append flycheck-disabled-checkers
						    '(javascript-jshint)))

			      (setq-default flycheck-disabled-checkers
					    (append flycheck-disabled-checkers
						    '(ruby-rubylint)))			      
			      
			      ;; use eslint with web-mode for jsx files
			      (flycheck-add-mode 'javascript-eslint 'web-mode)
			      ;; (flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
			      (flycheck-add-mode 'typescript-tslint 'web-mode)
			      (flycheck-add-mode 'typescript-tslint 'typescript-mode)
			      (flycheck-add-mode 'ruby-rubylint 'ruby-mode)
			      (add-to-list 'flycheck-checkers 'ruby-rubocop)
			      
			      ;; customize flycheck temp file prefix
			      (setq-default flycheck-temp-prefix ".flycheck")
			      
			      ;; disable json-jsonlist checking for json files
			      (setq-default flycheck-disabled-checkers
					    (append flycheck-disabled-checkers
						    '(json-jsonlist)))
			      (require 'bookmark)
			      (bookmark-bmenu-list)
			      (company-quickhelp-mode)
			      (require 'company-web-html)
			      (switch-to-buffer "*Bookmark List*")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes
   (quote
    ("64326a40de94bb2e1501334e6fc7c618ef687eb2a5f5bf2978d43d23e6c26512" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" default)))
 '(cycle-themes-mode nil)
 '(debug-on-error t)
 '(ivy-mode t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-docview org-gnus org-habit org-info org-irc org-mhe org-protocol org-rmail org-w3m org-bookmark org-checklist org-learn org-screen)))
 '(org-src-lang-modes
   (quote
    (("arduino" . arduino)
     ("redis" . redis)
     ("php" . php)
     ("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("js" . web-javascript)
     ("javascript" . web-javascript))))
 '(package-selected-packages
   (quote
    (key-chord counsel projectile-speedbar undo-tree evil bundler ox-reveal justify-kp nov jenkins jtags log4j-mode typescript typescript-mode edit-server langtool flycheck-vale emms ob-kotlin go-playground govet ob-go dockerfile-mode go-mode persp-mode late-night-theme dad-joke cycle-quotes cycle-themes gnuplot pdf-tools org-mind-map org-brain ob-restclient smex synonyms smartparens org-kanban kanban add-node-modules-path js2-mode flycheck markdown-mode chess org-plus-contrib restclient org-drill-table org dictionary company powerline yaml-mode web-mode sx plantuml-mode perspective org-pomodoro org-bullets multi-term magit logview ledger-mode json-mode jabber-otr ivy indium htmlize fold-this flymake-json exwm exec-path-from-shell eslint-fix company-web company-tern company-restclient calfw-org calfw-cal calfw borland-blue-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dictionary-word-definition-face ((t (:family "Inconsolata")))))
