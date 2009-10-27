;;; kemacs-org.el --- Configure orgmode
;;
;; Part of kEmacs

(require 'kemacs-defuns)

;; Add to the first plase in the list
(add-to-list 'load-path (concat dotfiles-dir "vendor/org-mode/lisp"))

(require 'org)

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Furthermore, you must activate font-lock-mode in Org buffers
(global-font-lock-mode t)

(setq org-return-follows-link t)

;; remember-mode
;;--------------------------------------------------------------------

;; Setup remember-mode to quickly catch ideas and journaling
(org-remember-insinuate)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)

;; setup templates for remember mode
(setq org-remember-templates
      '(
        ("Next"    ?n "* ACTIVE %?\n\n %^{Effort}p \n %i"   "todo.org"     "Todo list")
        ("Todo"    ?t "* TODO %?\n\n %^{Effort}p \n %i"   "todo.org"     "Todo list")
        ("Idea"    ?i "* %?\n  %i\n  %a"           "ideas.org"    "New Ideas")
        ("Emacs"   ?e "* %?\n  %i\n  %a"           "tech.org"     "Learning emacs")
        ("Ubuntu"  ?u "* %?\n  %i\n  %a"           "tech.org"     "Ubuntu-box")
        ("Songs"   ?s "* %?\n  %i\n"               "songs.org")
        ))

;; Use russian layout in remember-mode
(add-hook 'org-remember-mode-hook 'set-input-method-russian)
(add-hook 'org-agenda-mode-hook 'set-input-method-russian)

;; start the clock if there is a ACTIVE todo tag in template
(setq org-remember-clock-out-on-exit nil)
(add-hook 'org-remember-before-finalize-hook 'my-start-clock-if-needed)
(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "* ACTIVE" nil t)
      ;; (change-todo-state-on-old-clock)
      (org-clock-in))))

(defadvice org-clock-in (after sacha activate)
  "Set task's status to 'ACTIVE' when starting clock."
  (org-todo "ACTIVE"))


;; Use all files at the org-directory + more for agenda view
(setq org-agenda-files (append (file-expand-wildcards (concat org-directory "/[a-zA-Z]*.org"))))

;; Use ido whe possible
(setq org-completion-use-ido t)

;; What tags do I frequently use
(setq org-tag-alist '((:startgroup . nil)
                      ("тех" . ?n)
                      ("будни" . ?,)
                      ("отношения" . ?j)
                      (:endgroup . nil)
                      ))

;; Manage TODO states
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)")
        ))


(setq org-global-properties '(("Effort_ALL" . "0:05 0:10 0:15 0:20 0:30 0:40 
0:50 1:00 1:30 2:00 2:30 3:00")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Skip sheduled if deadline is shown
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Always start on the current day
(setq org-agenda-start-on-weekday nil)

(setq org-clock-sound "/usr/share/sounds/purple/login.wav")

(setq org-agenda-custom-commands
      '(("g" "GTD Block Agenda"
	 ((todo "NEXT|ACTIVE" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags "PROJECT/-TODO-NEXT-ACTIVE-WAITING-SOMEDAY-DONE-CANCELED"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "вхост|ольга|впоход|vhost/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "тех|имакс|оргмод|tech|emacs|orgmode/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "ап|фан|up/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "будни|мыло|everyday/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "отношения|relations/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "вне|out/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  (tags-todo "-вхост-ольга-впоход-vhost-тех-имакс-оргмод-tech-emacs-orgmode-ап-фан-up-будни-мыло-everyday-отношения-relations-вне-out/+TODO"((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	  ))
	
	("G" "GTD Review"
	 ((tags-todo "вхост|ольга|впоход|vhost/-TODO-NEXT-ACTIVE")
	  (tags-todo "тех|имакс|оргмод|tech|emacs|orgmode/-TODO-NEXT-ACTIVE")
	  (tags-todo "ап|фан|up/-TODO-NEXT-ACTIVE")
	  (tags-todo "будни|мыло|everyday/-TODO-NEXT-ACTIVE")
	  (tags-todo "отношения|relations/-TODO-NEXT-ACTIVE")
	  (tags-todo "вне|out/-TODO-NEXT-ACTIVE")
	  (tags-todo "-вхост-ольга-впоход-vhost-тех-имакс-оргмод-tech-emacs-orgmode-ап-фан-up-будни-мыло-everyday-отношения-relations-вне-out/-TODO-NEXT-ACTIVE")))
	))

;; Shortcut to the gtd agenda 
(defun org-agenda-gtd ()
  ""
  (interactive)
  (org-agenda "" "g") 
  )
(global-set-key "\C-cg" 'org-agenda-gtd)

;; ;; Easy incrase of effort estimate.
;; (add-hook 'org-agenda-mode-hook
;; 	  (lambda ()
;; 	    (org-defkey org-agenda-mode-map "E" 'org-clock-modify-effort-estimate)
;; 	    ))

;; When clock-out, change state to "NEXT".
;; We should do it more clever. Does not work together with clock-out-when-done
;; (setq org-clock-out-switch-to-state "NEXT")

(provide 'kemacs-org)
;;; kemacs-org.el ends here



