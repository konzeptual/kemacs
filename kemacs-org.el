;;; kemacs-org.el --- Configure orgmode
;;
;; Part of kEmacs
(add-to-ordered-list 'load-path (concat dotfiles-dir "vendor/org-mode/lisp") 0)
(require 'org) 

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Furthermore, you must activate font-lock-mode in Org buffers
                                        ;(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only
(global-font-lock-mode t)

(setq org-return-follows-link t)

;; Setup remember-mode to quickly catch ideas and journaling
(org-remember-insinuate)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; setup templates for remember mode
(setq org-remember-templates
      '(
	("Journal" ?j "* NEXT %U %?\n\n  %i\n"     "journal.org")
        ("Todo"    ?t "* TODO %?   %^g\n  %i\n "   "todo.org"     "Todo list")
        ("Idea"    ?i "* %?\n  %i\n  %a"           "ideas.org"    "New Ideas")
        ("Emacs"   ?e "* %?\n  %i\n  %a"           "tech.org"     "Learning emacs")
        ("Ubuntu"  ?u "* %?\n  %i\n  %a"           "tech.org"     "Ubuntu-box")
        ("Songs"   ?s "* %?\n  %i\n"               "songs.org")
	))

;; Use all files at the org-directory + more for agenda view
(setq org-agenda-files (append (file-expand-wildcards (concat org-directory "/[a-zA-Z]*.org"))
			'("~/sketches/drafts/viborHost/viborHostinga.org")))

;; Use ido whe possible
(setq org-completion-use-ido t)

;; quick overview of my commonly used tags in agenda
(setq org-agenda-custom-commands
      '(("G" . "GTD contexts")
        ("Gk" "kProject" tags-todo "kProject")
        ("Gu" "up" tags-todo "up")
        ("Gt" "Tech" tags-todo "tech")
        ("Ge" "Everyday Life" tags-todo "everyday")
        ("Gr" "Relations" tags-todo "relations")
        ("Go" "Outside" tags-todo "out")
        ("GO" "GTD Overview"
         (
	  (tags-todo "kProject/-TODO-NEXT")
          (tags-todo "up/-TODO-NEXT")
          (tags-todo "tech/-TODO-NEXT")
          (tags-todo "out/-TODO-NEXT")        
          (tags-todo "relations/-TODO-NEXT")
	  (tags-todo "everyday/-TODO-NEXT")
          (tags-todo "-kProject-up-tech-out-relations-everyday/-TODO-NEXT")
	  )
         nil                      ;; i.e., no local settings
	 )
	("g" "GTD Block Agenda"
         (
	  (todo "NEXT")
	  (tags-todo "kProject/+TODO")
          (tags-todo "up/+TODO")
          (tags-todo "tech/+TODO")
          (tags-todo "out/+TODO")        
          (tags-todo "relations/+TODO")
	  (tags-todo "everyday/+TODO")
          (tags-todo "-kProject-up-tech-out-relations-everyday/+TODO")
	  )
         nil                      ;; i.e., no local settings
	 )
	;; ..other commands here
        ))

;; What tags do I frequently use
(setq org-tag-alist '((:startgroup . nil)
                      ("kProject" . ?k)
		      ("up" . ?u)
                      ("tech" . ?t)
		      ("out" . ?o)
		      ("relations" . ?r)
		      ("everyday" . ?e)
                      (:endgroup . nil)
		      ))

;; Manage TODO states
(setq org-todo-keywords
      `((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)")))
       
;; Add associated tags suuport for org-mode
(require 'org-assoc-tags)
(setq org-assoc-tags '(
		       ("emacs" "tech")
		       ("orgmode" "emacs" "tech")
		       ))

(provide 'kemacs-org)
;;; kemacs-org.el ends here

