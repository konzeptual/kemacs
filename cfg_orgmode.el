;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Furthermore, you must activate font-lock-mode in Org buffers
                                        ;(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only
(global-font-lock-mode t)

;; Setup remember-mode to quickly catch ideas and journaling
(org-remember-insinuate)
(setq org-directory "~/gtd/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; setup templates for remember mode
(setq org-remember-templates
      '(("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org")
        ("Idea" ?i "* %?\n  %i\n  %a" "ideas.org" "New Ideas")
        ("Emacs" ?e "* %?\n  %i\n  %a" "tech.org" "Learning emacs")
        ("Ubuntu-box" ?u "* %t %?\n  %i\n  %a" "tech.org" "Setup modifications of the ubuntu-box")
        ("Songs to download" ?s "* %?\n  %i\n" "songs.org")))


;; What files are used for agenda view
(setq org-agenda-files '("/notes.org"))	
