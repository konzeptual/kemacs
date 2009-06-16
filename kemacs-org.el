;;; kemacs-org.el --- Configure orgmode
;;
;; Part of kEmacs

;; Add to the first plase in the list
(add-to-list 'load-path (concat dotfiles-dir "vendor/org-mode/lisp"))

(require 'org)

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; Furthermore, you must activate font-lock-mode in Org buffers
                                        ;(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only
(global-font-lock-mode t)

;; Switch on longlines-mode in org-mode
;; (add-hook 'org-mode-hook 'longlines-mode) ; Org buffers only

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
        ("Journal" ?j "* NEXT %?\n\n %^{Effort}p \n  %i"     "journal.org")
        ("Todo"    ?t "* TODO %?\n\n %^{Effort}p \n %i"   "todo.org"     "Todo list")
        ("Idea"    ?i "* %?\n  %i\n  %a"           "ideas.org"    "New Ideas")
        ("Emacs"   ?e "* %?\n  %i\n  %a"           "tech.org"     "Learning emacs")
        ("Ubuntu"  ?u "* %?\n  %i\n  %a"           "tech.org"     "Ubuntu-box")
        ("Songs"   ?s "* %?\n  %i\n"               "songs.org")
        ))

;; start the clock if there is a ACTIVE todo tag in template
(setq org-remember-clock-out-on-exit nil)
(add-hook 'org-remember-before-finalize-hook 'my-start-clock-if-needed)
(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "* ACTIVE" nil t)
      ;; (change-todo-state-on-old-clock)
      (org-clock-in))))


;; Use all files at the org-directory + more for agenda view
(setq org-agenda-files (append (file-expand-wildcards (concat org-directory "/[a-zA-Z]*.org"))))

;; Use ido whe possible
(setq org-completion-use-ido t)

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
      '(
        (sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)")
        ))


(setq org-global-properties '(("Effort_ALL" . "0:05 0:10 0:15 0:20 0:30 0:40 
0:50 1:00 1:30 2:00 2:30 3:00")))

;;; Obsololet. With smart search in agenda I do not need it anymore.
;; Add associated tags suuport for org-mode
;; (require 'org-assoc-tags)
;; (setq org-assoc-tags '(
;;                        ("emacs" "tech")
;;                        ("symfony" "tech")
;;                        ("orgmode" "emacs" "tech")
;;                        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Agenda-gtd is controlled by this list
(setq org-gtd-tags '(
                     (kProject {^k.*} vhost osru)
                     (up)
                     (tech symfony emacs orgmode stumpwm rails radiant)
                     (everyday email)
                     (relations)
                     (out)
                     ))

(defun get-org-gtd-review-setup (todo-filter)
  "Returns list, that is used to set up custom gtd-view in agenda.
Input: list of list of tags.
Output: formatted list for generating gtd-agenda, like this:
                         (tags-todo \"kProject/+TODO\")
                         (tags-todo \"up/+TODO\")
                         (tags-todo \"tech|symfony|emacs|orgmode/+TODO\")
                          ....
                         (tags-todo \"-kProject-up-tech-symfony-emacs-orgmode-...../+TODO\")
 "
  (let (
        (result ())
        (all-other ())
        )
    (dolist (cur-tag-list org-gtd-tags)
      (let (
            (cur-config (mapconcat 'symbol-name cur-tag-list "|"))
            )
        (progn
          (setq cur-config (cons (concat cur-config todo-filter) ()))
          (add-to-list 'cur-config 'tags-todo)
          (setq all-other (concat all-other "-" (mapconcat 'symbol-name cur-tag-list "-")))
          )
        (add-to-list 'result cur-config t)
        ))
    (setq all-other (cons 'tags-todo (cons (concat all-other todo-filter) ())))
    (add-to-list 'result all-other t)
    result
    ))

(setq org-agenda-custom-commands nil)
(setq org-gtd-review-setup (get-org-gtd-review-setup "/-TODO-NEXT-ACTIVE"))

(setq org-gtd-setup (cons '(todo "NEXT|ACTIVE") (cons '(tags "PROJECT/-TODO-NEXT-ACTIVE-WAITING-SOMEDAY-DONE-CANCELED") (get-org-gtd-review-setup "/+TODO"))))

(setq org-clock-sound "/usr/share/sounds/purple/login.wav")

;; Setup GTD views in agenda. constructed from variable org-gtd-tags
(add-to-list 'org-agenda-custom-commands (append '("G") '("GTD Review") (cons org-gtd-review-setup ())))
(add-to-list 'org-agenda-custom-commands (append '("g") '("GTD Block Agenda") (cons org-gtd-setup ())))

;; Shortcut to the gtd agenda 
(defun org-agenda-gtd ()
  ""
  (interactive)
  (org-agenda "" "g") 
  )
(global-set-key "\C-cg" 'org-agenda-gtd)

(provide 'kemacs-org)
;;; kemacs-org.el ends here



