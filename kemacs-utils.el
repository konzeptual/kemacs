;;; kemacs-utils.el --- Utilities for emacs
;;
;; Part of kEmacs

;; Automatic install and update emacs-lisp packages
(require 'auto-install)
(setq auto-install-directory (concat dotfiles-dir "vendor/"))

;; TRAMP mode
(setq tramp-default-method "ssh")
(setq tramp-default-user "antipin")
;; Use the same policy for autosave as we do it locally.
(setq tramp-backup-directory-alist backup-directory-alist)

;;; Dired
;;
;; plugin for dired.
;; Open files with external apps with C-Enter
(require 'run-assoc)
(setq associated-program-alist
      '(("gnochm" "\\.chm$")
	("evince" "\\.pdf$")
	("evince" "\\.ps$")
	("eog" "\\.jpg$")
	("totem" "\\.mov$")
	("totem" "\\.mp3$")
	("totem" "\\.avi$")
	("totem" "\\.mkv$")
	))

					;	((lambda (file)
					;	   (browse-url (concat "file:///" (expand-file-name file)))) "\\.html?$")))

;; sort plugin for dired.
(require 'dired-sort-map)

;; Try to guess target directory
(setq dired-dwim-target t)

;; wget interface for Emacs
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t) 
(setq wget-download-directory "~/downloads")
;;  If current directory is under the wget-download-directory, set
;; download directory current dir.  Otherwise, ask download directory. 
(setq wget-download-directory-filter #'wget-download-dir-filter-current-dir)


;; Enhancement on term. Allow multiple instances of terminal + bonuses.
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Mode for editing oddmuse wiki (for example EmacsWiki)
(require 'yaoddmuse)
(setq yaoddmuse-username "KonstantinAntipin")

(add-to-list 'load-path (concat dotfiles-dir "vendor/git-emacs"))
(require 'git-emacs)

(add-to-list 'load-path (concat dotfiles-dir "vendor/autotest"))
(require 'autotest)

;;; Shell-mode
;; http://snarfed.org/space/why I don't run shells inside Emacs
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )
;; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

;; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
	  (lambda () (run-at-time 3 nil
				  (lambda () (delete-windows-on "*Completions*")))))


;; Calendar
(setq calendar-week-start-day 0
      calendar-day-name-array ["Понедельник" "Вторник" "Среда" "Четверг"
			       "Пятница" "Суббота" "Воскресенье"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" 
				 "Июнь" "Июль" "Август" "Сентябрь"
				 "Октябрь" "Ноябрь" "Декабрь"])

(provide 'kemacs-utils)
;;; kemacs-utils.el ends here
