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


(provide 'kemacs-utils)
;;; kemacs-utils.el ends here
