;; TRAMP mode
(setq tramp-default-method "ssh")
(setq tramp-default-user "antipin")

;; plugin for dired.
;; Open files with external apps with C-Enter
(require 'run-assoc)
(setq associated-program-alist
      '(("gnochm" "\\.chm$")
	("evince" "\\.pdf$")
	("evince" "\\.ps$")
	("eog" "\\.jpg$")
	("totem" "\\.mp3$")
	("totem" "\\.avi$")
	("totem" "\\.mkv$")
	))
;	((lambda (file)
;	   (browse-url (concat "file:///" (expand-file-name file)))) "\\.html?$")))

;; wget interface for Emacs
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t) 
(setq wget-download-directory "~/downloads")
;;  If current directory is under the wget-download-directory, set
;; download directory current dir.  Otherwise, ask download directory. 
(setq wget-download-directory-filter #'wget-download-dir-filter-current-dir)

;; tramp
(setq tramp-default-method "ssh")
