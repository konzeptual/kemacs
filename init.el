;; Add directory recursievly to the load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Load additional config files
(defconst emacs-config-dir "~/.emacs.d/" "")
(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat emacs-config-dir file)))
    (message "Loaded config file: %s" file)
    ))

(load-cfg-files '(
		  "cfg_defuns"
		  "cfg_rusification"
		  "cfg_gui"
		  "cfg_navigation"
                  "cfg_development"
                  "cfg_webdev"
		  "cfg_orgmode"
		  "cfg_bindings"
		  "cfg_utils"
		  "cfg_test"
		  ))

; Global settings done via emacs-config
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-enabled nil t)
 '(ido-everywhere t)
 '(nxhtml-skip-welcome t)
 '(org-agenda-files (quote ("~/gtd/org/notes.org")))
 '(php-mode-force-pear t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

