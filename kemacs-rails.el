;;; kemacs-rails.el --- everything I need to work with RoR
;;
;; Part of kEmacs

(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari/util"))
(require 'rinari)
(require 'ri)
(require 'inf-ruby)
(require 'ruby-compilation)

;; define some keys in rinari-mode
(add-hook 'rinari-minor-mode-hook
	  (lambda ()
	    (define-key rinari-minor-mode-map (kbd "C-c ; W") 'rinari-restart-web-server)
	    )
	  )

;; (interactive)
;; (if (buffer-exists "*rails*") (kill-buffer "*rails*"))
;; (rinari-web-server)
;; )

(defun rinari-restart-web-server (&optional edit-cmd-args)
  "Restart web server"
  (interactive "P")
  (let ((rinari-web-server-buffer "*rails*"))
    (if (get-buffer rinari-web-server-buffer)
	(progn
	  (set-process-query-on-exit-flag (get-buffer-process rinari-web-server-buffer) nil)
	  (kill-buffer rinari-web-server-buffer))
      nil)
    (rinari-web-server edit-cmd-args)))

;; Usually I need rinary in git repos
(add-hook 'git-status-mode-hook 'rinari-minor-mode)


;;;rhtml-mode
(add-to-list 'load-path "/home/kons/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

(add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippets-rails"))
(require 'yasnippets-rails)


(add-to-list 'load-path (concat dotfiles-dir "vendor/cucumber.el/"))
(require 'feature-mode)

(add-to-list 'load-path (concat dotfiles-dir "vendor/rspec-mode.el"))
(require 'rspec-mode)

(provide 'kemacs-rails)
;;; kemacs-rails.el ends here

