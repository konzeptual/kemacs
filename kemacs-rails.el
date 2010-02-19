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

(defun rinari-restart-web-server ()
  "Restart web server"
  (interactive)
  (if (buffer-exists "*server*") (kill-buffer "*server*"))
  (rinari-web-server)
  )

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

