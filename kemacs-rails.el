;;; kemacs-rails.el --- everything I need to work with RoR
;;
;; Part of kEmacs

(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari/util"))
(require 'rinari)
(require 'ri)
(require 'inf-ruby)
(require 'ruby-compilation)

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

