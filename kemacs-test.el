;;; kemacs-test.el --- Ongoing testing of my emacs-setup.
;;
;; Part of kEmacs

;; Edit grep results
;; Opens files, edit and saves them from grep-buffer.
(require 'grep-ed)

(add-to-list 'load-path (concat dotfiles-dir "vendor/cucumber.el/"))
(require 'cucumber-mode)

(add-to-list 'load-path (concat dotfiles-dir "vendor/rspec-mode.el"))
(require 'rspec-mode)

(provide 'kemacs-test)
;;; kemacs-test.el ends here
