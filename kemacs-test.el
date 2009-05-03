;;; kemacs-test.el --- Ongoing testing of my emacs-setup.
;;
;; Part of kEmacs

;; http://sachachua.com/wp/2008/07/30/emacs-and-php-tutorial-php-mode/
(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0))
(add-hook 'php-mode-hook 'wicked/php-mode-init)

;; Edit grep results
;; Opens files, edit and saves them from grep-buffer.
(require 'grep-ed)


(require 'rdebug)

;; (add-to-list 'load-path (concat dotfiles-dir "vendor/"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/git-emacs"))
(require 'git-emacs)



(provide 'kemacs-test)
;;; kemacs-test.el ends here
