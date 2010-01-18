;;; kemacs-test.el --- Ongoing testing of my emacs-setup.
;;
;; Part of kEmacs

;; Edit grep results
;; Opens files, edit and saves them from grep-buffer.
(require 'grep-ed)

;; (add-to-list 'load-path (concat dotfiles-dir "/home/kons/.emacs.d/vendor/yasnippets-rspec/"))
;; (require 'yasnippets-rspec)


;;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; in shell-mode prompt is read only
(setq comint-prompt-read-only t)

;; Usually I need rinary in git repos
(add-hook 'git-status-mode-hook 'rinari-minor-mode)

(provide 'kemacs-test)
;;; kemacs-test.el ends here
