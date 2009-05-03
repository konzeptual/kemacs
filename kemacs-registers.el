;;; kemacs-registers.el --- Set up registers
;;
;; Part of the Emacs Starter Kit

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?r (file . ,(concat dotfiles-dir "kemacs-registers.el")))
             (?w (file . "/var/www/osoznan/wordpress/wp-content/plugins/vibrhost"))
             (?s (file . "/home/kons/dev/sf_vibrhost/"))
             (?r (file . "/home/kons/dev/depot/"))
             (?e (file . "/home/kons/.emacs.d/"))
	     ))
  (set-register (car r) (cadr r)))

(provide 'kemacs-registers)

;;; kemacs-registers.el ends here
