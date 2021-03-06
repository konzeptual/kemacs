;; Setup variable for directory with dot-files
;; Used in customization packages later
(defconst dotfiles-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor/"))

;; Add directory recursievly to the load path
;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir (concat dotfiles-dir "vendor/"))
;;            (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))

;; Load my customization packages
(require 'kemacs-defuns)
(require 'kemacs-russification)
(require 'kemacs-gui)
(require 'kemacs-navigation)
(require 'kemacs-anything)
(require 'kemacs-development)
(require 'kemacs-webdev)
(require 'kemacs-ruby)
(require 'kemacs-rails)
(require 'kemacs-org)
(require 'kemacs-bindings)
(require 'kemacs-utils)
(require 'kemacs-misc)
(require 'kemacs-registers)
(require 'kemacs-test)

(setq  user-mail-address "antipin.konstantin@googlemail.com"
       user-full-name "Konstantin Antipin"
       )

;; Global settings done via emacs-config
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(safe-local-variable-values (quote ((encoding . utf-8) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


