;;; kemacs-ruby.el --- Configuration related to ruby and Ruby on Rails
;;
;; Part of kEmacs

;; Half of the code from emacs-starter-kit

(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/rinari/util"))
(require 'rinari)
(require 'ri)

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-compilation)
     (setq ruby-use-encoding-map nil)
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-x hr") 'ri)
     ))

;; (global-set-key (kbd "C-h r") 'ri)

;; Rake files are ruby, too, as are gemspecs.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(add-hook 'ruby-mode-hook 'coding-hook)

;;; Flymake

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))


(require 'inf-ruby)
(require 'ruby-compilation)

;;;rhtml-mode
(add-to-list 'load-path "/home/kons/.emacs.d/vendor/rhtml/")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;; (add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippets-rails"))
;; (require 'yasnippets-rails)


(add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippets-rails"))
(require 'yasnippets-rails)


(provide 'kemacs-ruby)
;;; kemacs-ruby.el ends here

