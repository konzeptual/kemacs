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



;;
;;; Flymake
;;

;; -- Make Flymake sane in Tramp --
;; from http://github.com/mrflip/emacs-starter-kit/blob/4feb7dee32df94e91c6b6d44527b937d0f108057/mrflip-defuns.el
(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.
 
For the use of PREFIX see that function.
 
Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-ruby-init ()
  (condition-case er
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         ;; 'flymake-create-temp-inplace
			 'flymake-create-temp-intemp
			 ))
             (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
        (list rails-ruby-command (list "-c" local-file)))
    ('error ())))

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (flymake-ruby-init)
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


(add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippets-rails"))
(require 'yasnippets-rails)


(provide 'kemacs-ruby)
;;; kemacs-ruby.el ends here

