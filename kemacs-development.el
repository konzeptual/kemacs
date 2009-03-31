;;; kemacs-development.el --- Configuration for better coding
;;
;; Part of kEmacs

(require 'kemacs-defuns)

;;; Development

;;will make "Ctrl-k" kills an entire line if the cursor is
;;at the beginning of line -- very useful.
(setq kill-whole-line t)
;; will delete "hungrily" in C mode! Use it to see what it does -- very useful.
(setq c-hungry-delete-key t)
;; will let emacs put in a "carriage-return" for you automatically after
;;left curly braces, right curly braces, and semi-colons in "C mode" -- very useful.
(setq c-auto-newline 0)

;;compilation window shall scroll down
(setq compilation-scroll-output 1)

;; insert doxygen-style comments before function
(fset 'insert-function-comment
      [?\C-a ?\C-j ?\C-p tab ?/ ?* ?* ?\M-j ?\M-j ?@ ?r ?e ?t ?u ?r ?n ? ?v ?o ?i ?d ?  ?\M-j backspace ?/ ?\C-p ?\C-p ?  ])
(global-set-key (kbd "\e:") 'insert-function-comment)



;; Styling indentation and other
(setq c-default-style
      '((java-mode . "java") (other . "cc-mode")))

(defun php-mode-hook ()
  (setq tab-width 4
        c-basic-offset 4
        c-hanging-comment-ender-p nil
        indent-tabs-mode
        ))

;; enjoy automatic indentation of yanked text in the listed programming modes
;; Great stuff!
(let ((yank-indent-modes '(
			   emacs-lisp-mode scheme-mode lisp-mode
                           c-mode c++-mode objc-mode latex-mode
			   plain-tex-mode php-mode yaml-mode
			   lisp-interaction-mode
			   )))
  (defadvice yank (after indent-region activate)
    (if (member major-mode yank-indent-modes)
        (let ((mark-even-if-inactive t))
          (indent-region (region-beginning) (region-end) nil))))

  (defadvice yank-pop (after indent-region activate)
    (if (member major-mode yank-indent-modes)
        (let ((mark-even-if-inactive t))
          (indent-region (region-beginning) (region-end) nil)))))

;; super stuff for autocomplete
(add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippet/"))

(require 'yasnippet)
(yas/initialize)
(setq yas-dir (concat dotfiles-dir "vendor/yasnippet"))
(yas/load-directory (concat yas-dir "/snippets"))
;; Trick to have my own changes to the snippets.
;; Everything in my-snippet directory overwrites default one.
(setq yas/root-directory (concat yas-dir "/my-snippets"))
;; (message yas-dir)
(yas/load-directory yas/root-directory)


;;compilation window shall scroll down
(setq compilation-scroll-output 1)

;; show parenthesis (corresponding brackets)
(show-paren-mode t)

;;; Lisp
;; Evaluate current lisp buffer
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(add-to-list 'auto-mode-alist '(".stumpwmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.el[c]" . emacs-lisp-mode))

;;; setup automatic header updates
(require 'header2)
(defsubst header-usage ()
  "Insert \"Usage: \" line."
  (insert (concat (section-comment-start) "Usage: \n")))

(defsubst header-installation ()
  "Insert \"Installation: \" line."
  (insert (concat (section-comment-start) "Installation: \n")))

(defsubst header-lisp-installation ()
  "Insert info, that I usually put in my emacs lisp file"
  (insert header-prefix-string "1. Put " (get-buffer-name) " to some directory.\n"
	  header-prefix-string "   (for example ~/.emacs.d/vendor).\n"
	  header-prefix-string "2. In your .emacs add this directry to the load-path like this:\n"
	  header-prefix-string "   (add-to-list 'load-path (expand-file-name \"~/.emacs.d/vendor\"))\n"
	  header-prefix-string "3. Add the following code to your .emacs file:\n"
	  header-prefix-string "   (require '" (get-buffer-name 1) ")\n"))

(setq make-header-hook '(
                         ;;header-mode-line
                         header-title
                         header-blank
                         ;;header-file-name
                         ;;header-description
                         ;;header-status
                         header-author
                         ;;header-maintainer
                         ;;header-copyright
                         header-creation-date
                         ;;header-rcs-id
                         header-version
                         ;;header-sccs
                         header-modification-date
                         header-modification-author
                         header-update-count
                         header-url
                         header-keywords
                         ;;header-compatibility
                         header-blank
                         header-lib-requires
                         header-end-line
                         header-commentary
                         header-blank
                         header-blank
                         header-blank
                         header-end-line
			 header-installation
                         header-blank
			 header-lisp-installation
                         header-blank
                         header-end-line
			 header-usage
                         header-blank
                         header-blank
                         header-end-line
                         header-history
                         header-blank
                         header-blank
                         ;; header-rcs-log
                         header-end-line
                         header-free-software
                         header-code
                         header-eof
                         ))


(provide 'kemacs-development)
;;; kemacs-development.el ends here
