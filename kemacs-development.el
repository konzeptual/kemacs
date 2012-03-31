;;; kemacs-development.el --- Configuration for better coding;
;; Part of kEmacs

(require 'kemacs-defuns)

;;; Development

;;will make "Ctrl-k" kills an entire line if the cursor is
;;at the beginning of line -- very useful.
(setq kill-whole-line t)
;; will delete "hungrily" in C mode! Use it to see what it does -- very useful.
(setq c-hungry-delete-key t)
;; will let emacs put in a "carriage-return" for you automatically after
;;left curly braces, right curly braces, and semi-colons in "C mode" 
;; (setq c-auto-newline 0)

;;compilation window shall scroll down
(setq compilation-scroll-output 1)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank then comment out current line.
"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "^[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)

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
(setq yank-indent-modes '(
                          emacs-lisp-mode lisp-mode lisp-interaction-mode
                                          c-mode c++-mode
                                          php-mode 
					  ruby-mode html-mode rhtml-mode
					  css-mode html-mode
                                          ))

(defadvice yank (after indent-region activate)
  (if (member major-mode yank-indent-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode yank-indent-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;; super stuff for autocomplete
(add-to-list 'load-path (concat dotfiles-dir "vendor/yasnippet/"))

(require 'yasnippet)
;;(yas/initialize)
(setq yas-dir (concat dotfiles-dir "vendor/yasnippet"))
(yas/load-directory (concat yas-dir "/snippets"))
;; Trick to have my own changes to the snippets.
;; Everything in my-snippet directory overwrites the default one.
(setq yas/root-directory (concat yas-dir "/my-snippets"))
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

;;; Auto-complete-mode
;;;
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
;; start completion when entered 3 characters
(setq ac-auto-start 3)

(require 'auto-complete-yasnippet)
(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

(setq ac-modes
      (append ac-modes
	      '(yaml-mode
		eshell-mode
		rhtml-mode
		fundamental-mode
		rspec-mode
		)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

(add-hook 'lisp-interaction-mode
	  (lambda ()
	    (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

(add-hook 'rhtml-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

;;(require 'auto-complete-ruby)

;; (add-hook 'ruby-mode-hook
;; 	  (lambda ()
;; 	    (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer)) 
;; 	    (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))
;; ))

;; (setq ruby-mode-hook nil)

(add-hook 'fundamental-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-yasnippet))))


;; bash
(add-to-list 'auto-mode-alist '("\.bash.*" . shell-script-mode))

(provide 'kemacs-development)
;;; kemacs-development.el ends here
