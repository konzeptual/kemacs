;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; PEAR style
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
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           c-mode c++-mode objc-mode
                                           latex-mode plain-tex-mode
                                           php-mode yaml-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))
(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                           c-mode c++-mode objc-mode
                                           latex-mode plain-tex-mode
                                           php-mode yaml-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

;; super stuff for autocomplete
(require 'yasnippet)
(yas/initialize)
(setq yas-dir "~/.emacs.d/lisp/yasnippet")
(yas/load-directory (concat yas-dir "/snippets"))

;; Trick to have my own changes to the snippets.
;; Everything in my-snippet directory overwrites default one.
(setq yas/root-directory (concat yas-dir "/my-snippets"))
(yas/load-directory yas/root-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;compilation window shall scroll down
(setq compilation-scroll-output 1)
;;compile button
(global-set-key [f11] 'user-save-and-make-all)
(global-set-key [f12] 'user-save-and-make-install)


;; general compile function call "make all"
;;(defun *default-pathname-defaults* ("./"))
;; (defun user-save-and-make-all (&optional makePath)
;;   "save and call compile"
;;   (interactive)
;;   (save-buffer)
;;   (compile "make -k install")
;;   (compile "make -k install -C ../build_32/dilept/"))
;; (compile *default-pathname-defaults*)
;; (message "make -k executed!"))

(defun user-make-target (target)
  " Compile as 'make target'. The function calls make on
Makefiles in the current directory if it is readable. If not, it
tries to call make all on Makefile in the parent directory in the
case if sources are in some subdirectories from the Makefile. It
returns 1 if call was from the current directory, 2 if from
parent directory and nil if no Makefile found"
  (interactive)
  (if (file-readable-p "Makefile")
      (progn
        (compile (concat "make " target))
        'make-target-curr)
                                        ;    (if (file-readable-p "../build_32/flexiTask/Makefile")
                                        ;       (progn
                                        ;         (compile (concat "make -k -C ../build_32/flexiTask/ " target))
                                        ;         'make-target-parent)
    (if (file-readable-p "../build/TRD/Makefile")
        (progn
          (compile (concat "make -C ../build/TRD/ " target))
          'make-target-parent)
      nil)))

(defun user-save-and-make-install ()
  " Save and call compile as 'make install'. The function calls make
on Makefiles in the current directory if it is readable. If not,
it tries to call make all on Makefile in the parent directory in
the case if sources are in some subdirectories from the
Makefile."
  (interactive)
  (save-buffer)
  (let ((status (user-make-target "install")))
    (if (null status)
        (message "Error: No Makefiles both in current and in the parent directories!")
      (if (eq status 'make-target-curr)
          (message "Make install executed!")
        (message "Make install executed in the parent directory!")))))

(defun user-save-and-make-all ()
  (interactive)
  (save-buffer)
  (let ((status (user-make-target "all")))
    (if (null status)
        (message "Error: No Makefiles both in current and in the parent directories!")
      (if (eq status 'make-target-curr)
          (message "Make all executed!")
        (message "Make all executed in the parent directory!")))))

;; show parenthesis (corresponding brackets)
(show-paren-mode t)
