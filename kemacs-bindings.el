;;; kemacs-bindings.el --- kEmacs bindings
;;
;; Part of kEmacs

(require 'kemacs-defuns)

;; bind changing layout of keyboard
;;(global-set-key [(control ?')] 'user-toggle-input-method)

;; convenient command. kill all buffers except current.
(global-set-key "\C-c\C-u" 'kill-other-buffers)

;; scroll up-down bindings. (check out cfg_navigation.el)
(global-set-key [(control shift n)] 'scroll-one-line-up)
(global-set-key [(control shift p)] 'scroll-one-line-down)

;; Evaluate current lisp buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Self-written function that toggles fullscreen
(global-set-key [(meta return)] 'toggle-fullscreen)

;; Bound C-h to backspace.
(global-set-key "\C-xh" 'help-command)
;; rebind help
(global-set-key "\C-h" 'delete-backward-char)
;; Expected bahavior of M-backspace
(global-set-key "\C-\M-h" 'backward-kill-word)

;; The default C-x b is too slow. Use C-; instead
(global-set-key [(control ?\;)] 'ido-switch-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f1>") 'menu-bar-mode)

;; A better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Replacements for standard functions.
;; We are forcing horizontal splitting.
(global-set-key "\C-x4b" 'switch-to-buffer-other-window-horizontal)
(global-set-key "\C-x4f" 'find-file-other-window-horizontal)

(global-set-key (kbd "C-x f") 'anything-for-files)
;;(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(global-set-key "\C-\M-\\" 'indendt-whole-buffer)

(provide 'kemacs-bindings)
;;; kemacs-bindings.el ends here
