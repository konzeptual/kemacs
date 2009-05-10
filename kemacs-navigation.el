;;; kemacs-navigation.el --- Setup convenient navigation through emacs
;;
;; Part of kEmacs

;;; IDO
;; Great mode for browsing buffers and not only.
(require 'ido)
(ido-mode t)
(ido-everywhere t)

(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-enable-flex-matching t)
	    ))


;;; Smex
;; library for better M-x with ido
(add-to-list 'load-path (concat dotfiles-dir "vendor/smex"))
(require 'smex)
(eval-after-load "init.el" '(smex-initialize))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-save-file (concat dotfiles-dir ".smex.save"))


;; ;; set location of the bookmark file
;; (setq bookmark-default-file "~/.emacs.d/.emacs.bmk")

;; scroll one line at a time
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

;; "correct" order of the buffers with multiple frames
;; Do not show buffers from the other frames first.
(require 'flobl)

;;  automatically save places in files where you were editing.
;;  Restore them on the next visit
(require 'saveplace)
(setq save-place t)
(setq save-place-file (concat dotfiles-dir "places"))

;; Save a list of recent files visited.
(require 'recentf)
(recentf-mode 1)

;; Modification from
;; http://www.emacswiki.org/emacs-en/AutoIndentation
(defun kill-and-join-forward (&optional arg)
  "Replacement for the kill-line.
If we are at the end of the line - join with
previous and fix unneeded spaces."
    (interactive "P")
    (if (and (eolp) (not (bolp)))
	(delete-indentation t)
      (kill-line arg)))

(global-set-key "\C-k" 'kill-and-join-forward)

(provide 'kemacs-navigation)
;;; kemacs-navigation.el ends here

