;;; kemacs-webdev.el --- Configuration for better webdevelopment
;;
;; Part of kEmacs

;;; PHP
(require `php-mode)

;; check basic syntax on the fly
(require 'flymake-php)
(add-hook 'php-mode-user-hook 'flymake-php-load)

(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  )
(add-hook 'php-mode-hook 'wicked/php-mode-init)


;; (load (concat dotfiles-dir "vendor/nxhtml/autostart.el"))

;;; PO
;; Convinient editon of po-files (translation)
(autoload 'po-mode "po-mode")
(setq auto-mode-alist (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))

;;; YAML
;; Convenient editing of yaml-files
;;(require 'yaml-mode)
;;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; Markdown
;;  convinient blogging
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
;; Switch on longlines-mode.
(add-hook 'markdown-mode-hook 'longlines-mode)

(add-to-list 'auto-mode-alist '(".radius$" . html-mode))
(add-to-list 'auto-mode-alist '(".txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '(".md$" . markdown-mode))
(add-to-list 'auto-mode-alist '(".markdown$" . markdown-mode))

;; The following is for proper handling russian text in preview in markdown mode
(defadvice markdown
  (after decorate-xhtml activate)
  "Add proper XHTML headers and footers to markdown output"
  (save-excursion
    (set-buffer "*markdown-output*")
    (copy-region-as-kill (point-min) (point-max))
    (goto-char (point-min))
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
            "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
            "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
            "<head>\n<title>\n</title>\n</head>\n\n"
            "<body>\n\n")
    (goto-char (point-max))
    (insert "\n"
            "</body>\n"
            "</html>\n")))


(provide 'kemacs-webdev)
;;; kemacs-webdev.el ends here

