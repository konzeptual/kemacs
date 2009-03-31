;;; kemacs-webdev.el --- Configuration for better webdevelopment
;;
;; Part of kEmacs

;;; PHP
(require `php-mode)

;; check basic syntax on the fly
(require 'flymake-php)
(add-hook 'php-mode-user-hook 'flymake-php-load)

;;; PO
;; Convinient editon of po-files (translation)
(autoload 'po-mode "po-mode")
(setq auto-mode-alist (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))

;;; YAML
;; Convenient editing of yaml-files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; Markdown
;;  convinient blogging
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.txt" . markdown-mode) auto-mode-alist))

;; The following is for proper handling russian text in preview in markdown mode
(defadvice markdown
  (after decorate-xhtml activate)
  "Add proper XHTML headers and footers to markdown output"
  (save-excursion
    (set-buffer "*markdown-output*")
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

