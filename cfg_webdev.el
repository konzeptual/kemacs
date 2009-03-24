;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB DEVELOPMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP-mode. indent on enter
;;php-mode.el
(load "php-mode")
(require `php-mode)
(define-key php-mode-map (kbd "RET") 'newline-and-indent)

                                        ;(load "~/.emacs.d/lisp/nxml-mode-20041004/rng-auto.el")

                                        ;(load "~/.emacs.d/lisp/nxhtml/autostart.el")

;; Convinient editon of po-files (translation)
(autoload 'po-mode "po-mode")
(setq auto-mode-alist (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))

;; Convenient editing of yaml-files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Markdown mode - for convinient blogging
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.txt" . markdown-mode) auto-mode-alist))

;; Set up a macro that insert comment in html style
(fset 'insert-html-comments
      "<!--   -->\C-b\C-b\C-b\C-b\C-b")
(global-set-key (kbd "\C-c\C-cc") 'insert-html-comments)

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
