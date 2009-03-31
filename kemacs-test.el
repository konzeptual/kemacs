;;; kemacs-test.el --- Ongoing testing of my emacs-setup.
;;
;; Part of kEmacs


;; TODO: make it working properly
;; http://www.shellarchive.co.uk/2008-12-11-no-more-yasnippet-pop-up-menus.html
;; Yasnippet is great but for snippets with multiple hits
;; I really don’t like pop-up menus so I use this instead:
;; (setq yas/window-system-popup-function
;;       (setq yas/text-popup-function
;;             (lambda (templates)
;;               (ido-completing-read "snippet: "
;;                                    (mapcar (lambda (i)
;;                                              (yas/template-name
;;                                               (cdr i)))
;; templates)))))

;; ;; Jump to a definition in the current file. (This is awesome.)
;; (global-set-key "\C-x\C-i" 'ido-imenu)

;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
;; (global-set-key (kbd "C-x C-p") 'find-file-at-point)



(defun context-help ()
  "Open a browser window showing documentation for the word under the point.
Uses `major-mode' to optionally refine the search to a specific web site,
or a specific pattern in the URL. Defaults to a simple keyword search.
Uses `search-site-url' to do the actual search.
"
  (interactive)
  (browse-url
   (apply 'search-site-url
          (thing-at-point 'symbol)
          (cond
           ((equal major-mode 'css-mode)
            '("www.w3schools.com" "/css/" t))
           ((equal major-mode 'emacs-lisp-mode)
            '("www.gnu.org" "/emacs/"))
           ((or (equal major-mode 'html-mode)
                (equal major-mode 'html-helper-mode))
            '("www.htmlquick.com" "/reference/" t))
           ((equal major-mode 'javascript-mode)
            '("www.w3schools.com" nil t))
           ((equal major-mode 'python-mode)
            '("docs.python.org" "/ref/" t))
           (t nil)))))

;;; Anything
;;;
(require 'anything-config)
(require 'anything-auto-install)
(require 'anything-etags)
(require 'anything-match-plugin)


;;; Auto-complete-mode
;;;
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; start completion when entered 3 characters
(setq ac-auto-start 3)

(require 'auto-complete-yasnippet)
(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)


;; (require 'etags-table)
;; (setq etags-table-alist
;;       (list
;;        '("/home/kons/coding/kSurvey/.*\\\\.php$" "/home/kons/coding/kSurvey/TAGS")
;;        ))
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.php\" | etags -" dir-name)))

;;(require 'auto-complete-gtags)

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (require 'php-completion)
;;             (php-completion-mode t)
;;             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)))

;; (add-hook  'php-mode-hook
;;            (lambda ()
;;              (when (require 'auto-complete nil t)
;;                (make-variable-buffer-local 'ac-sources)
;;                (add-to-list 'ac-sources 'ac-source-php-completion)
;;                ;; if you like patial match,
;;                ;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
;;                ;; (add-to-list 'ac-sources 'ac-source-php-completion-patial)
;;                (auto-complete-mode t))))
(provide 'kemacs-test)
;;; kemacs-test.el ends here
