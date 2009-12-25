;;; kemacs-defuns.el --- Custom functions
;;
;; Part of kEmacs

;; http://rlazo.supersized.org/archives/82-Insert-a-path-into-the-current-buffer.html
(defun insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (expand-file-name file)))

;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(defun indendt-whole-buffer ()
  "indent whole buffer"
  (interactive)
  ;;  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  ;;  (untabify (point-min) (point-max))
  )

;; http://davidavraamides.net/files/mode-aware-google-help-in-emacs.html
(defun search-site-url (keyword &optional site inurl lucky)
  "Do a Google search for KEYWORD. Restrict to SITE and INURL, if specified.
Jump to best match (I Feel Lucky) if LUCKY set.
"
  (concat "http://www.google.com/"
          (format "search?q=%s" (url-hexify-string keyword))
          (if site (format "+site:%s" (url-hexify-string site)))
          (if inurl (format "+inurl:%s" (url-hexify-string inurl)))
          (if lucky "&btnI")))

(defun ask-google (start end)
  "Open browser, request google with string.
if there is active region - use it
if not - use current 'thing'(command/word/etc).
"
  (interactive "r")
  (browse-url
   (search-site-url (if (eq mark-active nil)
                        (thing-at-point 'symbol)
                      (buffer-substring start end)
                      ))))

(defun get-file-basename (file-name)
  "Substitute basename from file-name.
"
  (substring file-name 0 (- (length file-name)
                            (length (file-name-extension file-name))
                            1)
             ))

(defun get-buffer-name (&optional strip-extension)
  "Return current buffer name.
If buffer is associated with the file - get filename only, without path.
If not  - just buffer name.
If strip-extension is not nil - remove file extension.
"
  (let ((name (if (buffer-file-name)
                  (file-name-nondirectory (buffer-file-name))
                (buffer-name))))
    (if (not (eq strip-extension nil))
        (get-file-basename name)
      name
      )
    ))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d/%m/%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d %B %Y, %A")))
	;;        (system-time-locale "ru_RU")
	)
    (insert (format-time-string format))))

(defun coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (setq save-place t)
  (auto-fill-mode) ;; in comments only
  (if window-system (hl-line-mode t))
  )



(defun dh ()
  "Connect to the osoznan.ru"
  (interactive)
  (find-file "/ssh:konsty@osoznan.ru:")
  )

(defun aldan ()
  "Connect to the aldan_intoVps server"
  (interactive)
  (find-file "/ssh:konsty@89.33.197.84:/home/konsty")
  )


;;; Window management
;;; 
;; http://www.emacswiki.org/emacs/HorizontalSplitting
(defun window-split-horizontally-if-alone ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
	     (not (active-minibuffer-window)))
    (split-window-horizontally)))

;; Now emacs will split window horizontally for temporary buffers.
(add-hook 'temp-buffer-setup-hook 'window-split-horizontally-if-alone)


(defun switch-to-buffer-other-window-horizontal ()
  "Replacement for switch-to-buffer-other-window.
It does the same, but splits window horizontally."
  (interactive)
  (progn
    (window-split-horizontally-if-alone)
    (ido-switch-buffer-other-window)
    ))


(defun find-file-other-window-horizontal ()
  "Replacement for find-file-other-window.
It does the same, but splits window horizontally."
  (interactive)
  (progn
    (window-split-horizontally-if-alone)
    (ido-find-file-other-window)
    ))


(defun fake-stdin-slurp (filename)
  "Emulate stdin slurp using emacsclient hack"
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (insert-file filename)
  (end-of-buffer))

(defun set-input-method-russian ()
  ""
  (set-input-method 'russian-computer)
  )

(provide 'kemacs-defuns)
;;; kemacs-defuns.el ends here
