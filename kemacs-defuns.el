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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It is tea time!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Origin comes from http://www.hack.org/mc/files/.emacs.el
(defun tea-show-remaining-time ()
  "Show how much time is left. If timer is not started - say it."
  (if (not (tea-timer-is-active))
      (message "Timer is not yet started.")
    (let* (
	   (remaining-time (decode-time (time-subtract (timer--time tea-active-timer) (current-time))))
	   (remaining-seconds (nth 0 remaining-time))
	   (remaining-minutes (nth 1 remaining-time))
	   )
      (message "%d min %d sec left" remaining-minutes remaining-seconds)
      )
    ))

(defun tea-timer-cancel ()
  (interactive)
  "Cancel currenly running tea-timer. If not running - do nothing."
  (if (tea-timer-is-active)
      (progn
	(cancel-timer tea-active-timer)
	(makunbound 'tea-active-timer)
	)
    ))

(defun tea-timer-is-active ()
  "Check if we have a running tea-timer."
  (and (boundp 'tea-active-timer) (< (float-time) (float-time (timer--time tea-active-timer))))
  )

(defun tea-time (timeval)
  "Ask how long the tea should draw and start a timer.
Cancel prevoius timer, started by this function"
  (interactive "sHow long (min)? ")
  (if (not (string-match "\\`\\([0-9]+\\)\\'" timeval))
      (tea-show-remaining-time)
    (let* ((minutes (string-to-int (substring timeval (match-beginning 1)
					      (match-end 1))))
	   (seconds (* minutes 60)))
      (progn
	(tea-timer-cancel)
	(setq tea-active-timer (tea-timer seconds))
	)))
  )

(defun show-notification (notification)
  "Show notification. Use mumbles."
  (if (not (start-process "tea-time-mumble-notification" nil "mumbles-send" notification))
      (message notification)
    ))

(provide 'kemacs-defuns)
;;; kemacs-defuns.el ends here
