;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NICE BEHAVIOIUR MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

(toggle-fullscreen)

;; nice looking theme. Good for my eyes
(require 'color-theme)
(setq color-theme-is-global t)
;;(color-theme-xemacs)
(color-theme-charcoal-black)

;; will allow you to type just "y" instead of "yes" when you exit.
(fset 'yes-or-no-p 'y-or-n-p)
;;will make the display of date and time persistent.
;;(setq display-time-day-and-date nil)
(display-time)
;;will trucate lines if they are too long.
(setq-default truncate-lines t)

;; get rid of the toolbar on top of the window
(tool-bar-mode 0)
;; delete scrollbar
(scroll-bar-mode -1)
;; delete menu bar
(menu-bar-mode -1)

;; Show column number at bottom of screen
(column-number-mode 1)

;; no splash screen at startup
(setq inhibit-startup-message t)

;; turn on font-lock mode
;;(when (fboundp 'global-font-lock-mode)
;;  (global-font-lock-model t))

;; Always end a file with a newline
(setq require-final-newline t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;;my favourite scrolling
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)


;;Change backup behavior to save in a directory, not in a miscellany
;;of files all over the place.
(setq
 backup-by-copying t                    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))                  ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; use versioned backups

;; cursor does not blink
(blink-cursor-mode nil)
