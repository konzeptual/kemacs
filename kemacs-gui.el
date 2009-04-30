;;; kemacs-gui.el --- Configure GUI. menus, colors, etc...
;;
;; Part of kEmacs

;; Put frame in the fullscreen mode.
;;(set-frame-parameter nil 'fullscreen 'fullboth)

;; nice looking theme. Good for my eyes
(require 'color-theme)
(setq color-theme-is-global t)
;;(color-theme-xemacs)
(color-theme-charcoal-black)

;; will allow you to type just "y" instead of "yes" when you exit.
(fset 'yes-or-no-p 'y-or-n-p)
;;will make the display of date and time persistent.
;;(display-time)

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




;; cursor does not blink
(blink-cursor-mode -1)

;; Highlight line with the cursor.
;; http://www.emacsblog.org/2007/04/09/highlight-the-current-line/
;; and you must do the above after loading the color theme
(global-hl-line-mode 1)
(set-face-background 'hl-line "#444")
(set-face-foreground 'highlight nil)

;; Change cursor to "bar cursor" and
;; notify when idle, overwrite, read-only and input mode
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(require 'cursor-chg)  ; Load the library
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode

(setq longlines-wrap-follows-window-size t)


(provide 'kemacs-gui)
;;; kemacs-gui.el ends here

