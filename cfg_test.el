;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easy navigating through windows. With arrows.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;(require 'anything)
;;(require 'anything-config)

;; Enhancement on term. Allow multiple instances of terminal + bonuses.
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; sort plugin for dired.
(require 'dired-sort-map)

;; icicles installer
;; (load "~/.emacs.d/lisp/icicles-install")

;; "correct" order of the buffers with multiple frames
;; Do not show buffers from the other frames first.
(require 'flobl)

;; http://www.shellarchive.co.uk/2008-12-11-no-more-yasnippet-pop-up-menus.html
;; Yasnippet is great but for snippets with multiple hits
;; I really don’t like pop-up menus so I use this instead:
(setq yas/window-system-popup-function
      (setq yas/text-popup-function
            (lambda (templates)
              (ido-completing-read "snippet: "
                                   (mapcar (lambda (i)
                                             (yas/template-name
                                              (cdr i)))
                                           templates))))))
