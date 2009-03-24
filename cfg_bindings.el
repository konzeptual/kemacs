;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON KEYs defenition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bind changing layout of keyboard
(global-set-key [(control ?')] 'user-toggle-input-method)

;; convenient command. kill all buffers except current.
(global-set-key "\C-c\C-u" 'kill-other-buffers)

;; scroll up-down bindings. (check out cfg_navigation.el)
(global-set-key [(control shift n)] 'scroll-one-line-up)
(global-set-key [(control shift p)] 'scroll-one-line-down)

;; The default C-x b is too slow. Use C-; instead
(global-set-key [(control ?;)] 'ido-switch-buffer)
