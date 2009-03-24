;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  RUSIFICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For faster startup
(modify-frame-parameters nil '((wait-for-wm . nil)))
;; Использовать окружение UTF-8
(set-language-environment 'UTF-8)
;; UTF-8 для вывода на экран
(set-terminal-coding-system 'utf-8)
;; UTF-8 для ввода с клавиатуры
(set-keyboard-coding-system 'utf-8)
;; UTF-8 для работы с буфером обмена X (не работает в emacs 21!)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Необходима поддержка кодировок cp866 и cp1251
                                        ;(codepage-setup 1251)
                                        ;(define-coding-system-alias 'windows-1251 'cp1251)
                                        ;(codepage-setup 866)
;; Установки автоопределения кодировок
;; prefer-coding-system помещает кодировку в НАЧАЛО списка предпочитаемых кодировок
;; Поэтому в данном случае первой будет определяться utf-8-unix
(prefer-coding-system 'cp866)
(prefer-coding-system 'koi8-r-unix)
(prefer-coding-system 'windows-1251-dos)
(prefer-coding-system 'utf-8-unix)
;; Клавиатурная раскладка "как в Windows" (не работает в emacs 21!)
(setq default-input-method 'russian-computer)

;; One can use alias emacs='emacs --font "-*-*-medium-r-*-*-13-*-*-*-*-*-iso10646-1"' instead. FASTER.
;; Set default font
                                        ;(cond (window-system
                                        ;       (setq default-frame-alist
                                        ;                                        ;            (append '((font . "-*-*-medium-r-*-*-13-*-*-*-*-*-iso10646-1"))
                                        ;             (append '((font . "-*-*-medium-r-*-*-20-*-*-*-*-*-iso10646-1"))
                                        ;                     default-frame-alist))
                                        ;       ))

(defun russian-spell ()
  "Switch to russian ispell dictionary and check the buffer"
  (interactive)
  (save-buffer)
  (ispell-change-dictionary "russian")
  (ispell-buffer))

(defun do-nothing ()
  "null function")

(defun user-toggle-input-method ()
  "Change the stadart function tuggle-input-method
to redefine keys for cyrillic input
from UNIX type to win type"
  (interactive)
  (toggle-input-method)
  (message "Keybord changed to %s" current-input-method))

(defun user-to-cyr ()
  "Change input method to Cyrillic,
I bound this function with Alt-Shift-0, that is M-("
  (interactive)
  (when (string= current-input-method nil)
    (user-toggle-input-method)))

(defun user-to-nil ()
  "Change input method to nil (generally to English),
I bound this function with Alt-Sfift-9 that is M-)"
  (interactive)
  (when (string= current-input-method "russian-computer")
    (user-toggle-input-method)))

