;; cucumber.el -- Emacs mode for editing plain text user stories
;;
;; Copyright (C) 2008 Michael Klishin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; add this to your .emacs to load the mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;; ;; and load it
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;;
;; Key Bindings
;; ------------
;;
;;  \C-c ,v
;;  :   Verify all scenarios in the current buffer file.
;;
;;  \C-c ,s 
;;  :   Verify the scenario under the point in the current buffer.
;;
;;  \C-c ,f 
;;  :   Verify all features in project. (Available in feature and 
;;      ruby files)
;;
;;  \C-c ,r 
;;  :   Repeat the last verification process.

(eval-when-compile (require 'cl))

;;
;; Keywords and font locking
;;

(defconst feature-mode-keywords
  '("Feature", "Scenario", "Given", "Then", "When", "And"))

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face)))
 (set (make-local-variable 'font-lock-syntax-table) feature-font-lock-syntax-table))

(defconst feature-blank-line-re "^[[:space:]]*\\(#.*\\)?$")
(defconst feature-0-indent-re "^[[:space:]]*\\(Feature\\|Функционал\\|Сценарий\\|Background\\|Предыстория\\|Scenario\\(?: Outline\\)?\\|Структура\\(?: Сценария\\)?\\):")
(defconst feature-1-indent-re "^[[:space:]]*\\(Given\\|Допустим\\|When\\|Если\\|Then\\|То\\|But\\|\\(?:More \\)?Examples:\\)")
(defconst feature-2-indent-re "^[[:space:]]*\\(And\\|И\\)")
(defconst feature-3-indent-re "^[[:space:]]*\\(|\\|\"\"\"\\)")

(defconst feature-font-lock-keywords
  (list
   '("^[[:space:]]*Feature:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-type-face t)))
   '("^[[:space:]]*Функционал:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-type-face t)))
   '("^[[:space:]]*Сценарий:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-type-face t)))
   '("^[[:space:]]*In order" . font-lock-keyword-face)
   '("^[[:space:]]*Чтобы" . font-lock-keyword-face)
   '("^[[:space:]]*As an" . font-lock-keyword-face)
   '("^[[:space:]]*Как" . font-lock-keyword-face)
   '("^[[:space:]]*I want" . font-lock-keyword-face)
   '("^[[:space:]]*Я хочу" . font-lock-keyword-face)
   '("^[[:space:]]*Background:$" (0 font-lock-keyword-face))
   '("^[[:space:]]*Предыстория:$" (0 font-lock-keyword-face))
   '("^[[:space:]]*Scenario\\(?: Outline\\)?:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-function-name-face t)))
   '("^[[:space:]]*Структура\\(?: Сценария\\)?:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-function-name-face t)))
   '("^[[:space:]]*Given" . font-lock-keyword-face)
   '("^[[:space:]]*Допустим" . font-lock-keyword-face)
   '("^[[:space:]]*When" . font-lock-keyword-face)
   '("^[[:space:]]*Если" . font-lock-keyword-face)
   '("^[[:space:]]*Then" . font-lock-keyword-face)
   '("^[[:space:]]*То" . font-lock-keyword-face)
   '("^[[:space:]]*But" . font-lock-keyword-face)
   '("^[[:space:]]*And" . font-lock-keyword-face)
   '("^[[:space:]]*И" . font-lock-keyword-face)
   '("^[[:space:]]*@.*" . font-lock-preprocessor-face)
   '("^[[:space:]]*\\(?:More \\)?Examples:" . font-lock-keyword-face)
   '("^[[:space:]]*#.*" 0 font-lock-comment-face t)
   '("<[_[:alnum:]]+>" 0 font-lock-variable-name-face t)
   ))


;;
;; Keymap
;;

(defvar feature-mode-map nil "Keymap used in feature mode")

(if feature-mode-map
    nil
  (setq feature-mode-map (make-sparse-keymap))
  (define-key feature-mode-map "\C-m" 'newline)
  (define-key feature-mode-map  (kbd "C-c ,s") 'feature-verify-scenario-at-pos)
  (define-key feature-mode-map  (kbd "C-c ,v") 'feature-verify-all-scenarios-in-buffer)
  (define-key feature-mode-map  (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project))

;; Add relevant feature keybindings to ruby modes
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project)))


;;
;; Syntax table
;;

(defvar feature-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(unless feature-mode-syntax-table
  (setq feature-mode-syntax-table (make-syntax-table)))

;;
;; Variables
;;

(defcustom feature-cucumber-program "cucumber"
  "Program to use to run features"
  :type 'string :group 'feature)

(defvar feature-mode-hook nil
  "Hook run when entering `feature-mode'.")

(defcustom feature-indent-level 2
  "Indentation of feature statements"
  :type 'integer :group 'feature)

(defun feature-mode-variables ()
  (set-syntax-table feature-mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+ *")
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'font-lock-defaults) '((feature-font-lock-keywords) nil nil nil beginning-of-line))
  (set (make-local-variable 'font-lock-keywords) feature-font-lock-keywords))

(defun feature-minor-modes ()
  "Enable all minor modes for feature mode."
  (turn-on-orgtbl)
  (org-defkey orgtbl-mode-map "\C-c\C-c" nil)
  (org-defkey orgtbl-mode-map "\C-c\t" 'orgtbl-ctrl-c-ctrl-c))


;;
;; Mode function
;;

;;;###autoload
(defun feature-mode()
  "Major mode for editing plain text stories"
  (interactive)
  (kill-all-local-variables)
  (use-local-map feature-mode-map)
  (setq mode-name "Feature")
  (setq major-mode 'feature-mode)
  (feature-mode-variables)
  (feature-minor-modes)
  (set (make-local-variable 'indent-line-function) 'feature-indent-line)
  (run-mode-hooks 'feature-mode-hook))

(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))

;;
;; Indentation
;;


(defun feature-keyword-level ()
  "Keyword indent level or nil if not keyword line"
  (cond
   ((looking-at feature-0-indent-re) 0)
   ((looking-at feature-1-indent-re) 1)
   ((looking-at feature-2-indent-re) 2)
   ((looking-at feature-3-indent-re) 3)
   (t nil)))

(defun feature-compute-indentation ()
  "Calculate the indentation of the previous line and its level."
  (save-excursion
    (beginning-of-line)
    (progn
      (cond ((not (bobp))
             (forward-line -1)
             (while (and (or (looking-at feature-blank-line-re) (not (feature-keyword-level)))
                         (> (point) (point-min)))
               (forward-line -1))))
      (list (current-indentation) (feature-keyword-level))
      )))


(defun feature-indent-line ()
  "Indent the current line."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (let ((cl (feature-keyword-level))
          (need (feature-compute-indentation)))
      (delete-horizontal-space)
      (indent-to (+ (car need) 
                    (* feature-indent-level 
                       (if cl
                           (- cl (cadr need))
                         1)))))))

;;
;; Snippets
;;

(defvar feature-snippet-directory (concat (file-name-directory load-file-name) "snippets")
  "Path to the feature-mode snippets.

If the yasnippet library is loaded, snippets in this directory
are loaded on startup.  If nil, don't load snippets.")

(when (and (featurep 'yasnippet)
           feature-snippet-directory
           (file-exists-p feature-snippet-directory))
  (yas/load-directory feature-snippet-directory))


;;
;; Verifying features
;;

(defconst feature-scenario-pattern  "^[[:space:]]*Scenario:[[:space:]]*\\(.*\\)[[:space:]]*$")

(defun feature-scenario-name-at-pos (&optional pos)
  "Returns the name of the scenario at the specified position. if pos is not specified the current buffer location will be used."
  (interactive)
  (let ((start (or pos (point))))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward feature-scenario-pattern nil t)
	(error "Unable to find an scenario"))
      (match-string-no-properties 1))))

(defun feature-verify-scenario-at-pos (&optional pos)
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (feature-run-cucumber 
   (list "-n" (concat "'" (feature-escape-scenario-name (feature-scenario-name-at-pos)) "'"))
   :feature-file (buffer-file-name)))

(defun feature-verify-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (feature-run-cucumber '() :feature-file (buffer-file-name)))


(defun feature-verify-all-scenarios-in-project ()
  "Run all the scenarios defined in current project."
  (interactive)
  (feature-run-cucumber '()))

(defun feature-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (list 'lambda () 
				'(interactive)
				(list 'let (list (list `default-directory
						       default-directory))
				      redoer)))))

    (global-set-key (kbd "C-c ,r") redoer-cmd)))

(defun feature-run-cucumber (cuke-opts &optional &key feature-file)
  "Runs cucumber with the specified options"
  (feature-register-verify-redo (list 'feature-run-cucumber 
				      (list 'quote cuke-opts)
				      :feature-file feature-file))
  ;; redoer is registered

  (let ((opts-str    (mapconcat 'identity cuke-opts " "))
	(feature-arg (if feature-file 
			 (concat " FEATURE='" feature-file "'")
		       "")))
    (compile (concat "rake features CUCUMBER_OPTS=\"--no-color " opts-str "\"" feature-arg)))
  (end-of-buffer-other-window 0))

(defun feature-escape-scenario-name (scenario-name)
  "Escapes all the characaters in a scenario name that mess up using in the -n options"
  (replace-regexp-in-string "\\(\"\\)" "\\\\\\\\\\\\\\1" (replace-regexp-in-string "\\([()\']\\|\\[\\|\\]\\)" "\\\\\\1" scenario-name)))

(defun feature-root-directory-p (a-directory)
  "Tests if a-directory is the root of the directory tree (i.e. is it '/' on unix)."
  (equal a-directory (rspec-parent-directory a-directory)))

(defun feature-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (if (feature-root-directory-p directory) (error "No rakefle found"))
    (if (file-exists-p (concat directory "Rakefile")) 
	directory
      (feature-project-root (file-name-directory (directory-file-name directory))))))



(provide 'cucumber-mode)
(provide 'feature-mode)
