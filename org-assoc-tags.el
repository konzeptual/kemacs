;;; org-assoc-tags.el --- 
;; 
;; Author: Konstantin Antipin
;; Created: Tue Mar 31 18:02:07 2009 (+0200)
;; Version: 0.1.1
;; Last-Updated: Tue Mar 31 23:05:14 2009 (+0200)
;;           By: Konstantin Antipin
;;     Update #: 1
;; URL: http://www.emacswiki.org/emacs/org-assoc-tags.el
;; Keywords: org-mode tags
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This is actually a little tweak for org-mode.
;; 
;; You can define a key-tag and list of associated tags.
;; Every time you set a key-tag to the item, associated tags
;; assign automatically.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Installation: 
;; 
;; 1. Put org-assoc-tags.el to some directory.
;;    (for example ~/.emacs.d/vendor).
;; 2. In your .emacs add this directry to the load-path like this:
;;    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))
;; 3. Add the following code to your .emacs file:
;;    (require 'org-assoc-tags)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Usage: 
;; 
;; Customize org-assoc-tags variable via customization interface or like this:
;; (setq org-assoc-tags '(
;; 		       ("emacs" "tech")
;; 		       ("orgmode" "emacs" "tech")
;; 		       ))
;; format: (
;;          (key-tag1 first-associated-tag second-associated-tag ...)
;;          (key-tag2 first-associated-tag second-associated-tag ...)
;;         ...
;;          ).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 31.03.2009
;;      * Bug fix: Add hook to the remember-mode
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.;;; org-assoc-tags.el --- 

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defcustom org-assoc-tags nil
"List of associated tags, that is used by org-assign-assoc-tags function.
Every associated tag will be automatically assigned once key-tag is assigned.
format: (
         (key-tag1 first-associated-tag second-associated-tag ...)
         (key-tag2 first-associated-tag second-associated-tag ...)
        ...
         ).
"
:group 'org-tags
:tag "Org associated tags"
:type 'alist
)

(defun org-assign-assoc-tags ()
  (interactive)
  "Find associated tags and assign them.
   list of associated tags can be customized with org-assoc-tags.
"
  (dolist (tag (org-get-tags))
    (let ((assoc-tags (cdr (assoc tag org-assoc-tags))))
    (if (not (eq assoc-tags nil))
	(dolist (assoc-tag assoc-tags)
	  (progn
	    ;; turn off all hooks for a second, while we
	    ;; call org-toggle-tag
	    (let (tmp-org-after-tags-change-hook org-after-tags-change-hook)
	      (progn
		(setq org-after-tags-change-hook nil)
		(org-toggle-tag assoc-tag 'on)
		(setq org-after-tags-change-hook tmp-org-after-tags-change-hook)
;;; 		(message "tag %s, assoc tag %s" tag assoc-tag)
		))
	    )
      )))))

(add-hook 'org-after-tags-change-hook 'org-assign-assoc-tags)
(add-hook 'org-remember-before-finalize-hook 'org-assign-assoc-tags)

(provide 'org-assoc-tags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-assoc-tags.el ends here
