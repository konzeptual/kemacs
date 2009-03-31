;;; kemacs-misc.el --- Configuration that does not fit anywhere
;;
;; Part of kEmacs

;;Change backup behavior to save in a directory, not in a miscellany
;;of files all over the place.
(setq my-backup-dir (concat dotfiles-dir "saves"))

(setq
 ;; don't clobber symlinks
 backup-by-copying t
 ;; don't litter my fs tree
 backup-directory-alist `(("." . ,(expand-file-name
					 (concat dotfiles-dir "backups"))))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; use versioned backups
 version-control t)


(provide 'kemacs-misc)
;;; kemacs-misc.el ends here
