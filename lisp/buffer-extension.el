;;; buffer-extension.el --- Some enhanced functions for buffer manipulate.

;; Filename: buffer-extension.el
;; Description: Some enhanced functions for buffer manipulate.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 21:57:21
;; Version: 0.1
;; Last-Updated: 2009-02-07 21:57:21
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/buffer-extension.el
;; Keywords: buffer
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some enhanced functions for buffer manipulate.
;;

;;; Installation:
;;
;; Put buffer-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'buffer-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun kill-current-mode-buffers ()
  "Kill all buffers that major mode same with current mode."
  (interactive)
  (kill-special-mode-buffers major-mode))

(defun kill-current-mode-buffers-except-current ()
  "Kill all buffers that major mode same with current mode.
And don't kill current buffer."
  (interactive)
  (kill-special-mode-buffers major-mode t))

(defun kill-special-mode-buffers (mode &optional except-current-buffer)
  "Kill all buffers that major MODE same with special.
If option EXCEPT-CURRENT-BUFFER is `non-nil',
kill all buffers with MODE except current buffer."
  (interactive)
  (let ((current-buf (current-buffer))
        (count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (equal major-mode mode)
                 (or (not except-current-buffer)
                     (not (eq current-buf buffer))))
        (incf count)
        (kill-buffer buffer)))
    (message "Killed %s buffer%s" count (if (> count 1) "s" ""))))

(defun kill-all-buffers-except-current ()
  "Kill all buffers except current buffer."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))

(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

(defun unmark-all-buffers ()
  "Unmark all have marked buffers."
  (interactive)
  (let ((current-element (current-buffer)))
    (save-excursion
      (dolist (element (buffer-list))
        (set-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

(provide 'buffer-extension)

;;; buffer-extension.el ends here
