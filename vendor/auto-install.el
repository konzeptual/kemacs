;;; auto-install.el --- Auto install elisp file

;; Filename: auto-install.el
;; Description: Auto install elisp file
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-11 13:56:50
;; Version: 0.7.1
;; Last-Updated: 2009-03-11 22:43:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/auto-install.el
;; Keywords: auto-install
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `url' `dired' `find-func' `bytecomp' `cl'
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
;; Automates the installation of Emacs Lisp files and packages.
;;
;; `auto-install' provides an automated way to:
;;
;; (1) Download Emacs Lisp files and packages from common sources
;; (2) View them (diff) and save them to your repository
;; (3) Compile and Load them
;;
;; It provides the following commands:
;;
;;      `auto-install-from-url'
;;              Install an elisp file from a given url.
;;      `auto-install-from-emacswiki'
;;              Install an elisp file from EmacsWiki.org.
;;      `auto-install-from-gist'
;;              Install an elisp file from gist.github.com.
;;      `auto-install-from-library'
;;              Update an elisp library.
;;      `auto-install-from-directory'
;;              Update elisp files under directory.
;;      `auto-install-from-dired'
;;              Update dired marked elisp files from EmacsWiki.org
;;      `auto-install-from-buffer'
;;              Install the elisp file in the current buffer.
;;      `auto-install-update-emacswiki-package-name'
;;              Update the list of elisp package names from `EmacsWiki'.
;;      `auto-install-dired-mark-files'
;;              Mark dired files that contain at `EmacsWiki.org'.
;;
;; Tips:
;;
;;      Downloading is asynchronous: you can do your work and download
;;      files at the same time.  The download process won't hang
;;      Emacs.
;;
;;      `auto-install-from-url' remembers previous installations.  So if
;;      your search is the same as the previous search, you don't need
;;      to type it in, just hit RETURN.
;;
;;      `auto-install-from-emacswiki' will complete then names of
;;      packages from those in the Elisp area in `EmacsWiki'.
;;
;;      `auto-install-from-library' will prompt you library name in
;;      you load-path, then it try to download from EmacsWiki if it
;;      can't find match in `auto-install-filter-url'.
;;
;;      `auto-install-from-directory' can install elisp file
;;      under specify directory.
;;
;;      `auto-install-from-dired' can install marked files using dired.
;;      You can mark the files you want in dired and then use
;;      `auto-install-from-dired' to download those files
;;      asynchronously.
;;
;;      `auto-install-from-buffer' can save and install the contents of
;;      the current buffer as a file.  You need a valid elisp file name.
;;      The default name is the buffer name.
;;
;;      `auto-install-from-emacswiki' and `auto-install-from-library'
;;      will try to pick up file around point, you can move
;;      cursor to file name, and just hit RET for install.
;;
;;      All of the above functions support a filename filter.  You can
;;      input any url to download an elisp file, if the file name suffix is
;;      `.el', it will download and install the file automatically.
;;      Otherwise, it won't install it unless you input a valid elisp
;;      file name.
;;
;;      By default, if a file that you download does not exist on your
;;      system the file is downloaded to `auto-install-directory'.  If
;;      you already have a file with the same name in your load
;;      directory, `auto-install' will try to replace that file.
;;
;;      You can use command `auto-install-dired-mark-files' to mark files
;;      that contain at `EmacsWiki.org' for fast update.
;;
;;      By default, command `auto-install-from-emacswiki' will initialization
;;      current symbol as default value, if default value is you want,
;;      just hit RET, so lazy!
;;

;;; Installation:
;;
;; (1) Put auto-install.el somewhere in your load-path.
;;
;;     For example, put it into  ~/elisp/.
;;     Then add the following to your ~/.emacs:
;;
;;       (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; (2) And put the following in your ~/.emacs startup file:
;;
;;       (require 'auto-install)
;;
;; (3) Add this to your ~/.emacs to optionally specify a download directory:
;;
;;       (setq auto-install-directory "~/.emacs.d/auto-install/")
;;
;;     If you don't set this, "~/.emacs.d/auto-install/" will be used as the default,
;;     and will be created as needed.
;;
;; (4) Optionally, if your computer is always connected Internet when Emacs start up,
;;     I recommend you add below to your ~/.emacs, to update package name when start up:
;;
;;       (auto-install-update-emacswiki-package-name t)
;;
;;     And above setup is not necessary, because AutoInstall will automatically update
;;     package name when you just first call `auto-install-from-emacswiki',
;;     above setup just avoid *delay* when you first call `auto-install-from-emacswiki'.
;;
;; (5) I recommend you add below to your ~/.emacs for install-elisp users:
;;
;;       (auto-install-compatibility-setup)
;;
;;     This command `defalias'es `install-elisp',
;;     `install-elisp-from-emacswiki' and `install-elisp-from-gist' to
;;     `auto-install' ones.
;;

;;; Customize:
;;
;; `auto-install-directory'
;; The default directory for keeping auto-downloaded elisp files.
;;
;; `auto-install-buffer-name'
;; The base buffer name for temporarily storing downloaded download content.
;;
;; `auto-install-emacswiki-base-url'
;; The base url for downloading from EmacsWiki.org.
;;
;; `auto-install-gist-base-url'
;; The base url for downloading from gist.github.com
;;
;; `auto-install-filter-url'
;; Filter url for downloading a special library.
;;
;; `auto-install-save-confirm'
;; Whether to require confirmation when saving downloaded content.
;;
;; `auto-install-replace-confirm'
;; Whether to require confirmation when replacing an already-installed
;; file.
;;
;; `auto-install-install-confirm'
;; Whether to require confirmation when installing a file.
;;
;; `auto-install-from-dired-confirm'
;; Whether to require confirmation when downloading files marked in dired.
;;
;; And all above option can customize easy through:
;;      M-x RET customize-group RET auto-install RET
;;

;;; Change log:
;;
;; 2009/03/11
;;  * Andy Stewart:
;;      * Fix bug of `auto-install-download'.
;;
;; 2009/03/03
;;  * rubikitch
;;      * Add new command `auto-install-compatibility-setup'
;;        for install-elisp users.
;;  * Andy Stewart:
;;      * `auto-install-region-or-thing' return region string
;;        just when `transient-mark-mode' is on.
;;      * Fix doc.
;;
;; 2009/02/17
;;  * Andy Stewart:
;;      * Modified keybindings, make it more easy to remember.
;;      * Make `auto-install-save-confirm' default with `t'
;;        for security problem.
;;      * Pick up current symbol when use `auto-install-from-library'.
;;      * Remove unnecessary completion name from `auto-install-from-library'.
;;      * Refactory code.
;;      * Fix doc.
;;
;; 2009/02/12
;;  * Andy Stewart:
;;      * Remove option `auto-install-update-emacswiki-package-name-when-startup'.
;;      * Make current symbol as initialization of `auto-install-from-emacswiki'.
;;      * Add option `unforced' to function `auto-install-update-emacswiki-package-name'.
;;      * Fix doc.
;;      * Fix bug of `auto-install-from-library'.
;;
;; 2009/02/10
;;  * Andy Stewart:
;;      * Automatically download package name list when
;;        variable `auto-install-package-name-list' is nil.
;;      * Reverse `auto-install-package-name-list' for `anything' interface.
;;      * New command `auto-install-dired-mark-files',
;;        mark files that contain at `EmacsWiki.org'.
;;      * New command `auto-install-buffer-diff',
;;        view different between current version and old version.
;;
;; 2009/02/06
;;  * Andy Stewart:
;;      * Add new command `auto-install-from-directory'.
;;      * Remove option `auto-install-create-directory', not necessary.
;;      * Documentation improvements (thanks Scot Becker)
;;
;; 2009/02/01
;;  * Andy Stewart:
;;      * Make command `auto-install-from-emacswiki' can
;;        completing package name for input.
;;      * Add new command `auto-install-update-emacswiki-package-name'.
;;      * Add new option `auto-install-update-emacswiki-package-name-when-startup'
;;
;; 2009/01/30
;;  * Andy Stewart:
;;      * Compatibility with GNU Emacs 22.
;;
;; 2009/01/26
;;  * Andy Stewart:
;;      * Add new command `auto-install-from-gist'.
;;
;; 2009/01/21
;;  * Andy Stewart:
;;      * Add emacs-lisp syntax highlight for download buffer.
;;      * Make notify message display at mode-line instead echo-area.
;;
;; 2009/01/10
;;  * Andy Stewart:
;;      * Add new option `auto-install-filter-url' and new function
;;        `auto-install-from-library', try to use it. ;)
;;
;; 2009/01/08
;;  * Andy Stewart:
;;      * Fix coding bug.
;;
;; 2009/01/07
;;  * Andy Stewart:
;;      * Move `w3m' code to file `auto-install-extension.el' to make all
;;        user can use this package with standard emacs.
;;
;; 2009/01/06
;;  * Andy Stewart:
;;      * Clean code.
;;
;; 2009/01/02
;;  * Andy Stewart:
;;      * Add new option `auto-install-create-directory' for create install directory
;;        automatically if it doesn't exist.
;;      * Improve many document make it more clear.
;;      * Thanks document improve and create directory advice of 'Drew Adams'!
;;
;; 2008/12/24
;;  * Andy Stewart:
;;      * Remove `auto-install-window-configuration-before-download', `auto-install-init-window-layout'
;;        and `auto-install-revert-window-layout'.
;;        It's not necessary to revert window layout, `winner-mode' can revert window layout more better,
;;        just type `winner-undo'.
;;
;; 2008/12/15
;;  * Andy Stewart:
;;      * Fix a little bug of `auto-install-window-configuration-before-download'.
;;
;; 2008/12/11
;;  * Andy Stewart:
;;      * Add new function `auto-install-from-buffer', to install elisp file from current buffer.
;;        Modified `auto-install-buffer-save' to use `auto-install-from-buffer'.
;;
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch       <rubikitch@ruby-lang.org>
;;              For install-elisp.el
;;      Drew Adams      <drew.adams@oracle.com>
;;      Scot Becker     <scot.becker@gmail.com>
;;      Richard Riley   <rileyrgdev@gmail.com>
;;              For documentation improvements and advices.
;;

;;; TODO
;;
;;      Fix the problem parallel install process with recursive prompt.
;;      Redesign and give more friendly user interface.
;;      Scan RSS track package update and notify.
;;

;;; Require
(require 'url)
(require 'dired)
(require 'find-func)
(require 'bytecomp)
(eval-when-compile (require 'cl))
(when (<= emacs-major-version 22)       ;Compatibility with 22.
  (autoload 'ignore-errors "cl-macs"))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup auto-install nil
  "Auto install elisp files."
  :group 'external)

(defcustom auto-install-directory "~/.emacs.d/auto-install/"
  "The directory for saving elisp files.
This directory is used when a downloaded
elisp file does not already exist in other directory.
Otherwise, the existing file of the same name is replaced."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-buffer-name "auto-install"
  "The temporary buffer for storing download content."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-emacswiki-base-url "http://www.emacswiki.org/cgi-bin/wiki/download/"
  "The base emacswiki.org url from which to download elisp files."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-gist-base-url "http://gist.github.com/"
  "The base gist.github.com url from which to download elisp files."
  :type 'string
  :group 'auto-install)

(defcustom auto-install-filter-url
  '(("color-grep" "http://www.bookshelf.jp/elc/")
    ("auto-complete" "http://www.cx4a.org/pub/"))
  "Alist mapping filter url for library.
Default command `auto-install-from-library' will install from EmacsWiki,
if it can't find match in this alist."
  :type '(repeat (list (string :tag "Library")
                       (string :tag "Download URL")))
  :group 'auto-install)

(defcustom auto-install-save-confirm t
  "Whether confirmation is needed to save downloaded content.
Nil means no confirmation is needed.
If non-nil, the downloaded content is shown in a buffer and you are
prompted to confirm saving it to a file."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-replace-confirm nil
  "Whether confirmation is needed to replace an existing elisp file.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-install-confirm nil
  "Whether confirmation is needed to install a downloaded elisp file.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defcustom auto-install-from-dired-confirm t
  "Whether confirmation is needed to download marked files from Dired.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar auto-install-download-buffer nil
  "The download buffer used by `url-retrieve'.
This variable is always buffer-local.")
(make-variable-buffer-local 'auto-install-download-buffer)

(defvar auto-install-download-url nil
  "The url from which to download files.
This variable is always buffer-local.")
(make-variable-buffer-local 'auto-install-download-url)

(defvar auto-install-last-url nil
  "The last url used in `auto-install-from-url'.")

(defvar auto-install-last-gist-id nil
  "The last gist id you visit in `auto-install-from-gist'.")

(defvar auto-install-package-name-list nil
  "The package name list for completion input.")

(defvar auto-install-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'auto-install-buffer-diff) ;diff
    (define-key map (kbd "C-c C-c") 'auto-install-buffer-save) ;save
    (define-key map (kbd "C-c C-q") 'auto-install-buffer-quit) ;quit
    map)
  "Keymap used by variable `auto-install-minor-mode'.")

(define-minor-mode auto-install-minor-mode
  "Auto Install minor mode."
  :init-value nil
  :lighter " Auto-Install"
  :keymap auto-install-minor-mode-map
  :group 'auto-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-install-from-buffer ()
  "Install elisp file from current buffer."
  (interactive)
  (let (filename)
    (setq filename (read-string (format "Filename (%s): " (buffer-name)) nil nil (buffer-name)))
    (auto-install-mode)
    (auto-install-buffer-save filename)))

(defun auto-install-from-url (&optional url)
  "Download elisp file from URL."
  (interactive)
  (or url (setq url (read-string (format "URL (%s): " (or auto-install-last-url "")) nil nil auto-install-last-url)))
  (setq auto-install-last-url url)
  (auto-install-download url))

(defun auto-install-from-emacswiki (&optional file)
  "Download elisp file FILE from emacswiki."
  (interactive)
  (if auto-install-package-name-list
      ;; Install package if `auto-install-package-name-list' is non-nil.
      (progn
        (or file (setq file (auto-install-get-candidate "Package" auto-install-package-name-list)))
        (auto-install-download (concat auto-install-emacswiki-base-url file)))
    ;; Otherwise update package name and install.
    (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                           'auto-install-handle-emacswiki-package-install)))

(defun auto-install-from-gist (&optional gistid)
  "Download and install elisp files from gist.
Optional argument GISTID is gist ID for download elisp file from gist.github.com."
  (interactive)
  (or gistid (setq gistid (read-string (format "Gist ID (%s): " (or auto-install-last-gist-id ""))
                                       nil nil
                                       auto-install-last-gist-id)))
  (setq auto-install-last-gist-id gistid)
  (auto-install-download (format "%s%s.txt" auto-install-gist-base-url gistid)))

(defun auto-install-from-library (&optional library)
  "Download elisp file with LIBRARY.
Default this function will found 'download url' from `auto-install-filter-url',
if not found, try to download from EmacsWiki."
  (interactive
   (let* ((dirs load-path)
          (suffixes (find-library-suffixes)))
     (list (auto-install-get-candidate "Library name" (auto-install-get-library-list)))))
  (let ((filename (file-name-nondirectory (find-library-name library)))
        (base-url auto-install-emacswiki-base-url)
        (library-name (replace-regexp-in-string "\\(\\.el.*$\\)" "" library)))
    (if (assoc library-name auto-install-filter-url)
        (setq base-url (cadr (assoc library-name auto-install-filter-url))))
    (auto-install-download (concat base-url filename))))

(defun auto-install-from-directory (directory)
  "Update elisp file from EmacsWiki.
You can use this command to update elisp file under DIRECTORY."
  (interactive "DDirectory: ")
  (let (filename)
    (dolist (file (directory-files directory t))
      (if (file-directory-p file)
          ;; Don't match . or .. directory.
          (unless (string-match "^\\.\\.?$" (file-name-nondirectory file))
            ;; Find files in sub-directory.
            (auto-install-from-directory file))
        ;; Get file name.
        (setq filename (file-name-nondirectory file))
        ;; Not backup file.
        (unless (string-match "^\\.?#" filename)
          ;; Match elisp file.
          (if (string-match "^.*\\.el" filename)
              (auto-install-download (concat auto-install-emacswiki-base-url filename))))))))

(defun auto-install-from-dired ()
  "Update elisp files from emacswiki.
You can use this to download marked files in Dired asynchronously."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (if (or (not auto-install-from-dired-confirm)
              (yes-or-no-p "Do you want install marked files from EmacsWiki.org?"))
          (dolist (file (dired-get-marked-files))
            (auto-install-download (concat auto-install-emacswiki-base-url (file-name-nondirectory file)))))
    (message "This command is only for `dired-mode'.")))

(defun auto-install-update-emacswiki-package-name (&optional unforced)
  "Update elisp package name from `EmacsWiki'.
By default, this function will update package name forcibly.
If UNFORCED is non-nil, just update package name when `auto-install-package-name-list' is nil."
  (interactive)
  (unless (and unforced
               auto-install-package-name-list)
    (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                           'auto-install-handle-emacswiki-package-name)))

(defun auto-install-dired-mark-files ()
  "This function will mark files that match emacswiki page."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (if auto-install-package-name-list
          ;; Mark files that exist at `EmacsWiki'.
          (auto-install-dired-mark-files-internal)
        ;; Or get package name list and match files.
        (auto-install-download "http://www.emacswiki.org/cgi-bin/emacs?action=index;raw=1"
                               'auto-install-handle-dired-mark-files))
    (message "This command just use in `dired-mode'.")))

(defun auto-install-mode ()
  "Major mode for auto-installing elisp code."
  (interactive)
  ;; Load emacs-lisp syntax highlight.
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (lisp-mode-variables)
  (setq font-lock-mode t)
  (font-lock-fontify-buffer)
  ;; Read only.
  (setq buffer-read-only t)
  ;; Load `auto-install' mode.
  (auto-install-minor-mode t)
  (setq major-mode 'auto-install-minor-mode))

(defun auto-install-buffer-quit ()
  "Quit from `auto-install' temporary buffer."
  (interactive)
  ;; Quit buffer.
  (if (eq major-mode 'auto-install-minor-mode)
      (auto-install-quit)
    (message "This command just use in `auto-install-minor-mode'.")))

(defun auto-install-compatibility-setup ()
  "Install compatibility commands for install-elisp users."
  (interactive)
  (defalias 'install-elisp 'auto-install-from-url)
  (if (require 'anything-auto-install nil t)
      (defalias 'install-elisp-from-emacswiki 'anything-auto-install-from-emacswiki)
    (defalias 'install-elisp-from-emacswiki 'auto-install-from-emacswiki))
  (defalias 'install-elisp-from-gist 'auto-install-from-gist)
  (message "Install-elisp compatibility installed.
install-elisp                = %s
install-elisp-from-emacswiki = %s
install-elisp-from-gist      = %s"
           (symbol-function 'install-elisp)
           (symbol-function 'install-elisp-from-emacswiki)
           (symbol-function 'install-elisp-from-gist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun auto-install-download (url &optional handle-function)
  "Download elisp file from URL.
HANDLE-FUNCTION for handle download content,
default is `auto-install-handle-download-content'."
  ;; Check and create install directory.
  (unless (file-exists-p auto-install-directory)
    (make-directory auto-install-directory)
    (message "Create directory %s for install elisp file." auto-install-directory))
  ;; Download.
  (let* ((url-request-method "GET")
         (url-request-extra-headers nil)
         (url-mime-accept-string "*/*")
         (parsed-url (url-generic-parse-url url))
         (download-buffer (auto-install-get-buffer))
         (download-buffer-name (buffer-name download-buffer)))
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Bind download url with local buffer.
      (setq auto-install-download-url url)
      ;; Bind download buffer with local buffer.
      ;;
      ;; Use buffer-local variable receive
      ;; data from `url-retrieve' to make asynchronously
      ;; download file with special buffer.
      ;;
      ;; Because the buffer name is unique that generate
      ;; through `current-time', so can download many elisp file
      ;; asynchronously and won't conflict each other.
      (setq auto-install-download-buffer
            (url-retrieve parsed-url
                          'auto-install-download-callback
                          (list download-buffer-name handle-function))))))

(defun auto-install-download-callback (&optional redirect download-buffer-name handle-function)
  "The callback for `auto-install-download'.
With `auto-install-download', this downloads elisp files asynchronously.
REDIRECT is the argument for check download status.
DOWNLOAD-BUFFER-NAME is the name of download buffer.
HANDLE-FUNCTION is function for handle download content."
  (if (eq (car redirect) ':error)
      ;; Notify user and kill buffer when occur error.
      (with-current-buffer (get-buffer download-buffer-name)
        (message "Download from '%s' failed." auto-install-download-url)
        (kill-buffer download-buffer-name))
    ;; Otherwise continue install process.
    (auto-install-retrieve-decode download-buffer-name 'utf-8) ;decode retrieve information.
    (with-current-buffer (get-buffer download-buffer-name)
      ;; Show successful message
      (message "Download from '%s' successful." auto-install-download-url)
      ;; Handle download content.
      (funcall (or handle-function 'auto-install-handle-download-content)
               (current-buffer)))))

(defun auto-install-retrieve-decode (retrieve-buffer-name coding)
  "Decode the RETRIEVE-BUFFER-NAME with CODING."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer retrieve-buffer-name)
    (insert
     (with-current-buffer auto-install-download-buffer
       (set-buffer-multibyte t)
       (goto-char (1+ url-http-end-of-headers))
       (decode-coding-region
        (point) (point-max)
        (coding-system-change-eol-conversion coding 'dos))
       (buffer-substring (point) (point-max))))
    (goto-char (point-min))))

(defun auto-install-handle-download-content (download-buffer)
  "Handle the content downloaded to buffer DOWNLOAD-BUFFER."
  (with-current-buffer download-buffer
    ;; Load mode.
    (auto-install-mode)
    ;; Display help information in mode-line.
    (setq mode-line-format (list "Type C-c C-c to continue; Type C-c C-d for view diff; Type C-c C-q to quit."))
    ;; Save or switch.
    (if auto-install-save-confirm
        ;; Switch to buffer
        (switch-to-buffer download-buffer)
      ;; Save buffer.
      (auto-install-buffer-save))))

(defun auto-install-handle-emacswiki-package-name (download-buffer &optional prompt-install)
  "Handle elisp package name from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer.
PROMPT-INSTALL is non-nil, will prompt package name for install."
  ;; Update package name list.
  (auto-install-update-emacswiki-package-list download-buffer)
  ;; Prompt package name for install.
  (when prompt-install
    (auto-install-download
     (concat auto-install-emacswiki-base-url
             (auto-install-get-candidate "Package" auto-install-package-name-list)))))

(defun auto-install-handle-dired-mark-files (download-buffer)
  "Handle dired mark files that exist at `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  ;; Update package name list.
  (auto-install-update-emacswiki-package-list download-buffer)
  ;; Mark dired files.
  (auto-install-dired-mark-files-internal))

(defun auto-install-handle-emacswiki-package-install (download-buffer)
  "Handle elisp package install from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  (auto-install-handle-emacswiki-package-name download-buffer t))

(defun auto-install-update-emacswiki-package-list (download-buffer)
  "Filter and update package name list from `EmacsWiki'.
DOWNLOAD-BUFFER is the name of download buffer."
  ;; Clean `auto-install-package-name-list'.
  (setq auto-install-package-name-list nil)
  ;; Get package name.
  (goto-char (point-min))
  (while (re-search-forward "^.*\\.el$" nil t)
    (add-to-list 'auto-install-package-name-list (match-string 0)))
  ;; Reverse package list for `anything-auto-install'.
  (setq auto-install-package-name-list (nreverse auto-install-package-name-list))
  ;; Kill buffer.
  (kill-buffer download-buffer)
  ;; Display successful message.
  (message "Update package name from `EmacsWiki' successful."))

(defun auto-install-buffer-diff ()
  "View different between old version.
This command just run when have exist old version."
  (interactive)
  (let* ((new-file (url-file-nondirectory auto-install-download-url))
         (old-file (auto-install-get-path new-file)))
    (if old-file
        ;; View different when have old version exist.
        (ediff-buffers (current-buffer) (find-file-noselect old-file))
      ;; Otherwise notify user.
      (message "Haven't old version exist."))))

(defun auto-install-buffer-save (&optional filename)
  "Save downloaded content to file FILENAME."
  (interactive)
  (if (eq major-mode 'auto-install-minor-mode)
      (let (file-path)
        ;; Get filename
        (unless filename
          (setq filename (url-file-nondirectory auto-install-download-url)))
        ;; Make sure file suffix with `.el'.
        (while (not (string-match ".*\.el$" filename))
          (setq filename (read-string "Please input file name suffix with `.el': ")))
        ;; Get file path.
        (setq file-path
              (or
               ;; Replace file if have exist.
               (auto-install-get-path filename)
               ;; Otherwise, install in directory `auto-install-directory'.
               (concat auto-install-directory filename)))
        ;; Save file.
        (if (or (not (file-exists-p file-path))
                (not auto-install-replace-confirm)
                (yes-or-no-p (format "Do you want replace file: '%s' ?" file-path)))
            (progn
              (write-file file-path)
              ;; Install file.
              (auto-install-install file-path))
          ;; Quit
          (auto-install-quit)))
    (message "This command just use in `auto-install-minor-mode'.")))

(defun auto-install-install (file-path)
  "Install elisp file FILE-PATH."
  (if (or (not auto-install-install-confirm)
          (yes-or-no-p (format "Do you want install file: '%s' ?" file-path)))
      (let (byte-compile-warnings) ;; suppress compile warnings
        ;; Compile and load file.
        (unless (ignore-errors (byte-compile-file file-path t))
          ;; Show `ERROR' message if compile failed.
          (message (format "Auto-Install ERROR: Compiled file '%s' failed." file-path))))
    ;; Quit
    (auto-install-quit)))

(defun auto-install-quit ()
  "Quit auto-install."
  ;; Kill buffer
  (kill-buffer (current-buffer))
  ;; Show quit message.
  (message "Quit auto-install process."))

(defun auto-install-get-path (library)
  "Return the absolute file path of the Lisp source of LIBRARY."
  ;; If the library is byte-compiled, try to find a source library by
  ;; the same name.
  (if (string-match "\\.el\\(c\\(\\..*\\)?\\)\\'" library)
      (setq library (replace-match "" t t library)))
  (or
   (locate-file library
                (or find-function-source-path load-path)
                (find-library-suffixes))
   (locate-file library
                (or find-function-source-path load-path)
                load-file-rep-suffixes)))

(defun auto-install-get-buffer ()
  "Get a buffer for temporary storage of downloaded content.
Uses `current-time' to make buffer name unique."
  (let (time-now buffer)
    (setq time-now (current-time))
    (get-buffer-create (format "*%s<%s-%s-%s>*"
                               auto-install-buffer-name
                               (nth 0 time-now) (nth 1 time-now) (nth 2 time-now)))))

(defun auto-install-dired-mark-files-internal ()
  "Mark files that match `auto-install-package-name-list'."
  ;; Set buffer visible in select window.
  (set-buffer (window-buffer))
  ;; Get mark files.
  (save-excursion
    (let (filename)
      ;; Unmark all markes.
      (dired-unmark-all-marks)
      ;; Try to mark files.
      (goto-char (point-min))
      (while (not (eobp))
        (setq filename (dired-get-filename nil t))
        (if (and filename
                 (not (file-directory-p filename))
                 (member (file-name-nondirectory filename) auto-install-package-name-list))
            (dired-mark 1))
        (dired-next-line 1)))))

(defun auto-install-region-or-thing (&optional thing)
  "Return region or thing around point.
If `mark-active' and `transient-mark-mode', return region.
If THING is non-nil, return THING around point;
otherwise return symbol around point."
  ;; Return string.
  (if (and mark-active
           transient-mark-mode)
      ;; Return region string just when
      ;; `mark-active' and `transient-mark-mode' is on.
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    ;; Otherwise try to pick-up THING around point.
    (setq thing (or thing 'symbol))
    (ignore-errors
      (save-excursion
        (buffer-substring-no-properties (beginning-of-thing thing)
                                        (end-of-thing thing))))))

(defun auto-install-get-library-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)) name)
              (add-to-list 'names name)))))
    names))

(defun auto-install-get-candidate (prompt collection)
  "Get candidate from completing list.
PROMPT is string for prompt.
COLLECTION is list for completing candidates."
  (completing-read (format "%s (%s): " prompt (or (auto-install-region-or-thing) ""))
                   collection
                   nil nil nil nil
                   (auto-install-region-or-thing)))

(provide 'auto-install)

;;; auto-install.el ends here

;;; LocalWords:  el eol dirs fontify gistid txt func bytecomp DDirectory ediff
;;; LocalWords:  noselect Unmark unmark AutoInstall keybindings defalias'es
