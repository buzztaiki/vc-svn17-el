;;; vc-svn17.el --- Subversion 1.7 support for vc-svn

;; Copyright (C) 2012  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Maintainer: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides following features:
;;
;; - SVN 1.7 functions for Emacs23.
;; - Multiple SVN support.
;;
;; To use this package, put followings in your .emacs:
;;
;;     (require 'vc-svn17)
;;
;; You can configure SVN 1.7 and 1.6 programs if you have both
;; versions. Do it as followings:
;;
;;   ;; for svn 1.6
;;   (setq vc-svn-program "/usr/bin/svn")
;;   ;; for svn 1.7
;;   (setq vc-svn17-program "/usr/local/bin/svn")
;;
;; After this configuration, vc-svn will use suitable program for svn
;; version of a working copy.


;;; Code:

(require 'vc-svn)

(defmacro vc-svn17-e23-defun (name args &rest body)
  (declare (indent defun))
  `(when (<= emacs-major-version 23)
     ,(if (stringp (car body))
	  `(defun ,name ,args ,(car body) ,@(cdr body))
	`(defun ,name ,args nil ,@body))))

(defmacro vc-svn17-e23-defadvice (name args &rest body)
  (declare (indent defun))
  `(when (<= emacs-major-version 23)
     (defadvice ,name (,(car args) vc-svn17-e23 activate) ,@body)))

(vc-svn17-e23-defun vc-svn-root (file)
  (vc-find-root file vc-svn-admin-directory))

  
(vc-svn17-e23-defadvice vc-svn-registered (around)
  (let ((root (vc-svn-root (ad-get-arg 0))))
    (when root
      (ad-set-arg 0 root)
      ad-do-it)))

(vc-svn17-e23-defadvice vc-svn-responsible-p (around)
  (setq ad-return-value (vc-svn-root (ad-get-arg 0))))

(vc-svn17-e23-defadvice vc-svn-repository-hostname (around)
  (setq
   ad-return-value
   (with-temp-buffer
     (let (process-file-side-effects)
       (vc-svn-command t t dirname "info"))
     (goto-char (point-min))
     (when (re-search-forward "^URL: *\\(.*\\)" nil t)
       (match-string 1)))))

(defcustom vc-svn17-program nil
  "Name of SVN 1.7 executable.
vc-svn uses this value for subversion 1.7 if non-nil, otherwise vc-svn
  uses `vc-svn-program' always."
  :type 'string
  :risky t
  :group 'vc)

(defun vc-svn17-program ()
  "Returns SVN 1.7 executable."
  (or vc-svn17-program vc-svn-program))

(defun vc-svn17-wc-p (file)
  "Returns non-nil if the working copy version of `file' is 1.7."
  (file-exists-p
   (expand-file-name
    "wc.db"
    (expand-file-name vc-svn-admin-directory
		      (vc-svn-root file)))))

(defadvice vc-svn-command (around svn-17 activate)
  "SVN auto-detection is allowed if active.
`vc-svn17-program' is used if `vc-svn17-wc-p' is non-nil, otherwise
`vc-svn-program' is used."
  (let ((file-or-list (ad-get-arg 2)))
    (cond ((null file-or-list) ad-do-it)
	  ((vc-svn17-wc-p
	    (if (consp file-or-list) (car file-or-list) file-or-list))
	   (let ((vc-svn-program (vc-svn17-program)))
	     ad-do-it))
	  (t ad-do-it))))

(provide 'vc-svn17)
;;; vc-svn17.el ends here
