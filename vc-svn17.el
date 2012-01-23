;;; vc-svn17.el --- Subversion 1.7 support for vc-svn on Emacs23

;; Copyright (C) 2012  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Maintainer: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Version: 0.01
;; URL: https://github.com/buzztaiki/vc-svn17-el

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

;; This package provides subversion 1.7 support for vc-svn on Emacs23.

;; To use this package, put followings in your .emacs:

;; #+begin_src emacs-lisp
;;   (require 'vc-svn17)
;; #+end_src

;;; Code:

(require 'vc-svn)

(defmacro vc-svn17-defun (name args &rest body)
  (declare (indent defun))
  `(when (<= emacs-major-version 23)
     ,(if (stringp (car body))
	  `(defun ,name ,args ,(car body) ,@(cdr body))
	`(defun ,name ,args nil ,@body))))

(defmacro vc-svn17-defadvice (name args &rest body)
  (declare (indent defun))
  `(when (<= emacs-major-version 23)
     (defadvice ,name (,(car args) vc-svn17 activate) ,@body)))

(vc-svn17-defun vc-svn-root (file)
  (vc-find-root file vc-svn-admin-directory))

  
(vc-svn17-defadvice vc-svn-registered (around)
  (let ((root (vc-svn-root (ad-get-arg 0))))
    (when root
      (ad-set-arg 0 root)
      ad-do-it)))

(vc-svn17-defadvice vc-svn-responsible-p (around)
  (setq ad-return-value (vc-svn-root (ad-get-arg 0))))

(vc-svn17-defadvice vc-svn-repository-hostname (around)
  (setq
   ad-return-value
   (with-temp-buffer
     (let (process-file-side-effects)
       (vc-svn-command t t dirname "info"))
     (goto-char (point-min))
     (when (re-search-forward "^URL: *\\(.*\\)" nil t)
       (match-string 1)))))

(provide 'vc-svn17)
;;; vc-svn17.el ends here
