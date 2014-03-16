;;; mls-module.el --- mode-line-stats module helper -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 20-02-2014

;; Keywords: hardware

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage:

;;; Code:
;; TODO: merge mls-common with mls-module
(require 'mls-common)

(defvar mls-module-list nil)

(defun mls-module~valid-p (module)
  "Check if MODULE is valid."
  (let ((name (plist-get module :name))
        (format (plist-get module :format))
        (formatters (plist-get module :formatters)))
    (and (and (stringp name)
              (string-match-p ".+" name))
         (and (stringp format)
              (string-match-p ".+" format))
         ;; TODO: add more checks...
         (and (listp formatters)))))

(defun mls-module~find (module-or-name)
  "Find module using MODULE-OR-NAME."
  (if (stringp module-or-name)
      (cadr (assoc module-or-name mls-module-list))
    module-or-name))

(defun mls-module-get (module-or-name &optional prop)
  "Get module  using MODULE-OR-NAME.
If PROP is submitted it will return the module property."
  (let ((module (mls-module~find module-or-name)))
    (if prop
        (plist-get module prop)
      module)))

(defun mls-module-set (module-or-name prop value)
  "Set in MODULE-OR-NAME the PROP with VALUE."
  (let ((module (mls-module~find module-or-name)))
    (plist-put module prop value)))

(defun mls-module-define (data)
  "Define a new module with DATA (plist)."
  (let* ((name (plist-get data :name))
         (new-module `(,name ,data))
         (module (mls-module-get name)))
    (when (mls-module~valid-p data)
      (cond (module (setf (cadr module) data))
            (t (add-to-list 'mls-module-list new-module)))
      new-module)))

(defun mls-module-call (name action)
  "Call the module NAME function ACTION."
  (let* ((module (mls-module~find name))
         (func (mls-module-get module action)))
    (funcall func module)))

(defun mls-module-set-timer (module interval func)
  "Set MODULE timer with INTERVAL and FUNC."
  (mls-module-cancel-timer module)
  (mls-module-set module
                  :timer (run-at-time interval
                                      interval
                                      func)))

(defun mls-module-cancel-timer (module)
  "Cancel MODULE timer."
  (let ((timer (mls-module-get module :timer)))
    (when timer
      (cancel-timer timer))))

(defun mls-module-format-expand (module stats)
  "Expand the MODULE format with STATS."
  (let ((formatters (mls-module-get module :formatters))
        (fmt (mls-module-get module :format)))
    (mls-format-expand-list formatters fmt stats)))

(provide 'mls-module)
;;; mls-module ends here
