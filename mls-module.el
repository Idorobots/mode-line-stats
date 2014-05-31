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
(require 'cl)

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
      (cond (module (setf module data))
            (t (add-to-list 'mls-module-list new-module)))
      new-module)))

(defun mls-module-call (name action)
  "Call the module NAME function ACTION."
  (let* ((module (mls-module~find name))
         (func (mls-module-get module action)))
    (when func
      (funcall func module))))

(defun mls-module-set-timer (module interval func)
  "Set MODULE timer with INTERVAL and FUNC."
  (mls-module-cancel-timer module)
  (when interval
    (mls-module-set module
                  :timer (run-at-time interval
                                      interval
                                      func))))

(defun mls-module-cancel-timer (module)
  "Cancel MODULE timer."
  (let ((timer (mls-module-get module :timer)))
    (when timer
      (cancel-timer timer))))

(defun mls-module~format (module stats)
  "Expand the MODULE format with STATS."
  (let ((formatters (mls-module-get module :formatters))
        (fmt (mls-module-get module :format)))
    (mls-module~format-expand formatters fmt stats)))

(defun mls-module-start (module)
  "Start MODULE."
  (let ((interval (mls-module-get module :interval)))
    (mls-module-call module :init)
    (mls-module-set module :mode-line-string "")
    (when interval
      (mls-module-set-timer module
                          interval
                          `(lambda() (mls-module-update ',module))))))

(defun mls-module-stop (module)
  "Stop MODULE."
  (mls-module-set module :mode-line-string "")
  (mls-module-cancel-timer module)
  (mls-module-call module :cleanup))

(defun mls-module-update (module)
  "Update stats.
MODULE is passed as argument when called from `mls-module-call'."
  (let* ((stats (mls-module-call module :fetch))
         (data (mls-module~format module stats)))
    (mls-module-set module :data data)
    (mls-module-set module :mode-line-string (mls-module~data-to-string data))
    (mls-module-refresh)))


;; TODO: Only used by old network-stats.
(defun mls-format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (let ((regex (concat "%\\("
                       (reduce (lambda (a b)
                                 (concat a "\\|" b))
                               (mapcar #'car formatters))
                       "\\)")))
    (replace-regexp-in-string regex
                              (lambda (str)
                                (let ((fun (assoc (substring str 1)
                                                  formatters)))
                                  (if fun
                                      (funcall (cdr fun) arg)
                                      (error "Unrecognized format sequence: %s" str))))
                              format t t)))

(defun mls-module~format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument.
Return a list."
  (let ((regex (concat "%\\("
                       (reduce (lambda (a b)
                                 (concat a "\\|" b))
                               (mapcar #'car formatters))
                       "\\)"))
        (fmt-list (split-string format)))
    (mapcar #'(lambda (fmt)
               (replace-regexp-in-string regex
                             (lambda (str)
                               (let ((fun (assoc (substring str 1)
                                                 formatters)))
                                 (if fun
                                     (funcall (cdr fun) arg)
                                   (error "Unrecognized format sequence: %s" str))))
                             fmt t))
            fmt-list)))

(defun mls-module~data-to-string (data)
  "Build module mode-line-string using DATA list."
  (mapconcat 'identity data " "))

(defun mls-module-mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
            (apply 'mls-module-mapcar* f (mapcar 'cdr xs)))))

(defun mls-module-refresh ()
  "Module update."
  (interactive)
  (when (fboundp 'mls-buffer-refresh)
    (mls-buffer-refresh))
  (force-mode-line-update)
  (sit-for 0))

(provide 'mls-module)
;;; mls-module ends here
