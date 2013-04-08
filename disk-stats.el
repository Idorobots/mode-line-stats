;;; disk-stats.el --- display various disk stats in the mode-line  -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies
;; Copyright (C) 2012 Kajetan Rzepecki

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 05-04-2013

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


;;; Usage:

;; (require 'disk-stats)
;; (setq disk-usage-format "%p")
;; (setq disk-usage-partition "/")
;; (disk-usage-start)

;;; Code:

(require 'cl)
(require 'misc-utils)

(defvar disk-usage-formatters nil)
(defvar disk-usage-partition "/")
(defvar disk-usage-timer nil)
(defvar disk-usage-mode-line-string "")
(defvar disk-usage-use-global-mode-string t)

(defgroup disk-usage nil
  "Display various disk stats in the mode-line."
  :group 'disk-usage)

(defcustom disk-usage-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'disk-usage)

(defcustom disk-usage-format "%p"
  "Format string:
%p - Percentile used DISK space.
%u - Used space (in KB).
%f - Free space (in KB).
%t - Total space (in KB)."
  :type 'string
  :group 'disk-usage)

(defun disk-usage-start ()
  "Start displaying disk usage stats in the mode-line."
  (interactive)
  (when disk-usage-use-global-mode-string
    (add-to-list 'global-mode-string 'disk-usage-mode-line-string t))

  (and disk-usage-timer (cancel-timer disk-usage-timer))
  (setq disk-usage-mode-line-string "")
  (setq disk-usage-timer (run-at-time disk-usage-update-interval
                                        disk-usage-update-interval
                                        (lambda ()
                                          (setq disk-usage-mode-line-string (disk-usage))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun disk-usage-stop ()
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (setq disk-usage-mode-line-string "")
  (when disk-usage-use-global-mode-string
    (setq global-mode-string (delq 'disk-usage-mode-line-string
                                   global-mode-string)))
  (setq disk-usage-timer
        (and disk-usage-timer (cancel-timer disk-usage-timer))))

(defun disk-usage ()
  (format-disk-usage disk-usage-format))

(defun format-disk-usage (format)
  (let ((stats (disk-stats)))
    (format-expand disk-usage-formatters format stats)))

(defun disk-stats ()
  "Returns a bunch of disk stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                 (remove-if (lambda (str) (string= str ""))
                   (split-string
                     (shell-command-to-string (concat "df | tail -n +2"))
                     "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(setq disk-usage-formatters
  (list
    ; Percentile DISK usage.
    (cons "p" (lambda (stats)
                (number-to-string (nth 4 (assoc disk-usage-partition stats)))))
    (cons "u" (lambda (stats)
                (number-to-string (nth 2 (assoc disk-usage-partition stats)))))
    (cons "f" (lambda (stats)
                (number-to-string (nth 3 (assoc disk-usage-partition stats)))))
    (cons "t" (lambda (stats)
                (number-to-string (nth 1 (assoc disk-usage-partition stats)))))))

(provide 'disk-stats)

;;; file ends here
