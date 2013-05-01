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

;; By default, the first device found in df will be used as
;; disk-usage-device. If you want to use a different one you
;; can set it before calling disk-usage-start.

;; (require 'disk-stats)
;; (setq disk-stats-format "%p")
;; (setq disk-stats-device "/dev/sda3")
;; (disk-stats-start)

;;; Code:

(require 'cl)
(require 'misc-utils)

(defvar disk-stats-formatters nil)
(defvar disk-stats-device nil)
(defvar disk-stats-timer nil)
(defvar disk-stats-mode-line-string "")
(defvar disk-stats-use-global-mode-string t)

(defvar disk-stats-settings
  '((:formats
     ((:primary "&p{d}")
      (:secondary "DISK[%p{%%}]")
      (:monitor "&p")))
    (:levels
     (("%p" ((90.0 "crit")
             (50.0 "warn")
             (0.0  "norm"))))))
  "DISK stats settings.")

(defgroup disk-stats nil
  "Display various disk stats in the mode-line."
  :group 'disk-stats)

(defcustom disk-stats-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'disk-stats)

(defcustom disk-stats-format "%p"
  "Format string:
%p - Percentile used DISK space.
%u - Used space (in KB).
%f - Free space (in KB).
%t - Total space (in KB)."
  :type 'string
  :group 'disk-stats)

(defun disk-stats-start ()
  "Start displaying disk usage stats in the mode-line."
  (interactive)
  (when disk-stats-use-global-mode-string
    (add-to-list 'global-mode-string 'disk-stats-mode-line-string t))

  (unless disk-stats-device
    (setq disk-stats-device (caar (disk-stats-fetch))))

  (and disk-stats-timer (cancel-timer disk-stats-timer))
  (setq disk-stats-mode-line-string "")
  (setq disk-stats-timer (run-at-time disk-stats-update-interval
                                        disk-stats-update-interval
                                        (lambda ()
                                          (setq disk-stats-mode-line-string (disk-stats))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun disk-stats-stop ()
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (setq disk-stats-mode-line-string "")
  (when disk-stats-use-global-mode-string
    (setq global-mode-string (delq 'disk-stats-mode-line-string
                                   global-mode-string)))
  (setq disk-stats-timer
        (and disk-stats-timer (cancel-timer disk-stats-timer))))

(defun disk-stats ()
  (format-disk-stats disk-stats-format))

(defun format-disk-stats (format)
  (let ((stats (disk-stats-fetch)))
    (format-expand disk-stats-formatters format stats)))

(defun disk-stats-fetch ()
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

(setq disk-stats-formatters
  (list
    ; Percentile DISK usage.
    (cons "p" (lambda (stats)
                (number-to-string (nth 4 (assoc disk-stats-device stats)))))
    (cons "u" (lambda (stats)
                (number-to-string (nth 2 (assoc disk-stats-device stats)))))
    (cons "f" (lambda (stats)
                (number-to-string (nth 3 (assoc disk-stats-device stats)))))
    (cons "t" (lambda (stats)
                (number-to-string (nth 1 (assoc disk-stats-device stats)))))))

(provide 'disk-stats)

;;; file ends here
