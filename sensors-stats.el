;;; sensors-stats.el --- display various sensors stats in the mode-line  -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 10-06-2013

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

;; (require 'sensors-stats)
;; (setq sensors-stats-patterns
;;   '(("c0" "Core 0")
;;     ("c1" "Core 1")))
;; (sensors-stats-start)

;;; Commentary:

;;; Code:
(require 'mls-utils)

(defvar sensors-stats-formatters nil)
(defvar sensors-stats-timer nil)
(defvar sensors-stats-mode-line-string "")
(defvar sensors-stats-use-global-mode-string t)
(defvar sensors-stats-command "sensors")
(defvar sensors-stats-patterns
  '(("c0" "Core 0")
    ("c1" "Core 1"))
  "Alist of patterns.
First value is the formatter.
Second value is the `sensors` label.")

(defvar sensors-stats-settings
  '((:formats
     ((:primary "&c0{t}")
      (:secondary " SENSORS[%c0{ºC}]")
      (:monitor "&c0")))
    (:levels
     (("%c0" ((60.0 "crit")
              (40.0 "warn")
              (0.0  "norm"))))))
  "SENSORS stats settings.")

(defgroup sensors-stats nil
  "Display various sensors stats in the mode-line."
  :group 'sensors-stats)

(defcustom sensors-stats-update-interval 15
  "Number of seconds between sensors stats recalculation."
  :type 'number
  :group 'sensors-stats)

(defvar sensors-stats-format nil)

(defun sensors-stats-start ()
  "Start displaying sensors usage stats in the mode-line."
  (interactive)
  (when sensors-stats-use-global-mode-string
    (add-to-list 'global-mode-string 'sensors-stats-mode-line-string t))

  (sensors-stats-formatters-init)

  (and sensors-stats-timer (cancel-timer sensors-stats-timer))
  (setq sensors-stats-mode-line-string "")
  (setq sensors-stats-timer (run-at-time sensors-stats-update-interval
                                        sensors-stats-update-interval
                                        (lambda ()
                                          (setq sensors-stats-mode-line-string (sensors-stats))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun sensors-stats-stop ()
  "Stop displaying sensors usage stats in the mode-line."
  (interactive)
  (setq sensors-stats-mode-line-string "")
  (when sensors-stats-use-global-mode-string
    (setq global-mode-string (delq 'sensors-stats-mode-line-string
                                   global-mode-string)))
  (setq sensors-stats-timer
        (and sensors-stats-timer (cancel-timer sensors-stats-timer))))

(defun sensors-stats ()
  "Build the stats."
  (format-sensors-stats sensors-stats-format))

(defun format-sensors-stats (format)
  "Expand the FORMAT."
  (let ((stats (sensors-stats-fetch)))
    (mls-format-expand sensors-stats-formatters format stats)))

(defun sensors-stats-fetch ()
  "Return a bunch of sensors stats in a form of an alist."
  (let ((stats (mapcar #'(lambda (s) (split-string s ":"))
                 (delete "" (split-string
                   (shell-command-to-string sensors-stats-command)
                   "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(defun sensors-stats-formatters-init ()
  "Initialize the formatters."
  (setq sensors-stats-format (mapconcat #'(lambda (pattern)
                                            (concat "%" (car pattern)))
                                        sensors-stats-patterns
                                        " "))
  (setq sensors-stats-formatters
        (mapcar #'(lambda (pattern-list)
                    `(,(car pattern-list)
                           lambda (stats)
                           (number-to-string (cadr (assoc ,(cadr pattern-list) stats)))))
                sensors-stats-patterns)))

(provide 'sensors-stats)
;;; sensors-stats ends here
