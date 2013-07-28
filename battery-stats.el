;;; battery-stats.el --- display various battery stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;; (require 'battery-stats)
;; (setq battery-stats-format "%p")
;; (battery-stats-start)

;;; Commentary:

;;; Code:

(require 'cl)
(require 'mls-utils)

(defvar battery-stats-formatters nil)
(defvar battery-stats-timer nil)
(defvar battery-stats-mode-line-string "")
(defvar battery-stats-use-global-mode-string t)

(defvar battery-stats-battery-format "%c %r %B %d %L %p %m %h %t")

(defvar battery-stats-settings
  '((:formats
     ((:primary "&p{b}")
      (:secondary " BAT[%p{%%}]")
      (:monitor "&p")))
    (:levels
     (("%p" ((75.0 "norm")
             (35.0 "warn")
             (0.0  "crit"))))))
  "BATTERY stats settings.")

(defgroup battery-stats nil
  "Display various disk stats in the mode-line."
  :group 'battery-stats)

(defcustom battery-stats-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'battery-stats)


(defcustom battery-stats-format "%p %t %r"
  "Format string:
%c Current capacity (mAh or mWh)
%r Current rate of charge or discharge
%B Battery status (verbose)
%b Battery status: empty means high, `-' means low,
   `!' means critical, and `+' means charging
%d Temperature (in degrees Celsius)
%L AC line status (verbose)
%p Battery load percentage
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  :type 'string
  :group 'battery-stats)

(defun battery-stats-start ()
  "Start displaying disk usage stats in the mode-line."
  (interactive)

  (setq battery-mode-line-format battery-stats-battery-format)
  (display-battery-mode)
  (setq global-mode-string (delq 'battery-mode-line-string
                                 global-mode-string))
  (when battery-stats-use-global-mode-string
    (add-to-list 'global-mode-string 'battery-stats-mode-line-string t))

  (and battery-stats-timer (cancel-timer battery-stats-timer))
  (setq battery-stats-mode-line-string "")
  (setq battery-stats-timer (run-at-time battery-stats-update-interval
                                        battery-stats-update-interval
                                        (lambda ()
                                          (setq battery-stats-mode-line-string (battery-stats))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun battery-stats-stop ()
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (setq battery-stats-mode-line-string "")
  (when battery-stats-use-global-mode-string
    (setq global-mode-string (delq 'battery-stats-mode-line-string
                                   global-mode-string)))
  (setq battery-stats-timer
        (and battery-stats-timer (cancel-timer battery-stats-timer)))
  (display-battery-mode -1))

(defun battery-stats ()
  ""
  (format-battery-stats battery-stats-format))

(defun format-battery-stats (format)
  ""
  (let ((stats (battery-stats-fetch)))
    (mls-format-expand battery-stats-formatters format stats)))

(defun battery-stats-fetch ()
  "Return a bunch of disk stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                       (split-string (substring-no-properties battery-mode-line-string) " "))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(setq battery-stats-formatters
  (list
    (cons "c" (lambda (stats)
                (car (nth 0 stats))))
    (cons "r" (lambda (stats)
                (car (nth 1 stats))))
    (cons "B" (lambda (stats)
                (car (nth 3 stats))))
    (cons "d" (lambda (stats)
                (car (nth 4 stats))))
    (cons "L" (lambda (stats)
                (car (nth 5 stats))))
    (cons "p" (lambda (stats)
                (car (nth 6 stats))))
    (cons "m" (lambda (stats)
                (car (nth 7 stats))))
    (cons "h" (lambda (stats)
                (car (nth 8 stats))))
    (cons "t" (lambda (stats)
                (car (nth 9 stats))))))

(provide 'battery-stats)
;;; battery-stats.el ends here
