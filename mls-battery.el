;;; mls-battery.el --- display various battery stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;; (require 'mls-battery)
;; (setq mls-battery-format "%p")
;; (mls-battery-start)

;;; Commentary:

;;; Code:
(require 'mls-module)

(defvar mls-battery-battery-format "%c %r %B %d %L %p %m %h %t")

(defvar mls-battery-default-levels '((75.0 "norm")
                                     (35.0 "warn")
                                     (0.0  "crit")))
(defcustom mls-battery-levels `(("%p"  ,mls-battery-default-levels))
  "Module levels."
  :type 'sexp ;; FIXME: should write a better type here
  :group 'mls-battery)

(defcustom mls-battery-name "battery"
  "Module name."
  :type 'string
  :group 'mls-battery)

(defcustom mls-battery-mode-line-format "&p{b}"
  "Mode line format."
  :type 'string
  :group 'mls-battery)

(defcustom mls-battery-buffer-format " BAT[%p{%%}]"
  "Buffer format."
  :type 'string
  :group 'mls-battery)

(defcustom mls-battery-monitor-format "&p"
  "Monitor format."
  :type 'string
  :type 'mls-battery)

(defgroup mls-battery nil
  "Display various disk stats in the mode-line."
  :group 'mls-battery)

(defcustom mls-battery-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'mls-battery)


(defcustom mls-battery-format "%p %t %r"
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
  :group 'mls-battery)

(defun mls-battery-update (module)
  "Update stats."
  (let* ((stats (mls-module-call module :fetch))
         (data (mls-module-format-expand module stats)))
    (mls-module-set module :data data)
    (mls-module-set module :mode-line-string (mls-data-to-string data))
    (mls-module-update)))

(defun mls-battery-start (module)
  "Start displaying battery usage stats in the mode-line."
  (interactive)

  (let ((interval (mls-module-get module :interval)))
    (mls-module-set module :mode-line-string "")
    (setq battery-mode-line-format mls-battery-battery-format)
    (display-battery-mode)
    (setq global-mode-string (delq 'battery-mode-line-string
                                   global-mode-string))
    (mls-module-set-timer module
                          interval
                          `(lambda() (mls-module-call ',module :update)))))

(defun mls-battery-stop (module)
  "Stop displaying battery usage stats in the mode-line."
  (interactive)
  (mls-module-set module :mode-line-string "")
  (mls-module-cancel-timer module)
  (display-battery-mode -1))

(defun mls-battery-fetch (&optional module)
  "Return a bunch of battery stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                       (split-string (substring-no-properties battery-mode-line-string) " "))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(defun mls-battery-formatters-init ()
  "Build battery formatters."
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

(mls-module-define `(:name ,mls-battery-name
                     :mode-line-format ,mls-battery-mode-line-format
                     :buffer-format ,mls-battery-buffer-format
                     :format ,mls-battery-format
                     :monitor-format ,mls-battery-monitor-format
                     :levels  ,mls-battery-levels
                     :interval ,mls-battery-update-interval
                     :timer nil
                     :data nil
                     :mode-line-string ""
                     :formatters ,(mls-battery-formatters-init)
                     :update     mls-battery-update
                     :fetch      mls-battery-fetch
                     :start      mls-battery-start
                     :stop       mls-battery-stop))

(provide 'mls-battery)
;;; mls-battery.el ends here
