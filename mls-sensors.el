;;; mls-sensors.el --- display various sensors stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;; (require 'mls-sensors)
;; (setq mls-sensors-patterns
;;   '(("c0" "Core 0")
;;     ("c1" "Core 1")))
;; (mls-sensors-start)

;;; Commentary:

;;; Code:
(require 'mls-module)

(defvar mls-sensors-command "sensors")
(defvar mls-sensors-patterns
  '(("c0" "Core 0")
    ("c1" "Core 1"))
  "Alist of patterns.
First value is the formatter.
Second value is the `sensors` label.")

(defvar mls-sensors-default-levels '((60.0 "crit")
                                     (40.0 "warn")
                                     (0.0  "norm")))
(defcustom mls-sensors-levels `(("%c0"  ,mls-sensors-default-levels)
                                ("%c1" ,mls-sensors-default-levels))
  "Module levels."
  :type 'sexp ;; FIXME: should write a better type here
  :group 'mls-sensors)

(defcustom mls-sensors-name "sensors"
  "Module name."
  :type 'string
  :group 'mls-sensors)

(defcustom mls-sensors-mode-line-format "&c0{t}"
  "Mode line format."
  :type 'string
  :group 'mls-sensors)

(defcustom mls-sensors-buffer-format "
    Cpu0 temp: %c0{ºC}
    Cpu1 temp: %c1{ºC}"
  "Buffer format."
  :type 'string
  :group 'mls-sensors)

(defcustom mls-sensors-monitor-format "&c0"
  "Monitor format."
  :type 'string
  :type 'mls-sensors)

(defgroup mls-sensors nil
  "Display various sensors stats in the mode-line."
  :group 'mls-sensors)

(defcustom mls-sensors-update-interval 15
  "Number of seconds between sensors stats recalculation."
  :type 'number
  :group 'mls-sensors)

(defvar mls-sensors-format nil)

(defun mls-sensors-update (module)
  "Update stats."
(let* ((stats (mls-module-call module :fetch))
         (data (mls-module-format-expand module stats)))
    (mls-module-set module :data data)
    (mls-module-set module :mode-line-string (mls-data-to-string data))
    (mls-module-update)))

(defun mls-sensors-start (module)
  "Start displaying sensors usage stats in the mode-line."
  (interactive)
  (let ((interval (mls-module-get module :interval)))
    (mls-module-set module :mode-line-string "")
    (mls-module-set-timer module
                          interval
                          `(lambda() (mls-module-call ',module :update)))))

(defun mls-sensors-stop (module)
  "Stop displaying sensors usage stats in the mode-line."
  (interactive)
  (mls-module-set module :mode-line-string "")
  (mls-module-cancel-timer module))

(defun mls-sensors-fetch (&optional module)
  "Return a bunch of sensors stats in a form of an alist."
  (let ((stats (mapcar #'(lambda (s) (split-string s ":"))
                 (delete "" (split-string
                   (shell-command-to-string mls-sensors-command)
                   "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(defun mls-sensors-format-init ()
  "Initialize the formatters."
  (mapconcat #'(lambda (pattern)
                 (concat "%" (car pattern)))
             mls-sensors-patterns
             " "))

(defun mls-sensors-formatters-init ()
  "Initialize the formatters."
  (mapcar #'(lambda (pattern-list)
              `(,(car pattern-list)
                lambda (stats)
                (number-to-string (cadr (assoc ,(cadr pattern-list) stats)))))
          mls-sensors-patterns))

(mls-module-define `(:name ,mls-sensors-name
                     :mode-line-format ,mls-sensors-mode-line-format
                     :buffer-format ,mls-sensors-buffer-format
                     :format ,(mls-sensors-format-init)
                     :monitor-format ,mls-sensors-monitor-format
                     :levels  ,mls-sensors-levels
                     :interval ,mls-sensors-update-interval
                     :timer nil
                     :data nil
                     :mode-line-string ""
                     :formatters ,(mls-sensors-formatters-init)
                     :update     mls-sensors-update
                     :fetch      mls-sensors-fetch
                     :start      mls-sensors-start
                     :stop       mls-sensors-stop))

(provide 'mls-sensors)
;;; mls-sensors ends here
