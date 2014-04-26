;;; mls-disk.el --- display various disk stats in the mode-line  -*- coding: mule-utf-8 -*-

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
;; mls-disk-device. If you want to use a different one you
;; can set it before calling mls-disk-start.

;; (require 'mls-disk)
;; (setq mls-disk-format "%p")
;; (setq mls-disk-device "/dev/sda3")
;; (mls-disk-start)

;;; Code:
(require 'mls-module)

(defgroup mls-disk nil
  "Display various disk stats in the mode-line."
  :group 'mls-disk)

(defvar mls-disk-default-levels '((90  "crit")
                                  (50  "warn")
                                  (0   "norm")))

(defcustom mls-disk-levels `(("%/dev/sda1_p" ,mls-disk-default-levels))
  "Module levels."
  :type 'sexp ;; FIXME: should write a better type here
  :group 'mls-disk)

(defcustom mls-disk-name "disk"
  "Module name."
  :type 'string
  :group 'mls-disk)

(defcustom mls-disk-mode-line-format "&/dev/sda1_p{d}"
  "Mode line format."
  :type 'string
  :group 'mls-disk)

(defcustom mls-disk-buffer-format "
    Used:  %/dev/sda1_p{%}
    Free:  %/dev/sda1_fG{GB}
    Total: %/dev/sda1_tG{GB}"
  "Buffer format."
  :type 'string
  :group 'mls-disk)

(defcustom mls-disk-monitor-format "&/dev/sda1_p"
  "Monitor format."
  :type 'string
  :type 'mls-disk)

(defcustom mls-disk-update-interval 15
  "Number of seconds between disk stats recalculation."
  :type 'number
  :group 'mls-disk)

(defcustom mls-disk-format "%/dev/sda1_p"
  "Format string:
%p - Percentile used DISK space.
%u# - Used space (where # is the unit: K, M, G or T).
%f# - Free space (where # is the unit: K, M, G or T).
%t# - Total space (where # is the unit: K, M, G or T)."
  :type 'string
  :group 'mls-disk)

(defun mls-disk-update (module)
  "Update stats."
  (let* ((stats (mls-module-call module :fetch))
         (data (mls-module-format-expand module stats)))
    (mls-module-set module :data data)
    (mls-module-set module :mode-line-string (mls-data-to-string data))
    (mls-module-update)))

(defun mls-disk-start (module)
  "Start displaying disk usage stats in the mode-line."
  (interactive)

  (mls-module-set module :mode-line-string "")
  (mls-module-set-timer module
                        (mls-module-get module :interval)
                        `(lambda() (mls-module-call ',module :update))))

(defun mls-disk-stop (module)
  "Stop displaying disk usage stats in the mode-line."
  (interactive)
  (mls-module-set module :mode-line-string "")
  (mls-module-cancel-timer module))

(defun mls-disk-fetch (&optional module)
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

(defun mls-disk-devices ()
  "List all devices."
  (mapcar #'car (mls-disk-fetch)))

(defun mls-disk-normalize-value (value unit)
  "Normalize VALUE using UNIT magnitude (M G or T)."
  (cond ((equal unit 'M) (/ value 1024.0))
        ((equal unit 'G) (/ value 1048576.0))
        ((equal unit 'T) (/ value 1073741824.0))
        (t value)))

(defun mls-disk-build-formatter (device formatter index &optional unit)
  "Builds a `mls-disk` FORMATTER with VALUE."
  (cons (concat device "_" formatter (when unit (symbol-name unit)))
        `(lambda (stats)
           (number-to-string
            (mls-disk-normalize-value
             (nth ,index (assoc ,device stats))
             ',unit)))))

(defun mls-disk-formatters-init ()
  "Initialize the formatters."
  (let ((formatters nil))
    (mapc #'(lambda (device)
                (add-to-list 'formatters (mls-disk-build-formatter device "p" 4))

                (add-to-list 'formatters (mls-disk-build-formatter device "u" 2 'M))
                (add-to-list 'formatters (mls-disk-build-formatter device "u" 2 'G))
                (add-to-list 'formatters (mls-disk-build-formatter device "u" 2 'T))
                (add-to-list 'formatters (mls-disk-build-formatter device "u" 2 'K))

                (add-to-list 'formatters (mls-disk-build-formatter device "f" 3 'M))
                (add-to-list 'formatters (mls-disk-build-formatter device "f" 3 'G))
                (add-to-list 'formatters (mls-disk-build-formatter device "f" 3 'T))
                (add-to-list 'formatters (mls-disk-build-formatter device "f" 3 'K))

                (add-to-list 'formatters (mls-disk-build-formatter device "t" 1 'M))
                (add-to-list 'formatters (mls-disk-build-formatter device "t" 1 'G))
                (add-to-list 'formatters (mls-disk-build-formatter device "t" 1 'T))
                (add-to-list 'formatters (mls-disk-build-formatter device "t" 1 'K)))
            (mls-disk-devices))
    formatters))

(mls-module-define `(:name ,mls-disk-name
                     :mode-line-format ,mls-disk-mode-line-format
                     :buffer-format ,mls-disk-buffer-format
                     :format ,mls-disk-format
                     :monitor-format ,mls-disk-monitor-format
                     :levels  ,mls-disk-levels
                     :interval ,mls-disk-update-interval
                     :timer nil
                     :data nil
                     :mode-line-string ""
                     :formatters ,(mls-disk-formatters-init)
                     :update     mls-disk-update
                     :fetch      mls-disk-fetch
                     :start      mls-disk-start
                     :stop       mls-disk-stop))

(provide 'mls-disk)
;;; mls-disk ends here
