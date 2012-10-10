;;; memory-stats.el --- display various memory stats in the mode-line  -*- coding: mule-utf-8 -*-

;; Copyright (C) 2012 Kajetan Rzepecki

;; Author: Kajetan Rzepecki

;; Created: 1 Sep 2012

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

;; (require 'memory-stats)
;; (memory-usage-start)


;; There are a few variables to tweak:
;;   `memory-usage-update-interval' - Time interval after which current memory stats are updated.
;;   `memory-usage-format' - A string format used in the mode-line.
;;                      Supports the following escape sequences:
;;      %r - Percentile main RAM usage.
;;      %R - Percentile main RAM usage (no buffers or cache).
;;      %t - Total RAM in MB.
;;      %c - Cache in MB.
;;      %b - Buffers in MB.
;;      %f - Free main RAM in MB.
;;      %F - Total free memory (with cache and buffers) in MB.
;;      %u - Used memory in MB.
;;      %U - Used memory (no cache or buffers) in MB.
;;      %S - Swap usage in percent.
;;      %St - Total swap in MB.
;;      %Su - Used swap in MB.
;;      %Sf - Free swap in MB.

;;; TODO:

;; Maybe add HDD usage.

;;; Code:

(require 'cl)
(require 'misc-utils)

(defvar memory-usage-formatters nil)
(defvar memory-usage-timer nil)
(defvar memory-usage-mode-line-string "")

(defgroup memory-usage nil
  "Display various memory stats in the mode-line."
  :group 'memory-usage)

(defcustom memory-usage-update-interval 2
  "Number of seconds between memory stats recalculation."
  :type 'number
  :group 'memory-usage)

(defcustom memory-usage-format "%R %S"
  "Format string:
%r - Percentile main RAM usage.
%R - Percentile main RAM usage (no buffers or cache).
%t - Total RAM in MB.
%c - Cache in MB.
%b - Buffers in MB.
%f - Free main RAM in MB.
%F - Total free memory (with cache and buffers) in MB.
%u - Used memory in MB.
%U - Used memory (no cache or buffers) in MB.
%S - Swap usage in percent.
%St - Total swap in MB.
%Su - Used swap in MB.
%Sf - Free swap in MB."
  :type 'string
  :group 'memory-usage)

(defun memory-usage-start ()
  "Start displaying memory usage stats in the mode-line."
  (interactive)
  (add-to-list 'global-mode-string 'memory-usage-mode-line-string t)
  (and memory-usage-timer (cancel-timer memory-usage-timer))
  (setq memory-usage-mode-line-string "")
  (setq memory-usage-timer (run-at-time memory-usage-update-interval
                                        memory-usage-update-interval
                                        (lambda ()
                                          (setq memory-usage-mode-line-string (memory-usage))
                                          (force-mode-line-update)
                                          (sit-for 0)))))

(defun memory-usage-stop ()
  "Stop displaying memory usage stats in the mode-line."
  (interactive)
  (setq memory-usage-mode-line-string "")
  (setq global-mode-string (delq 'memory-usage-mode-line-string
                                 global-mode-string))
  (setq memory-usage-timer
        (and memory-usage-timer (cancel-timer memory-usage-timer))))

(defun memory-usage ()
  (format-memory-usage memory-usage-format))

(defun format-memory-usage (format)
  (let ((stats (memory-stats)))
    (format-expand memory-usage-formatters format stats)))

(defun memory-stats ()
  "Returns a bunch of memory stats in a form of an alist."
  (let ((stats (mapcar #'split-string
                 (remove-if (lambda (str) (string= str ""))
                   (split-string
                     (shell-command-to-string "free -o -m | tail -n2")
                     "\n")))))
    (mapcar (lambda (lst)
              (cons (car lst)
                    (mapcar #'string-to-number (cdr lst))))
            stats)))

(setq memory-usage-formatters
  (list
    ; Percentile RAM usage.
    (cons "r" (lambda (stats)
               (let* ((ram (assoc "Mem:" stats))
                      (used (nth 2 ram))
                      (total (nth 1 ram)))
                 (format "%.0f" (* 100 (/ (float used)
                                          total))))))
    ; Percentile RAM usage (no cache).
    (cons "R" (lambda (stats)
                (let* ((ram (assoc "Mem:" stats))
                       (total (nth 1 ram))
                       (used (nth 2 ram))
                       (buffers (nth 5 ram))
                       (cache (nth 6 ram)))
                  (format "%.0f" (* 100 (/ (- used buffers cache)
                                           (float total)))))))
    ; Total RAM.
    (cons "t" (lambda (stats)
                (number-to-string (nth 1 (assoc "Mem:" stats)))))
    ; Cache.
    (cons "c" (lambda (stats)
                (number-to-string (nth 6 (assoc "Mem:" stats)))))
    ; Buffers.
    (cons "b" (lambda (stats)
                (number-to-string (nth 5 (assoc "Mem:" stats)))))
    ; Free memory.
    (cons "f" (lambda (stats)
                (number-to-string (nth 3 (assoc "Mem:" stats)))))
    ; Free memory total.
    (cons "F" (lambda (stats)
                (let* ((ram (assoc "Mem:" stats))
                       (free (nth 3 ram))
                       (buffers (nth 5 ram))
                       (cache (nth 6 ram)))
                  (number-to-string (+ free buffers cache)))))
    ; Used memory.
    (cons "u" (lambda (stats)
                (number-to-string (nth 2 (assoc "Mem:" stats)))))
    ; Used memory (no buffers or cache).
    (cons "U" (lambda (stats)
                (let* ((ram (assoc "Mem:" stats))
                       (used (nth 2 ram))
                       (buffers (nth 5 ram))
                       (cache (nth 6 ram)))
                  (number-to-string (- used buffers cache)))))
    ; Total swap.
    (cons "St" (lambda (stats)
                 (number-to-string (nth 1 (assoc "Swap:" stats)))))
    ; Used swap.
    (cons "Su" (lambda (stats)
                 (number-to-string (nth 2 (assoc "Swap:" stats)))))
    ; Free swap.
    (cons "Sf" (lambda (stats)
                 (number-to-string (nth 3 (assoc "Swap:" stats)))))
    ; Percentile swap usage.
    (cons "S" (lambda (stats)
                (let* ((swap (assoc "Swap:" stats))
                       (total (nth 1 swap))
                       (used (nth 2 swap)))
                  (format "%.0f" (* 100 (/ (float used)
                                           total))))))))

(provide 'memory-stats)

;;; file ends here