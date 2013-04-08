;;; cpu-stats.el --- display various CPU stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;; (require 'cpu-stats)
;; (cpu-usage-start)


;; There are a few variables to tweak:
;;   `cpu-usage-update-interval' - Time interval after which current CPU stats are updated.
;;   `cpu-usage-format' - A string format used in the mode-line.
;;                      Supports the following escape sequences:
;;      %A - Average CPU usage across multiple cores in percent.
;;      %C# - CPU usage of core number # (starting from 0) in percent.
;;      %_u - User CPU usage of core _ (either A or C#).
;;      %_s - System CPU usage of core _.
;;      %_i - IO CPU usage of core _.

;;; TODO:

;; CPU temperature, frequency, etc.

;;; Code:

(require 'cl)
(require 'misc-utils)

(defvar *previous-stats* nil)
(defvar cpu-usage-timer nil)
(defvar cpu-usage-mode-line-string "")
(defvar cpu-usage-formatters nil)
(defvar cpu-usage-use-global-mode-string t)

(defgroup cpu-usage nil
  "Display various CPU stats in the mode-line."
  :group 'cpu-usage)

(defcustom cpu-usage-update-interval 2
  "Number of seconds between CPU stats recalculation."
  :type 'number
  :group 'cpu-usage)

(defcustom cpu-usage-format "%A %C0 %C1"
  "Format string:
%A - average CPU usage in percent.
%C# - CPU usage of #th core in percent.
%_u - User CPU usage of core _ (either \"A\" or \"C#\").
%_s - System CPU usage of core _.
%_i - IO CPU usage of core _."
  :type 'string
  :group 'cpu-usage)

(defun cpu-usage-start ()
  "Start displaying CPU usage stats in the mode-line."
  (interactive)
  (when cpu-usage-use-global-mode-string
    (add-to-list 'global-mode-string 'cpu-usage-mode-line-string t))
  (and cpu-usage-timer (cancel-timer cpu-usage-timer))
  (setq cpu-usage-mode-line-string "")
  (setq *previous-stats* (read-stats))
  (setq cpu-usage-timer (run-at-time cpu-usage-update-interval
                                     cpu-usage-update-interval
                                     (lambda ()
                                       (setq cpu-usage-mode-line-string (cpu-usage))
                                       (force-mode-line-update)
                                       (sit-for 0)))))

(defun cpu-usage-stop ()
  "Stop displaying CPU usage stats in the mode-line."
  (interactive)
  (setq cpu-usage-mode-line-string "")
  (when cpu-usage-use-global-mode-string
    (setq global-mode-string (delq 'cpu-usage-mode-line-string
                                   global-mode-string)))
  (setq cpu-usage-timer
        (and cpu-usage-timer (cancel-timer cpu-usage-timer))))

(defun cpu-usage ()
  (format-cpu-usage cpu-usage-format))

(defun format-cpu-usage (format)
  (let ((stats (cpu-stats)))
    (format-expand cpu-usage-formatters format stats)))

(defun cpu-stats ()
  "Returns a bunch of CPU stats for each core and averages in a form of an alist."
  ;; TODO Needs moar stats.
  (let* ((cores (remove-if-not (lambda (line)
                                 (string-prefix-p "cpu" (car line)))
                               (read-stats)))
         (stats (cl-mapcar-many
                        (lambda (cpu prev-cpu)
                          (let* ((norm-user (nth 1 cpu))
                                 (nice-user (nth 2 cpu))
                                 (user (+ norm-user nice-user))
                                 (sys (nth 3 cpu))
                                 (idle (nth 4 cpu))
                                 (iowait (or (nth 5 cpu) 0))
                                 (prev-norm-user (nth 1 prev-cpu))
                                 (prev-nice-user (nth 2 prev-cpu))
                                 (prev-user (+ prev-norm-user prev-nice-user))
                                 (prev-sys (nth 3 prev-cpu))
                                 (prev-idle (nth 4 prev-cpu))
                                 (prev-iowait (or (nth 5 prev-cpu) 0))
                                 (step-denom (float (- (+ user sys idle iowait)
                                                       (+ prev-user prev-sys prev-idle prev-iowait))))
                                 (user-result (/ (- user prev-user)
                                                 step-denom))
                                 (sys-result (/ (- sys prev-sys)
                                                step-denom))
                                 (io-result (/ (- iowait prev-iowait)
                                               step-denom)))
                          (list (car cpu) user-result sys-result io-result)))
                        (list cores *previous-stats*))))
    (setq *previous-stats* cores)
    stats))

(defun read-stats ()
  "Reads /proc/stat and returns an alist of cores every `cpu-usage-update-interval'."
  (let ((stats (remove-if (lambda (str) (string= str ""))
                 (split-string
                   (shell-command-to-string "cat /proc/stat")
                   "\n"))))
    (mapcar (lambda (str)
              (let ((s (split-string str)))
                (cons (car s)
                      (mapcar #'string-to-number
                              (cdr s)))))
            stats)))

(defmacro make-cpu-functions (name cpu-name)
  `(list
    ;; User CPU usage.
    (cons ,(concat name "u")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 1 (assoc ,cpu-name stats))))))
    ;; System CPU usage.
    (cons ,(concat name "s")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 2 (assoc ,cpu-name stats))))))
    ;; IO CPU usage.
    (cons ,(concat name "i")
          (lambda (stats)
            (format "%.0f" (* 100 (nth 3 (assoc ,cpu-name stats))))))
    ;; CPU usage.
    (cons ,name
          (lambda (stats)
            (format "%.0f" (* 100 (apply #'+ (cdr (assoc ,cpu-name stats)))))))))

(setq cpu-usage-formatters
  `(,@(make-cpu-functions "A" "cpu")
    ,@(make-cpu-functions "C0" "cpu0")
    ,@(make-cpu-functions "C1" "cpu1")
    ,@(make-cpu-functions "C2" "cpu2")
    ,@(make-cpu-functions "C3" "cpu3")
    ,@(make-cpu-functions "C4" "cpu4")
    ,@(make-cpu-functions "C5" "cpu5")
    ,@(make-cpu-functions "C6" "cpu6")
    ,@(make-cpu-functions "C7" "cpu7")
    ; ...
))

(provide 'cpu-stats)

;;; file ends here
