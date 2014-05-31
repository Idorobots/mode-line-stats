;;; mls-cpu.el --- display various CPU stats in the mode-line  -*- coding: mule-utf-8 -*-

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

;;; Commentary:

;;; Usage:

;; (require 'mls-cpu)
;; (mls-cpu-start)


;; There are a few variables to tweak:
;;   `mls-cpu-update-interval' - Time interval after which current CPU stats are updated.
;;   `mls-cpu-format' - A string format used in the mode-line.
;;                      Supports the following escape sequences:
;;      %A - Average CPU usage across multiple cores in percent.
;;      %C# - CPU usage of core number # (starting from 0) in percent.
;;      %_u - User CPU usage of core _ (either A or C#).
;;      %_s - System CPU usage of core _.
;;      %_i - IO CPU usage of core _.

;;; TODO:

;; CPU temperature, frequency, etc.

;;; Code:
(require 'mls-module)

(defgroup mls-cpu nil
  "Display various CPU stats in the mode-line."
  :group 'mls-cpu)

(defcustom mls-cpu-update-interval 2
  "Number of seconds between CPU stats recalculation."
  :type 'number
  :group 'mls-cpu)

(defvar mls-cpu-default-levels '((90  "crit")
                                 (50  "warn")
                                 (0   "norm")))
(defcustom mls-cpu-levels `(("%A"  ,mls-cpu-default-levels)
                            ("%C0" ,mls-cpu-default-levels)
                            ("%C1" ,mls-cpu-default-levels))
  "Module levels."
  :type 'sexp ;; FIXME: should write a better type here
  :group 'mls-cpu)

(defcustom mls-cpu-name "cpu"
  "Module name."
  :type 'string
  :group 'mls-cpu)

(defcustom mls-cpu-mode-line-format "&C0{c}"
  "Mode line format."
  :type 'string
  :group 'mls-cpu)

(defcustom mls-cpu-buffer-format "%C0{%}"
  "Buffer format."
  :type 'string
  :group 'mls-cpu)

(defcustom mls-cpu-format "%A %C0 %C1"
  "Format string:
%A - average CPU usage in percent.
%C# - CPU usage of #th core in percent.
%_u - User CPU usage of core _ (either \"A\" or \"C#\").
%_s - System CPU usage of core _.
%_i - IO CPU usage of core _."
  :type 'string
  :group 'mls-cpu)

(defcustom mls-cpu-monitor-format "&A"
  "Monitor format."
  :type 'string
  :type 'mls-cpu)

(defun mls-cpu-fetch (&optional module)
  "Return a bunch of CPU stats for each core and averages in a form of an alist.
MODULE is passed as argument when called from `mls-module-call'."
  ;; TODO Needs moar stats.
  (let* ((cores (remove-if-not (lambda (line)
                                 (string-prefix-p "cpu" (car line)))
                               (mls-cpu-read-stats)))
         (previous-stats (mls-module-get module :previous-stats))
         (stats (mls-module-mapcar*
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
                        cores previous-stats)))
    (mls-module-set module :previous-stats cores)
    stats))

(defun mls-cpu-read-stats (&optional module)
  "Read /proc/stat and return an alist of cores every `mls-cpu-update-interval'.
MODULE is passed as argument when called from `mls-module-call'."
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

(defmacro mls-cpu-make-functions (name cpu-name)
  "Macro to make formatters functions using NAME and CPU-NAME."
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

(defun mls-cpu-formatters-init ()
  "Build cpu formatteres."
  `(,@(mls-cpu-make-functions "A" "cpu")
    ,@(mls-cpu-make-functions "C0" "cpu0")
    ,@(mls-cpu-make-functions "C1" "cpu1")
    ,@(mls-cpu-make-functions "C2" "cpu2")
    ,@(mls-cpu-make-functions "C3" "cpu3")
    ,@(mls-cpu-make-functions "C4" "cpu4")
    ,@(mls-cpu-make-functions "C5" "cpu5")
    ,@(mls-cpu-make-functions "C6" "cpu6")
    ,@(mls-cpu-make-functions "C7" "cpu7")
    ; ...
))

(defun mls-cpu-init (&optional module)
  "Start MODULE displaying CPU usage stats in the mode-line."
  (mls-module-set module
                  :previous-stats (mls-module-call module :read-stats)))

(mls-module-define `(:name ,mls-cpu-name
                     :mode-line-format ,mls-cpu-mode-line-format
                     :buffer-format ,mls-cpu-buffer-format
                     :format ,mls-cpu-format
                     :monitor-format ,mls-cpu-monitor-format
                     :levels  ,mls-cpu-levels
                     :interval ,mls-cpu-update-interval
                     :timer nil
                     :previous-stats nil
                     :data nil
                     :mode-line-string ""
                     :formatters ,(mls-cpu-formatters-init)
                     :init       mls-cpu-init
                     :fetch      mls-cpu-fetch
                     :read-stats mls-cpu-read-stats))

;; (mls-module-call "cpu" :start)

(provide 'mls-cpu)
;;; mls-cpu.el ends here
