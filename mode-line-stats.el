;;; @(#) mode-line-stats.el --- emacs mode line stats mode

;; This file is not part of Emacs

;; Copyright (C) 2013 Andreu Gil Pàmies
;; Filename: mode-line-stats.el
;; Version: 0.1
;; Keywords: hardware
;; Author: Andreu Gil Pàmies <agpchil@gmail.com>
;; Maintainer: Andreu Gil Pàmies <agpchil@gmail.com>
;; Created: 05-04-2013
;; Description: minor mode to display stats in mode-line
;; URL: http://github.com/agpchil/mode-line-stats
;; Compatibility: Emacs24


;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'mode-line-stats)
;;     (mode-line-stats-mode)
;;

;;; Commentary:

;; * Change the default keybinding for toggle mode-lines
;;
;;    (setq mls-toggle-key (kbd "C-º"))

;; * Configure which modules are enabled
;;
;;    (setq mls-modules '(battery cpu disk memory))

;; * Module settings
;;
;;    Each module have an alist of settings with a specific name.
;;    For cpu module is 'mls-cpu-settings'.
;;    This alist have the following structure:
;;
;;    (setq mls-cpu-settings
;;      '((:formats
;;         ((:primary "&A{c}")
;;          (:secondary "CPU[%C0{%%},%C1{%%}]")
;;          (:monitor "&A")))
;;        (:levels
;;         (("%A" ((90.0 "crit")
;;                 (50.0 "warn")
;;                 (0.0  "norm")))
;;          ("%C0" ((90.0 "crit")
;;                  (50.0 "warn")
;;                  (0.0  "norm")))
;;          ("%C1" ((90.0 "crit")
;;                  (50.0 "warn")
;;                  (0.0  "norm")))))))
;;
;;    :primary format is the one used in the current mode-line.
;;    :secondary is for the alternative mode-line.
;;    :monitor can only contain one formatter and is used to
;;    call a hook.
;;
;;    :levels contains an alist with the levels of each formatter.
;;    The values of a level are matched in sequencial order applying
;;    a 'greather than' comparator with the current value.  Each level
;;    has a face defined.  For example:
;;      "mls-crit-primary-face" for primary mode-line.
;;      "mls-crit-secondary-face" for secondary mode-line.
;;
;;    The formatters available are the ones supported by the modules
;;    with minor enhacements.  For example you can add '{text}' after
;;    a formatter to add text colorized depending of the level.
;;    Also you can hide the value using & instead of %.

;; * Expanding a formatter when an specific level raised
;;
;;    mls-monitor-hook allows you to run functions and change settings
;;    temporary.  The settings will be restore afterwards.
;;    For example you can expand the formatter when the critical level
;;    is reached.
;;
;;    (add-hook 'mls-monitor-hook
;;       '(lambda (name level settings format-type)
;;          (cond ((and (string= format-type :primary)
;;                (string= name "cpu")
;;                (string= level "crit"))
;;                 (mls-set-setting settings (list :formats format-type)
;;                                           "&A{CPU: }%A{%% }")))))
;;
;;    Special note: Inside the hook you can't add formatters that are
;;    not already in :primary or :secondary formats.

;; * Change the character of no data available
;;
;;    While the isn't data available to display a '?' is displayed
;;    instead.  You can change it with:
;;
;;     (setq mls-no-data-string "?")
;;
;;    The same string will be used in primary and secondary mode-line.

;; * Change the stats order in mode-lines
;;
;;    You can directly change the format of the primary
;;    and secondary mode-lines by changing the variables:
;;
;;    mls-mode-line-format-primary
;;    mls-mode-line-format-secondary

;;; Change Log:


;;; Code:
(require 'dash)
(require 'mls-module)
(require 'mls-buffer)

(defgroup mode-line-stats nil
  "Minor mode for mode-line-stats"
  :prefix "mode-line-stats"
  :group 'help
  :link '(url-link "http://github.com/agpchil/mode-line-stats"))

(defvar mls-format-regexp
  "\\([\%\&][^ \{]+\\)\\(\{\\([^\}]+\\)?\}\\)?"
  "Regexp to parse formatters in the format strings.")

(defvar mls-modules '(cpu memory disk misc sensors)
  "Modules enabled.")

(defvar mls-mode-line-string ""
  "The entry to the mode line.")
(put 'mls-mode-line-string 'risky-local-variable t)

(defvar mls-format-keys '(:mode-line-format :buffer-format :monitor-format))

(defvar mls-position :left
  "Set the position of mls.
Available values are :left, :right, :header-line and
:global-mode-string.")

(defvar mls-pretty-float-func 'mls-pretty-float-handler
  "Function to prettify float value.")

(defvar mls-pretty-percent-func 'mls-pretty-percent-handler
  "Function to prettify percent value.")

(defvar mls-pretty-integer-func 'mls-pretty-integer-handler
  "Function to prettify integer value.")

(defvar mls-pretty-string-func 'mls-pretty-string-handler
  "Function to prettify string value.")

(defvar mls-pretty-percent-width 30
  "Percent graph width.")

(defvar mls-monitor-hook nil
  "Hook to run after the monitor format is processed.
The hook function will receive the params corresponding to
the hook format processed:
NAME: current module name string
LEVEL: current level name
MODULE-ALIST: an alist with the module settings
MODULE-FMT-TYPE: a keyword with the module fmt type.

The values of MODULE-ALIST can be changed inside the hook
but it will be restored after that.")

(defface mls-norm-face
  '((((class color))
     :background "#b6bd68" :foreground "#1d1f21"))
  "Normal face used in primary mode-line."
  :group 'mode-line-stats)

(defface mls-warn-face
  '((((class color))
     :background "#de935f" :foreground "#1d1f21"))
  "Warning face used in primary mode-line."
  :group 'mode-line-stats)

(defface mls-crit-face
  '((((class color))
     :background "#cc6666" :foreground "#1d1f21"))
  "Critical face used in primary mode-line."
  :group 'mode-line-stats)

(defvar mls-no-data-string "?"
  "String to show while loading data.")

(defun mls-get-level (module fmt value)
  "Given a MODULE, FMT and the VALUE return the level name."
  (let ((levels (cadr (assoc fmt (mls-module-get module :levels))))
        (level nil)
        (level-value nil)
        (level-name nil)
        (found nil))

    (while (and levels value (not found))
      (setq level (car levels))
      (setq level-value (car level))
      (setq level-name (cadr level))
      (setq found (cond
                   ((and (numberp value) (numberp level-value))
                    (when (>= value level-value) level-name))
                   ((and (stringp value) (stringp level-value))
                    (when (string= value level-value) level-name))))
      (setq levels (cdr levels)))
    found))

(defun mls-get-face (module fmt value fmt-type)
  "Return the face of the current level.
Given a MODULE, FMT, VALUE and the FMT-TYPE
it will return the face of the current level."
  (let ((level (mls-get-level module fmt value)))
    (when level
      (if (eq fmt-type :mode-line-format)
          (format "mls-%s-face" level)
        (format "mls-buffer-%s-face" level)))))

(defun mls-normalize-value (value)
  "Convert the VALUE to integer if contain a number."
  (cond ((and (stringp value)
              (equal (number-to-string (string-to-number value))
                     value))
         (string-to-number value))
        ((and (stringp value)
              (floatp (string-to-number value)))
         (string-to-number (replace-regexp-in-string "0*$" "" value)))
        ((or (stringp value) (numberp value))
         value)))

(defun mls-pretty-percent-handler (value)
  "Prettify percent VALUE."
  (let* ((width mls-pretty-percent-width)
         (x (/ (* width value) 100)))
    (concat "["
            (make-string x ?|)
            (make-string (- width x) ?-)
            "] "
            (format "%d" value))))

(defun mls-pretty-float-handler (value)
  "Prettify float VALUE."
  (format "%.2f" value))

(defun mls-pretty-integer-handler (value)
  "Prettify integer VALUE."
  (format "%d" value))

(defun mls-pretty-string-handler (value)
  "Prettify string VALUE."
  (format "%s" value))

(defun mls-pretty-value (value face hide-value-p &optional comment)
  "Propertize the VALUE according to the current level.
FACE is the current level face.
HIDE-VALUE-P flag to hide the value when displaying.
COMMENT additional text to be propertized and displayed."
  (let ((text-to-show nil))

    (if value
        (progn
          (unless hide-value-p
            (setq text-to-show (cond ((floatp value)
                                      (funcall mls-pretty-float-func value))
                                     ((and (numberp value) (equal comment "%"))
                                      (funcall mls-pretty-percent-func value))
                                     ((numberp value)
                                      (funcall mls-pretty-integer-func value))
                                     ((stringp value)
                                      (funcall mls-pretty-string-func value)))))

          (setq text-to-show (concat text-to-show comment))

          (propertize text-to-show 'face face))
      mls-no-data-string)))

(defun mls-hidden-formatter-p (fmt)
  "Return t if formatter FMT is normalized, nil otherwise."
  (string-match-p "^&" fmt))

(defun mls-normalize-formatter (fmt)
  "Convert custom symbols of FMT."
  (replace-regexp-in-string "&" "%" fmt))

(defun mls-get-format-info (fmt-string &optional normalizep)
  "Given a FMT-STRING it parse the formatters.
if NORMALIZEP is nil it will use custom formatters.
Return a list with a list for each formatter
containg the formatter and the comment.

\(\(\"%p\" \"comment\"\)\)"
  (let ((result nil)
        (fmt nil)
        (comment nil)
        (index 0))

    (while (string-match mls-format-regexp fmt-string index)
      (setq index (match-end 0))
      (setq fmt (match-string 1 fmt-string))
      (setq comment (match-string 3 fmt-string))
      (when normalizep
          (setq fmt (mls-normalize-formatter fmt)))
      (setq result (cons (list fmt comment) result)))
    (reverse result)))

(defun mls-get-active-formatters (module-or-name &optional normalizep)
  "Given MODULE-OR-NAME return a list with all active formatters.
If NORMALIZEP is nil it will use custom formatters."
  (let ((result (mapcar #'(lambda (key)
                             (mls-module-get module-or-name key))
                         mls-format-keys)))
    (setq result (mapconcat 'identity result " "))
    (setq result (mls-get-format-info result normalizep))
    (mapcar #'(lambda (a) (car a)) result)))

(defun mls-get-current-monitor-level (module values)
  "Return the current level name of the hook formatter.
MODULE is a module plist.
VALUES is a list of data values."
  (let* ((output-fmt (mls-module-get module :monitor-format))
         (formatters (mls-get-active-formatters module t))
         (fmt-info (car (mls-get-format-info output-fmt)))
         (value nil)
         (fmt nil)
         (index nil))

    (when fmt-info
      (setq fmt (mls-normalize-formatter (car fmt-info)))
      (setq index (-elem-index fmt formatters))
      (setq value (mls-normalize-value (nth index values)))
      (when value
        (mls-get-level module fmt value)))))

(defun mls-process (module data fmt-type)
  "Process the module and return the string to be displayed in mode-line.
MODULE is a plist.
DATA is a list of values.
FMT-TYPE is the format type, usually :mode-line-format or :buffer-format."
  (let* ((output-fmt (mls-module-get module fmt-type))
         (formatters-info (mls-get-format-info output-fmt))
         (active-formatters (mls-get-active-formatters module t))
         (formatter-info nil)
         (fmt nil)
         (fmt-sane nil)
         (value nil)
         (comment nil)
         (regexp nil)
         (index nil))

    (dolist (formatter-info formatters-info)
      (setq fmt (car formatter-info))
      (setq comment (cadr formatter-info))
      (setq fmt-sane (mls-normalize-formatter fmt))
      (setq regexp (concat fmt (when comment
                                 (format "{%s}" comment))))
      (setq index (-elem-index fmt-sane active-formatters))
      (setq value (mls-normalize-value (nth index data)))
      (setq value (mls-pretty-value value
                                    (mls-get-face module fmt-sane value fmt-type)
                                    (mls-hidden-formatter-p fmt)
                                    comment))
      (setq output-fmt (replace-regexp-in-string regexp value output-fmt)))
    output-fmt))

(defun mls-run-hook (module values fmt-type)
  "Run the hook with parameters.
MODULE: module plist.
VALUES: a list of data values used to get the current level.
FMT-TYPE: format type."
  (run-hook-with-args 'mls-monitor-hook
                      (mls-module-get module :name)
                      (mls-get-current-monitor-level module values)
                      module
                      fmt-type))

;; TODO: check for required arguments (:start, :stop,...)
(defun mls-valid-module-p (module-name)
  "Return t if MODULE-NAME is a valid module, nil otherwise."
  (let ((module (mls-module-get module-name)))
    (when module
        (mls-module~valid-p module))))

(defun mls-enabled-module-p (module-name)
  "Return t if MODULE-NAME is enabled, nil otherwise."
  (let ((module-sym (intern module-name))
        (running-p (mls-module-get module-name :running)))
    (and running-p (member module-sym mls-modules))))

(defun mls-disable-module (module-name)
  "Disable the module MODULE-NAME."
  (when (mls-enabled-module-p module-name)
    (mls-module-stop module-name)
    (mls-module-set module-name :running nil)))

(defun mls-enable-module (module-name)
  "Enable the module with name MODULE-NAME."
  (let ((module-file nil)
        (module-format nil)
        (enabled-p (mls-enabled-module-p module-name)))

    (setq module-file (intern (format "mls-%s" module-name)))

    (when (and (require module-file) (mls-valid-module-p module-name) (not enabled-p))
      (message (format "Enabling module: %s" module-name))
      (setq module-format (mapconcat 'identity
                                     (mls-get-active-formatters module-name t)
                                     " "))
      (mls-module-set module-name :format module-format)
      (mls-module-start module-name)
      (mls-module-set module-name :running t))))

(defun mls-run-module (module-name fmt-type)
  "Run the module.
MODULE-NAME corresponds to module name.
FMT-TYPE is the format type \(:mode-line-format or :buffer-format\)."
  (when (mls-enabled-module-p module-name)
    (let* ((module-name (downcase module-name))
           (module (mls-module-get module-name))
           (module-backup (copy-tree module))
           (data (mls-module-get module-backup :data)))

      (mls-run-hook module-backup data fmt-type)
      (mls-process module-backup data fmt-type))))

(defun mls-refresh ()
  "Refresh mode-line-stats."
  (let ((modules (reverse mls-modules))
        (acc-string nil))
    (dolist (module-sym modules)
      (add-to-list 'acc-string
                   (mls-run-module (format "%s" module-sym)
                                   :mode-line-format)))
    (setq mls-mode-line-string acc-string)
    (force-mode-line-update)))

(defun mls-cleanup ()
  "Remove mls from mode-line."
  (let* ((target (cond ((eq mls-position :header-line)
                        'header-line-format)
                       ((eq mls-position :global-mode-string)
                        'global-mode-string)
                       (t 'mode-line-format)))
         (value (-remove '(lambda(x)
                            (equal x 'mls-mode-line-string))
                         (symbol-value target))))
    (set-default target value)
    (set target value)))

(defun mls-init (position)
  "Set the POSITION for mls."
  (let ((value nil))
    (mls-cleanup)
    (setq mls-position position)

    (cond ((eq position :left)
           ;; Workaround: mode-line won't show if first elem is not a string
           (setq value (cond ((stringp (-first-item mode-line-format))
                              mode-line-format)
                             (t (-insert-at 0 "" mode-line-format))))
           (setq value (-insert-at 1 'mls-mode-line-string value))
           (setq-default mode-line-format value)
           (setq mode-line-format value))
          ((eq position :right)
           (setq value (-snoc 'mls-mode-line-string mode-line-format))
           (setq-default mode-line-format value)
           (setq mode-line-format value))
          ((eq position :global-mode-string)
           (push " " global-mode-string)
           (push 'mls-mode-line-string global-mode-string))
          ((eq position :header-line)
           (setq value (-insert-at 0 'mls-mode-line-string
                                   header-line-format))
           (setq-default header-line-format value)
           (setq header-line-format value)))
    (mls-refresh)))

(defun mls-turn-on ()
  "Turn on mode-line-stats mode."
  (dolist (module-sym mls-modules)
    (mls-enable-module (symbol-name module-sym)))

  (unless mls-buffer
    (mls-buffer-init))

  (mls-init mls-position))

(defun mls-turn-off ()
  "Turn off mode-line-stats mode."
  (mls-cleanup)

  (dolist (module-sym mls-modules)
    (mls-disable-module (symbol-name module-sym))))

(define-minor-mode mode-line-stats-mode
  "Show stats in mode-line."
  :global t
  :group 'mode-line-stats
  :init-value t
  :lighter " Mode-Line-Stats"
  (progn
    (if mode-line-stats-mode
        (mls-turn-on)
      (mls-turn-off))))

(provide 'mode-line-stats)
;;; mode-line-stats.el ends here
