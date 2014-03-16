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
(require 'mls-module)

(defgroup mode-line-stats nil
  "Minor mode for mode-line-stats"
  :prefix "mode-line-stats"
  :group 'help
  :link '(url-link "http://github.com/agpchil/mode-line-stats"))

(defvar mls-toggle-key (kbd "C-º")
  "Keybinding to toggle primary/secondary mode-lines.")

(defvar mls-format-regexp
  "\\([\%\&][^ \{]+\\)\\(\{\\([^\}]+\\)?\}\\)?"
  "Regexp to parse formatters in the format strings.")

(defconst mls-modules-available '(battery cpu memory disk misc sensors)
  "Modules available to use.")

(defvar mls-modules '(cpu)
  "Modules enabled.")

(defvar mls-format-keys '(:mode-line :buffer :monitor))

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

(defface mls-norm-primary-face
  '((((class color))
     :background "#b6bd68" :foreground "#1d1f21"))
  "Normal face used in primary mode-line."
  :group 'mode-line-stats)

(defface mls-warn-primary-face
  '((((class color))
     :background "#de935f" :foreground "#1d1f21"))
  "Warning face used in primary mode-line."
  :group 'mode-line-stats)

(defface mls-crit-primary-face
  '((((class color))
     :background "#cc6666" :foreground "#1d1f21"))
  "Critical face used in primary mode-line."
  :group 'mode-line-stats)

(defface mls-norm-buffer-face
  '((((class color))
     :foreground "#b6bd68"))
  "Normal face used in buffer mode-line."
  :group 'mode-line-stats)

(defface mls-warn-buffer-face
  '((((class color))
     :foreground "#de935f"))
  "Warning face used in buffer mode-line."
  :group 'mode-line-stats)

(defface mls-crit-buffer-face
  '((((class color))
     :foreground "#cc6666"))
  "Critical face used in buffer mode-line."
  :group 'mode-line-stats)

(defvar mls-buffer-name "*mls*"
  "Buffer name.")

(defvar mls-buffer nil
  "Buffer object.")

(defvar mls-format-primary nil
  "Primary format to show stats.
\(this will be appended in the default mode-line or header-line\)")

(defvar mls-format nil
  "Contains the original mode line plus the primary one.")

(defvar mls-format-backup nil
  "Backup original format \(mode-line or header-line\).")

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
      (if (eq fmt-type :mode-line)
          (format "mls-%s-primary-face" level)
        (format "mls-%s-buffer-face" level)))))

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

(defun mls-find-formatter-position (formatter list)
  "Find the position of the FORMATTER in the LIST."
  (let* ((total (length list))
         (items (member formatter list))
         (remain (length items)))
    (when items
      (- total remain))))

(defun mls-get-current-monitor-level (module values)
  "Return the current level name of the hook formatter.
MODULE is a module plist.
VALUES is a list of data values."
  (let* ((output-fmt (mls-module-get module :monitor))
         (formatters (mls-get-active-formatters module t))
         (fmt-info (car (mls-get-format-info output-fmt)))
         (value nil)
         (fmt nil)
         (index nil))

    (when fmt-info
      (setq fmt (mls-normalize-formatter (car fmt-info)))
      (setq index (mls-find-formatter-position fmt formatters))
      (setq value (mls-normalize-value (nth index values)))
      (when value
        (mls-get-level module fmt value)))))

(defun mls-process (module data fmt-type)
  "Process the module and return the string to be displayed in mode-line.
MODULE is a plist.
DATA is a list of values.
FMT-TYPE is the format type, usually :mode-line or :buffer."
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
      (setq index (mls-find-formatter-position fmt-sane active-formatters))
      (setq value (mls-normalize-value (nth index data)))
      (setq value (mls-pretty-value value
                                    (mls-get-face module fmt-sane value fmt-type)
                                    (mls-hidden-formatter-p fmt)
                                    comment))
      (setq output-fmt (replace-regexp-in-string regexp value output-fmt)))
    output-fmt))

(defun mls-run-hook (module fmt-type values)
  "Run the hook with parameters.
MODULE: module plist
FMT-TYPE: format type
VALUES: a list of data values used to get the current level"
  (run-hook-with-args 'mls-monitor-hook
                      (mls-module-get module :name)
                      (mls-get-current-monitor-level module values)
                      module
                      fmt-type))

;; TODO: check for required arguments (:start, :stop,...)
(defun mls-module-valid-p (module-name)
  "Return t if MODULE-NAME is a valid module, nil otherwise."
  (let ((module-sym (if (stringp module-name)
                        (intern module-name)
                      module-name)))
    (member module-sym mls-modules-available)))

(defun mls-module-enabled-p (module-name)
  "Return t if MODULE-NAME is enabled, nil otherwise."
  (let ((module-sym (intern module-name))
        (running-p (mls-module-get module-name :running)))
    (and running-p (member module-sym mls-modules))))

(defun mls-disable-module (module-name)
  "Disable the module MODULE-NAME."
  (when (mls-module-enabled-p module-name)
    (mls-module-call module-name :stop)
    (mls-module-set module-name :running nil)))

(defun mls-enable-module (module-name)
  "Enable the module with name MODULE-NAME."
  (let ((module-file nil)
        (module-format nil)
        (enabled-p (mls-module-enabled-p module-name)))

    (when (and (mls-module-valid-p module-name) (not enabled-p))
      (setq module-file (intern (format "mls-%s" module-name)))
      (require module-file)
      (message (format "Enabling module: %s" module-name))
      (setq module-format (mapconcat 'identity
                                     (mls-get-active-formatters module-name t)
                                     " "))
      (mls-module-set module-name :format module-format)
      (mls-module-call module-name :start)
      (mls-module-set module-name :running t))))

(defun mls-display (module-name fmt-type)
  "Display the module in the mode-line.
MODULE-NAME corresponds to module name.
FMT-TYPE is the format type \(:mode-line or :buffer\)."
  (when (mls-module-enabled-p module-name)
    (let* ((module-name (downcase module-name))
           (module (mls-module-get module-name))
           (module-backup (copy-tree module))
           (data (mls-module-get module-backup :data)))

      (mls-run-hook module-backup fmt-type data)
      (mls-process module-backup data fmt-type))))

(defun mls-get-position ()
  "Get the format symbol acording to mls-position.
If `mls-position is :left or :right it will return
'mode-line-format.  For :global-mode-string it will
return 'global-mode-string and for :header-line it
will return 'header-line-format."
  (cond ((eq mls-position :header-line)
         'header-line-format)
        ((eq mls-position :global-mode-string)
         'global-mode-string)
        (t 'mode-line-format)))

(defun mls-backup-format ()
  "Backup the current 'mode-line-format'."
  (let ((fmt (symbol-value (mls-get-position))))
    (setq mls-format-backup fmt)))

(defun mls-restore-format ()
  "Restore the backup of 'mode-line-format'."
  (let ((target (mls-get-position)))
    (set-default target mls-format-backup)
    (set target mls-format-backup)
    (setq mls-format-backup nil)
    (force-mode-line-update)))

(defun mls-set-position (position)
  "Set the POSITION for mls."
  (cond ((eq position :left)
         (setq mls-format
               (cons mls-format-primary
                     mls-format-backup)))
        ((eq position :right)
         (setq mls-format (copy-tree mls-format-backup))
         (setf (cdr (last mls-format)) mls-format-primary))
        ((eq position :global-mode-string)
         (push " " global-mode-string)
         (push mls-format-primary global-mode-string)
         (setq mls-format mode-line-format))
        ((eq position :header-line)
         (setq mls-format
               (cons mls-format-primary
                     mls-format-backup)))))

(defun mls-buffer-init ()
  "Create mls buffer."
  (setq mls-buffer (get-buffer-create mls-buffer-name)))

(defun mls-buffer-module-title (module-name)
  "Return the module title for MODULE-NAME."
  (let ((module-name (capitalize (format "%s" module-name))))
    (propertize (format "\n %s stats\n"
                        module-name)
                'face font-lock-type-face)))

(defun mls-buffer-update ()
  "Update mls buffer."
  (interactive)
  (with-current-buffer mls-buffer
    (erase-buffer)
    (insert (propertize "Mode line stats" 'face font-lock-preprocessor-face))
    (insert "\n")
    (dolist (module mls-modules)
      (insert (mls-buffer-module-title module))
      (insert (mls-display (format "%s" module) :buffer))
      (insert "\n"))))

(defun mls-mode-line-setup ()
  "Add mode-line-stats format into currrent mode-line."
  (unless mls-format-primary
    (mls-generate-mode-line-format))

  (unless mls-buffer
    (mls-buffer-init))

  (mls-set-position mls-position))

(defun mls-generate-mode-line-format ()
  "Generate the mode line format using `mls-modules`."
  (let ((mode-line-format-sym 'mls-format-primary)
        (modules (reverse mls-modules)))
    (dolist (module-sym modules)
      (push `(:eval (mls-display ,(symbol-name module-sym) :mode-line))
            (symbol-value mode-line-format-sym)))))

(defun mls-enable-mode-line ()
  "Enable mode-line format."
  (let ((target (if (eq mls-position :header-line)
                    'header-line-format
                  'mode-line-format)))
    (set-default target mls-format)
    (set target mls-format)))

(defun mls-turn-on ()
  "Turn on mode-line-stats mode."
  ;; Backup mode line
  (unless mls-format-backup
    (mls-backup-format))

  (dolist (module-sym mls-modules)
    (mls-enable-module (symbol-name module-sym)))

  (mls-mode-line-setup)

  (mls-enable-mode-line)

  (mls-keymap-setup))

(defun mls-turn-off ()
  "Turn off mode-line-stats mode."
  (mls-restore-format)

  (dolist (module-sym mls-modules)
    (mls-disable-module (symbol-name module-sym))))

(defvar mode-line-stats-mode-map (make-keymap)
  "Keymap for mode-line-stats mode.")

(defun mls-keymap-setup ()
  "Init the keymap."
;; key bindings
  (define-key mode-line-stats-mode-map mls-toggle-key 'mls-mode-line-toggle))

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
