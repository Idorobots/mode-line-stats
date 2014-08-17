;;; mls-buffer.el --- mode-line-stats buffer -*- coding: mule-utf-8 -*-

;; This file is not part of Emacs

;; Copyright (C) 2014 Andreu Gil Pàmies

;; Author: Andreu Gil Pàmies <agpchil@gmail.com>

;; Created: 16-07-2014

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

;;; Code:
(defvar mls-buffer-name "*mls*"
  "Buffer name.")

(defvar mls-buffer nil
  "Buffer object.")

(defface mls-buffer-norm-face
  '((((class color))
     :foreground "#b6bd68"))
  "Normal face used in buffer mode-line."
  :group 'mode-line-stats)

(defface mls-buffer-warn-face
  '((((class color))
     :foreground "#de935f"))
  "Warning face used in buffer mode-line."
  :group 'mode-line-stats)

(defface mls-buffer-crit-face
  '((((class color))
     :foreground "#cc6666"))
  "Critical face used in buffer mode-line."
  :group 'mode-line-stats)

(defun mls-buffer-init ()
  "Create mls buffer."
  (setq mls-buffer (get-buffer-create mls-buffer-name))
  (with-current-buffer mls-buffer
    (mls-buffer-mode)))

(defun mls-buffer-module-title (module-name)
  "Return the module title for MODULE-NAME."
  (let ((module-name (capitalize (format "%s" module-name))))
    (propertize (format "\n %s stats\n"
                        module-name)
                'face font-lock-type-face)))

(defun mls-buffer-refresh ()
  "Refresh mls buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (line nil)
        (column nil))
    (with-current-buffer mls-buffer
      (setq line (line-number-at-pos))
      (setq column (current-column))
      (erase-buffer)
      (insert (propertize "Mode line stats" 'face font-lock-preprocessor-face))
      (insert "\n")
      (dolist (module mls-modules)
        (insert (mls-buffer-module-title module))
        (insert (mls-display (format "%s" module) :buffer-format))
        (insert "\n"))
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char column))))

(defun mls-buffer-show ()
  "Show mls buffer."
  (interactive)
  (switch-to-buffer mls-buffer))

(defun mls-buffer-hide ()
  "Hide mls buffer."
  (interactive)
  (switch-to-buffer nil))

(defvar mls-buffer-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "u" 'mls-buffer-refresh)
    (define-key map "q" 'mls-buffer-hide)

    map)
  "Keymap for the *mls-buffer* buffer.")

(define-derived-mode mls-buffer-mode special-mode "mls:buffer"
  "Major mode for the mls buffer.
\\{mls-buffer-mode-map}."
  (use-local-map mls-buffer-mode-map)
   ;; don't record undo info
  (setq buffer-undo-list t)
  (read-only-mode))

(provide 'mls-buffer)
;;; mls-buffer ends here
