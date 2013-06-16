;; Various utilities

(require 'cl)

(defun format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (let ((regex (concat "%\\("
                       (reduce (lambda (a b)
                                 (concat a "\\|" b))
                               (mapcar #'car formatters))
                       "\\)")))
    (replace-regexp-in-string regex
                              (lambda (str)
                                (let ((fun (assoc (substring str 1)
                                                  formatters)))
                                  (if fun
                                      (funcall (cdr fun) arg)
                                      (error "Unrecognized format sequence: %s" str))))
                              format t t)))

(defun mls-mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
            (apply 'mls-mapcar* f (mapcar 'cdr xs)))))

(provide 'misc-utils)
