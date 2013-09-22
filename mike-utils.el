;;; mike-utils.el --- Utility functions for mikemacs

;; ================
;; = STRING UTILS =
;; ================

;; Utils from
;; http://emacswiki.org/emacs/ElispCookbook
(defun mike-string-ends-with (str suffix)
  "Return t if STR ends with SUFFIX."
  (let ((elength (length suffix)))
    (string-equal suffix (substring str (- 0 elength)))))

(defun mike-string-starts-with (str prefix)
  "Return t if STR begins with PREFIX."
  (cond ((>= (length str) (length prefix))
         (string-equal prefix (substring str 0 (length prefix))))
        (t nil)))

;;; ==============
;;; = FILE UTILS =
;;; ==============

(defun mike-catfile (&rest comps)
  "Safely concatenate path COMPS to produce a directory or filename."
  (if (null comps)
      (file-name-as-directory ".")
    (let ((result (car comps)))
      (dolist (filename (cdr comps))
        (setq result (concat (file-name-as-directory result) filename)))
      (let ((dir (file-name-as-directory result)))
        (if (file-directory-p dir)
            dir
          result)))))

(provide 'mike-utils)
