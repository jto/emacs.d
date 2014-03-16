(defun mike-man-mode-hook ()
  (setq show-trailing-whitespace nil))

(defun mike-after-man-load ()
  (add-hook 'man-mode-hook 'mike-man-mode-hook))

(eval-after-load "man" '(mike-after-man-load))

(provide 'mike-man)
