;; * JAVASCRIPT

(defun mike-init-js ()
  (try-require 'js))

(add-hook 'after-init-hook 'mike-init-js)

(provide 'mike-js)
