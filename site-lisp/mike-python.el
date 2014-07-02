;; * PYTHON

(defun mike-init-python ()
  (when (try-require 'python)
    (add-hook 'python-mode-hook 'autopair-mode)
    (try-require 'virtualenv)))

(add-hook 'after-init-hook 'mike-init-python)

(provide 'mike-python)
