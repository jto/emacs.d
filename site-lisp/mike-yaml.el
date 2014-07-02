;; * YAML

(add-to-list 'auto-mode-alist '("\\.cfg\\'" . yaml-mode))

(defun mike-init-yaml ()
  (try-require 'yaml-mode))

(add-hook 'after-init-hook 'mike-init-yaml)

(provide 'mike-yaml)
