;; * GRAPHVIZ DOT

(defun mike-init-graphviz ()
  (try-require 'graphviz-dot-mode))

(add-hook 'after-init-hook 'mike-init-graphviz)

(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;; Graphviz customizations
(defun mike-after-graphviz-load ()
  (setq graphviz-dot-auto-indent-on-braces nil)
  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4))

(eval-after-load "graphviz-dot-mode" '(mike-after-graphviz-load))

(provide 'mike-graphviz)
