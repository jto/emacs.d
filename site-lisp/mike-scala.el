;; * SCALA

(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

(defun mike-after-scala-load ()
  (when (try-require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

(eval-after-load "scala-mode2" '(mike-after-scala-load))

(defun mike-init-scala ()
  (try-require 'scala-mode2-autoloads))

(add-hook 'after-init-hook 'mike-init-scala)

(provide 'mike-scala)
