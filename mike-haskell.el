;; * HASKELL

(defun mike-init-haskell ()
  (unless (try-require 'haskell-mode-autoloads)
    (try-require 'haskell-mode))
  (eval-after-load "haskell-mode"
    '(progn
       (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))

(add-hook 'after-init-hook 'mike-init-haskell)

(provide 'mike-haskell)
