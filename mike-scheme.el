;; * SCHEME

;; Geiser customizations (Scheme Slime-like environment)
;; Disable read-only prompt in Geiser.
;; The read-only prompt doesn't play nicely with custom REPLs
;; such as in SICP.
(defun mike-geiser-turn-off-read-only-prompt ()
  "Turn off read-only prompt in `geiser'."
  (interactive)
  (setq geiser-repl-read-only-prompt-p nil))

(defun mike-geiser-mode-hook ()
  (autopair-mode 0)
  (paredit-mode 1))

(defun mike-after-geiser-load ()
  (add-hook 'geiser-repl-mode-hook 'mike-geiser-mode-hook))

(eval-after-load "geiser" '(mike-after-geiser-load))

(defun mike-init-scheme ()
  (try-require 'geiser-autoloads))

(add-hook 'after-init-hook 'mike-init-scheme)

(provide 'mike-scheme)
