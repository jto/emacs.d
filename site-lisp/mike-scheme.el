;; * SCHEME

(require 'geiser)
(require 'scheme)

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

(defun mike-scheme-mode-hook ()
  (autopair-mode 0)
  (paredit-mode 1))

(add-hook 'geiser-repl-mode-hook 'mike-geiser-mode-hook)
(add-hook 'scheme-mode-hook 'mike-scheme-mode-hook)

(provide 'mike-scheme)
