;; * ELISP

(defun mike-after-elisp-load ())

(defun mike-init-elisp ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
  (eval-after-load "elisp-slime-nav"
    '(progn (mike-after-elisp-load)
            (diminish 'elisp-slime-nav-mode))))

(add-hook 'after-init-hook 'mike-init-elisp)

;; ** IELM

(defun mike-ielm-mode-hook ()
  (autopair-mode 0)
  (paredit-mode 1))

(defun mike-init-ielm ()
  ;; Add paredit-mode to IELM
  (when (and (try-require 'ielm)
             (try-require 'paredit))


    (add-hook 'ielm-mode-hook 'mike-ielm-mode-hook)))

(add-hook 'after-init-hook 'mike-init-ielm)

(provide 'mike-elisp)
