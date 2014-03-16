;; * ESHELL

;; eshell customizations

(defun mike-get-eshell ()
  (interactive)
  (mike-get-mode-buffer 'eshell-mode
                        (lambda () (eshell t))))

(defun mike-perldoc (man-args)
  "Perldoc to be called from eshell."
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man man-args)))

(defun eshell/perldoc (&rest args)
  "Like `eshell/man', but invoke `perldoc'."
  (if (member (car args) '("-l" "-h"))
      (eshell-wait-for-process
       (eshell-external-command "perldoc" args))
    (funcall 'mike-perldoc (apply 'eshell-flatten-and-stringify args))))

(defun mike-eshell-mode-hook ()
  (setq show-trailing-whitespace nil))

(defun mike-init-eshell ()
  (try-require 'eshell)
  (try-require 'em-hist)

  ;; custom be damned, these are opinionated
  (setq eshell-history-size 1024
        eshell-prompt-regexp "^[^#$]*[#$] "
        eshell-save-history-on-exit t
        eshell-highlight-prompt nil
        eshell-git/prompt-face 'default
        eshell-git/pwd-face 'default
        eshell-git/branch-face 'default)

  (global-set-key (kbd "C-c e") 'mike-get-eshell)

  (when (try-require 'eshell-git)
    (setq eshell-prompt-function 'eshell-git/prompt-function))

  (add-hook 'eshell-mode-hook 'mike-eshell-mode-hook))

(add-hook 'after-init-hook 'mike-init-eshell)

(provide 'mike-eshell)
