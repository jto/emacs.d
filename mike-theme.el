;; * THEME

(defcustom mike-theme 'solarized-light
  "Theme to load on startup."
  :type 'symbol
  :group 'mikemacs)

(defun mike-load-theme ()
  (when (not (custom-theme-enabled-p mike-theme))
    (load-theme mike-theme t)))

(add-hook 'after-init-hook 'mike-load-theme)

(provide 'mike-theme)
