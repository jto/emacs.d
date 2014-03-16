;; * TEX

(defun mike-init-tex ()
  (try-require 'auctex-autoloads)
  ;; Load auctex settings
  (when (featurep 'ns)
    ;; Settings work on OS X; work on making these xplaf
    (setq TeX-PDF-mode t
          TeX-view-program-list '(("Open" "open \"%o\""))
          TeX-view-program-selection '((output-pdf "Open")))))

(add-hook 'after-init-hook 'mike-init-tex)

(provide 'mike-tex)
