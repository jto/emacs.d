;; * PERL

;; modes
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile\\'" . cperl-mode))

;; CPerl customizations
(defun mike-cperl-mode-hook ()
  (setq cperl-indent-level                4
        cperl-close-paren-offset         -4
        cperl-continued-statement-offset  4
        cperl-indent-parens-as-block      t
        cperl-tab-always-indent           t
        cperl-merge-trailing-else         nil)
  (auto-fill-mode 0)
  (when (featurep 'outshine) (outline-minor-mode 1)))

(defun mike-after-cperl-load ()
  (add-hook 'cperl-mode-hook 'mike-cperl-mode-hook))

(eval-after-load "cperl-mode" '(mike-after-cperl-load))

(provide 'mike-perl)
