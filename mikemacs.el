(defcustom mikemacs-packages '(
                               (mike-custom . mike-init-custom)
                               (mike-packages . mike-init-packages)
                               mike-buffer-utils
                               (mike-path-utils . mike-init-paths)
                               (mike-globals . mike-init-globals)
                               mike-man
                               mike-term
                               mike-org
                               mike-perl
                               mike-scala
                               mike-yaml
                               mike-graphviz
                               mike-sql
                               mike-elisp
                               mike-scheme
                               mike-tex
                               mike-js
                               mike-ssh
                               mike-eshell
                               mike-python
                               mike-haskell
                               mike-theme
                               mike-server
                               )
  "A list of mikemacs packages to load. Elements are a package string or symbol,
or a pair (package . init-function-name). Order is important."
  :type '(repeat (choice string
                         symbol
                         (cons (choice string symbol)
                               function)))
  :group 'mikemacs)

(defun mikemacs-init ()
  "Load mikemacs packages."
  (dolist (p mikemacs-packages)
    (let ((pkg           (if (consp p) (car p) p))
          (init-function (if (consp p) (cdr p) nil)))
      (require pkg)
      (when init-function (funcall init-function)))))

(provide 'mikemacs)
