;; * PACKAGES

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.

(require 'cl)
(require 'package)

;; ** SETUP

(defcustom mike-exclude-packages '()
  "Packages to exclude from this Emacs instance."
  :type '(repeat symbol)
  :group 'mikemacs)

(defcustom mike-package-packages '()
  "A list of packages to ensure are installed in Emacs."
  :type '(repeat symbol)
  :group 'mikemacs)

;; Add package repositories.
(defun mike-init-package-repositories ()
  "Add marmalade and melpa to `package-archives'."
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun mike-init-package-list ()
  "Add mike packages to `mike-package-packages'.
Exclude packages by setting mike-exclude-packages in before-init.el"
  (dolist (p '(
               starter-kit
               starter-kit-lisp
               starter-kit-bindings
               starter-kit-eshell
               elisp-slime-nav
               diminish
               magit
               geiser
               graphviz-dot-mode
               autopair
               melpa
               exec-path-from-shell
               auctex
               yasnippet
               s
               ht
               dash
               yaml-mode
               color-theme-solarized
               virtualenv
               scala-mode2
               haskell-mode
               org
               outshine
               helm
               helm-ack
               helm-projectile
               flx-ido
               ido-vertical-mode
               bookmark+
               projectile
               dired-details
               smart-mode-line
               undo-tree
               auto-complete
               multiple-cursors
               multi-term
               smartparens
               ack-and-a-half
               ))
    (pushnew p mike-package-packages)))

(defun mike-init-packages ()
  (package-initialize)
  (mike-init-package-list)
  (mike-init-package-repositories)
  (when (not (package-installed-p 'melpa))
    (package-install 'melpa))
  (require 'melpa)
  (mike-install-packages))

(provide 'mike-packages)
