;; * PACKAGES

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.

(require 'cl)
(require 'package)

;; ** SETUP

(defcustom mike-exclude-packages '()
  "Packages to exclude from this Emacs instance."
  :type '(set symbol)
  :group 'mikemacs)

(defcustom mike-package-packages '()
  "A list of packages to ensure are installed in Emacs."
  :type '(list symbol)
  :group 'mikemacs)

(defvar mike-missing-packages '()
  "A list of missing packages set by `try-require'.")

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

;; ** FUNCTIONS

(defun try-require (feature)
  "Attempt to load a library or module named FEATURE.

Return true if the library given as argument is successfully
loaded. If not, instead of an error, just add the package to a
list of missing packages.

From URL `http://www.mygooglest.com/fni/dot-emacs.html'."
  (condition-case err
      ;; protected form
      (progn
        (if (stringp feature)
            (load-library feature)
          (require feature))
        feature)
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'mike-missing-packages feature 'append))
     nil)))

(defun mike-package-to-install-p (pkg)
  "True if P should be installed and is not."
  (not (or (package-installed-p pkg)
           (member pkg mike-exclude-packages))))

(defun mike-all-packages-installed-p ()
  "Return t if all packages to install are installed, nil otherwise."
  (loop for pkg in mike-package-packages
        when (mike-package-to-install-p pkg) do (return nil)
        finally (return t)))

(defun mike-check-packages ()
  "Check for uninstalled packages."
  (interactive)
  (message
   (if (mike-all-packages-installed-p)
       "All packages installed."
     "Uninstalled packages. Run `mike-install-packages' to install.")))

(defun mike-install-packages ()
  "Install uninstalled packages in `mike-package-packages'.
Won't install packages in `mike-exclude-packages'."
  (interactive)
  (package-refresh-contents)
  (mike-init-package-list)
  (dolist (pkg mike-package-packages)
    (when (mike-package-to-install-p pkg)
      (condition-case-unless-debug err
          (package-install pkg)
        (error (message "%s" (error-message-string err)))))))

(defun mike-init-packages ()
  (package-initialize)
  (mike-init-package-list)
  (mike-init-package-repositories)
  (when (not (package-installed-p 'melpa))
    (package-install 'melpa))
  (require 'melpa)
  (mike-install-packages))

(provide 'mike-packages)
