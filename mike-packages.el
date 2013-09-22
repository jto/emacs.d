;;; mike-packages.el --- Opinionated, customizable package list for mikemacs

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.

;;; ===========
;;; = REQUIRE =
;;; ===========

(require 'cl)
(require 'mike-paths)
(require 'package)

;;; =========
;;; = SETUP =
;;; =========

;; Add package repositories.
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize package.
(package-initialize)

;; Create a list of packages to install if not present.
(when (not package-archive-contents)
  (package-refresh-contents))

;;; ========================
;;; = VARIABLE DEFINITIONS =
;;; ========================

(defvar mike-required-packages '()
  "A list of packages to ensure are installed at launch.")
(dolist (p '(starter-kit
             starter-kit-lisp
             starter-kit-bindings
             starter-kit-eshell
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
             haskell-mode))
  (pushnew p mike-required-packages))

(defvar mike-exclude-packages '()
  "Packages to exclude from this Emacs instance.

Set this list in before-init.el to exclude unwanted packages.")

;;; =============
;;; = FUNCTIONS =
;;; =============

(defun mike-package-to-install-p (pkg)
  "True if P should be installed and is not."
  (not (or (package-installed-p pkg)
           (member pkg mike-exclude-packages))))

(defun mike-check-packages ()
  "Check for uninstalled packages."
  (interactive)
  (let ((uninstalled-packages (cl-remove-if-not 'mike-package-to-install-p
                                                mike-required-packages)))
    (message
     (if (null uninstalled-packages)
         "All packages installed."
       "Uninstalled packages. Run `mike-install-packages' to install."))))

(defun mike-install-packages ()
  "Install uninstalled packages in `mike-required-packages'."
  (interactive)
  (dolist (pkg mike-required-packages)
    (when (mike-package-to-install-p pkg)
      (condition-case-unless-debug err
          (package-install pkg)
        (error (message "%s" (error-message-string err)))))))

;;; ===========
;;; = PROVIDE =
;;; ===========

(provide 'mike-packages)
