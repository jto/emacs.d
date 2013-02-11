(require 'cl)

;; Time Emacs startup.
;; From http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time))

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Create a list of packages to install if not present.
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar *my-packages* '()
  "A list of packages to ensure are installed at launch.")
(add-to-list '*my-packages* 'starter-kit)
(add-to-list '*my-packages* 'starter-kit-lisp)
(add-to-list '*my-packages* 'starter-kit-bindings)
(add-to-list '*my-packages* 'starter-kit-eshell)
(add-to-list '*my-packages* 'magit)
(add-to-list '*my-packages* 'geiser)
(add-to-list '*my-packages* 'graphviz-dot-mode)
(add-to-list '*my-packages* 'autopair)
(dolist (p *my-packages*)
  (when (not (package-installed-p p))
    (package-install p)))

;; I like the menu.
(when window-system
  (menu-bar-mode))

;; Geiser customizations (Scheme Slime-like environment)
;; Disable read-only prompt in Geiser.
;; The read-only prompt doesn't play nicely with custom REPLs
;; such as in SICP.
(setq geiser-repl-read-only-promp-p nil)
(eval-after-load "geiser"
  (progn
    (add-hook 'geiser-repl-mode-hook 'paredit-mode)
    ))

;; Graphviz customizations
(setq graphviz-dot-auto-indent-on-braces nil)
(setq graphviz-dot-auto-indent-on-semi nil)
(setq graphviz-dot-indent-width 4)

;; Org-mode and mobile-org customizations
;; set org-mobile-encryption-password in custom
(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (expand-file-name "org" dropbox-dir))
(setq org-agenda-file (expand-file-name "agendafiles.txt" org-directory))
(setq org-mobile-directory (expand-file-name "MobileOrg" dropbox-dir))
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))

;; Enable column number mode everywhere.
(setq column-number-mode t)

;; Separate custom file.
(when (not (featurep 'aquamacs))
  (setq custom-file "~/.emacs.d/emacs-custom.el")
  (load custom-file 'noerror))

;; Time Emacs startup complete.
(message "Emacs startup in %ds"
         (let ((time (current-time)))
           (let ((hi (first time))
                 (lo (second time)))
             (- (+ hi lo)
                (+ (first *emacs-load-start*)
                   (second *emacs-load-start*))))))
