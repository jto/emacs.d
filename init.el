;;; #+TITLE: Mike Prentice's Emacs 24 Configuration

;;; Mikemacs: Opinionated defaults for Mike's Emacs happiness.

;;; My attempt at a literate Emacs configuration. Structure and
;;; contents borrowed liberally from URL
;;; `http://www.aaronbedra.com/emacs.d/'. Also draws inspiration from
;;; Emacs Prelude and Emacs Starter Kit.

;;; init.el

;;; * Startup

;;; Keep track of Emacs start time in order to calculate how long it
;;; takes to load. I use this both out of curiosity and to see if a
;;; new configuration significantly increases/decreases my startup
;;; time.

(defvar mike-emacs-load-start-time (current-time)
  "The time at which Emacs was started.

See URL `http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html'.")

;;; before-init.el, if it exists, will run before the configurations
;;; listed here. Use it to override defaults and set machine or
;;; environment specific parameters before libraries are loaded. For
;;; example, `user-mail-address'.

(let ((before-file (expand-file-name "before-init.el"
                                     user-emacs-directory)))
  (when (file-exists-p before-file)
    (load-file before-file)))

;;; Starting in Emacs 24.3, the official documentation recommends we
;;; require cl-lib instead of cl.

;;; Package cl-lib gives us access to nifty macros and functions from
;;; Common Lisp. My favorite is `cl-pushnew'.

(require 'cl-lib)

;;; * User Details

(setq user-full-name "Mike Prentice")

;;; * Environment

;;; ** site-lisp plugins

(defvar mike-plugins-dir
  (expand-file-name "site-lisp" user-emacs-directory)
  "Directory that contains manually installed code and packages.")

;;; Add site-lisp to the load path to pick up libraries I install
;;; manually, outside of package management tools.

(cl-pushnew mike-plugins-dir load-path)

;;; Update `load-path' with plugins subdirectories from site-lisp.
;;; Make sure it's a proper directory and not the special directories
;;; "." and ".."

(dolist (dirname (directory-files mike-plugins-dir t "\\w+"))
  (when (file-directory-p dirname)
    (cl-pushnew dirname load-path)))

;;; ** PATH and exec-path

;;; Add common paths to `exec-path' and PATH environment variable for
;;; executing files in /usr/local/bin, ~/bin, and ~/local/bin.

(defvar mike-path-paths
  (cl-map 'list 'expand-file-name '("/usr/local/bin" "~/bin" "~/local/bin"))
  "Paths to safely add to $PATH environment variable and `exec-path'.

Useful for graphical Emacs that may not pick up $PATH from user's
bashrc. Set in before-init.el to override.")

(let ((path-list (cl-map 'list
                         'expand-file-name
			 mike-path-paths)))
  ;; add to `exec-path'
  (cl-dolist (path path-list)
    (cl-pushnew path exec-path))
  ;; add to PATH
  (setenv "PATH"
          (let ((env-path-as-list (split-string (getenv "PATH")
						path-separator)))
            (mapconcat 'identity
                       (cl-dolist (path path-list env-path-as-list)
                         (cl-pushnew path env-path-as-list))
                       path-separator))))

;;; * Package Management

(require 'package)

;;; Don't activate packages at startup. We rely on use-package to
;;; activate.
(setq package-enable-at-startup nil)

;;; ** Package repositories

;;; We add marmalade and melpa repositories to supplement ELPA.

(defvar mike-package-archives
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.org/packages/"))
  "Repos to add to `package-archives'.  Set in before-init.el to
override.")

(dolist (repo mike-package-archives)
  (add-to-list 'package-archives repo))

;;; ** Bootstrap use-package
;;; From URL
;;; `http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html'

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; * exec-path-from-shell

;;; When we start in a graphical environment, such as on a Mac, we
;;; want to pull some environment settings from the shell.

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(ns mac))
	    (exec-path-from-shell-initialize)))

;;; * Start-up options

;;; ** Turn off the splash screen

(setq inhibit-splash-screen t)

;;; ** Turn off the toolbar

(tool-bar-mode -1)

;;; ** Selection

;;; Typing when the mark is active will write over the marked region.
;;; This is the current expected behavior and most like other text
;;; editors.

(delete-selection-mode t)

;;; Highlight region when mark is active.

(transient-mark-mode t)

;;; Cut and paste using the system clipboard as well as Emacs'
;;; internal clipboard.

(setq x-select-enable-clipboard t)

;;; ** Display settings

(when window-system
  ;; Set frame title to buffer title.
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; Try to use inconsolata 14 by default
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal)
  ;; Fallback to DejaVu Sans Mono for characters not supported by
  ;; Inconsolata
  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

;;; Use the menu.

(when window-system (menu-bar-mode 1))

;;; Put empty line markers on the left side when the file ends.

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;; ** Indentation

;;; Spaces forever.

(setq tab-width 2
      indent-tabs-mode nil)

;;; ** Smart indentation with electric-indent-mode

(use-package electric
  :config (electric-indent-mode 1))

;;; ** Show column numbers

(setq column-number-mode t)

;;; ** Line numbers

(use-package linum
  :config (global-linum-mode 1))

;;; ** Show trailing whitespace

(setq show-trailing-whitespace t)

;;; ** Yes and no

;;; Make yes and no answers one character.

(defalias 'yes-or-no-p 'y-or-n-p)

;;; ** Global keybindings

;;; Sane return behavior for programming.
(global-set-key (kbd "RET") 'newline-and-indent)

;;; Quicker than C-x o
(global-set-key (kbd "M-o") 'other-window)

;;; Perform incremental search with regex.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;;; ** Echo keystrokes so we don't wait around

(setq echo-keystrokes 0.1)

;;; ** No dialog boxes

(setq use-dialog-box nil)

;;; ** Visual bell

(setq visible-bell t)

;;; ** Highlight parentheses

(use-package paren
  :config (show-paren-mode t))

;;; * Backup files

(use-package mike-backup
  :load-path "mikemacs/"
  :init (add-hook 'before-save-hook #'mike-force-backup-of-buffer)
  :config (mike-backup-init))

;;; * Utility libraries

;;; These are external libraries and packages to enhance Emacs. I add
;;; my own utility functions in a later section.

;;; ** Anzu mode

;;; Anzu mode displays the number of matching searches when doing an
;;; incremental search.  Many other editors do this and it's amazing
;;; how useful it is.

(use-package anzu
  :ensure t
  :diminish anzu-mode
  ;; Almost always `query-replace-regexp' is more useful than
  ;; `query-replace'.  Bind `anzu-query-replace-regexp' to M-% to use
  ;; the anzu version.
  :bind ("M-%" . anzu-query-replace-regexp)
  :config (progn
	    (global-anzu-mode 1)))

;;; ** Ace jump mode

(use-package ace-jump-mode
  :ensure t
  :bind ("C-0" . ace-jump-mode))

;;; ** Magit

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;;; ** undo-tree

;;; More powerful undo and redo with undo-tree. Don't show minor mode
;;; in mode line.

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (progn
	    (global-undo-tree-mode 1)))

;;; ** paredit

;;; Rebind paredit barf and slurp keys to what I find more natural.

(use-package paredit
  :ensure t
  :bind (("M-]" . paredit-forward-slurp-sexp)
	 ("M-[" . paredit-backward-slurp-sexp)
	 ("M-}" . paredit-forward-barf-sexp)
	 ("M-{" . paredit-backward-barf-sexp))
  :init (progn (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
	       (add-hook 'ielm-mode-hook #'enable-paredit-mode)
	       (add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)
	       (add-hook 'scheme-mode-hook #'enable-paredit-mode)))

;;; ** Recent files

;;; Keep track of recent files.  Very handy.

(use-package recentf
  :ensure t
  :config (recentf-mode))

;;; ** Autopair

;;; We want autopair in most modes, so we enable it globally and will
;;; disable it where we don't want it. Don't show minor mode in mode
;;; line.

(defun mike-turn-off-autopair-mode ()
  (when (fboundp #'autopair-mode) (autopair-mode 0)))

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :config (progn (add-hook 'paredit-mode-hook #'mike-turn-off-autopair-mode)
		 (autopair-global-mode 1)))

;;; ** Hippie expand

;;; Expands current word.  A good quick choice for expansions when
;;; auto-complete isn't needed.  Bind to M-/.

(global-set-key (kbd "M-/") 'hippie-expand)

;;; Don't include punctuation at the end of expansions.
(setq hippie-expand-dabbrev-as-symbol nil)

;;; ** Autocomplete

;;; We want smart auto-completion.

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config (progn
	    (require 'auto-complete-config)
	    (ac-config-default)))

;;; ** Helm

;;; Helm is a navigation utility I'm trying out.  From URL
;;; `http://tuhdo.github.io/helm-intro.html'.

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files))
  :init
  (progn
    (require 'helm-config)
    (setq
     ;; Open helm buffer inside current window, not occupy whole other window.
     helm-split-window-in-side-p t
     ;; Move to end or beginning of source when reaching top or bottom of source.
     helm-move-to-line-cycle-in-source t
     ;; Search for library in `require' and `declare-function' sexp.
     helm-ff-search-library-in-sexp t
     ;; Scroll 8 lines in other window using M-<next>/M-<prior>
     helm-scroll-amount 8
     ;; Use recent files.
     helm-ff-file-name-history-use-recentf t
     ;; enable fuzzy matching
     helm-M-x-fuzzy-match t
     helm-buffers-fuzzy-matching t
     helm-recentf-fuzzy-match t))
  :config
  (progn
    ;; If we have access to curl, use it for Google suggestions.
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    ;; Rebind tab to run persistent action.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; Make tab work in terminal.
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; List actions using C-z
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    ;; Start helm-mode globally
    (helm-mode 1)))

;;; Suggest auto-complete candidates with helm instead of a popup with
;;; C-:.

(use-package ac-helm
  :ensure t
  :bind (("C-;" . ac-complete-with-helm)))

;;; ** Projectile

;;; Projectile brings fast and useful project management to Emacs.
;;; Don't show minor mode in mode line.

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode 1)))

;;; Use helm projectile.

(use-package helm-projectile
  :ensure t
  :init
  (setq
   projectile-completion-system 'helm))

;;; * Language and interpreter hooks

;;; ** Elisp

(use-package eldoc
  :diminish eldoc-mode
  :init (progn (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
	       (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)))

(use-package elisp-slime-nav
  :ensure t
  :init (progn (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
	       (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode)))

;;; ** Scheme

;;; Geiser customizations (Scheme Slime-like environment)

(defun mike-toggle-geiser-repl-read-only-prompt ()
  "Toggle read-only prompt in `geiser'."
  (interactive)
  (when (boundp 'geiser-repl-read-only-prompt-p)
    (setq geiser-repl-read-only-prompt-p (not geiser-repl-read-only-prompt-p))))

(use-package geiser
  :ensure t)

;;; Enable auto-complete for geiser.

(use-package ac-geiser
  :ensure t
  :init (progn (add-hook 'geiser-mode-hook #'ac-geiser-setup)
	       (add-hook 'geiser-repl-mode-hook #'ac-geiser-setup)
	       (add-to-list 'ac-modes 'geiser-repl-mode-hook)))

;;; * Text mode settings

(defun mike-fill-column-hook ()
  "Set the fill column to Mike's preferred width. Add this
function to a mode's hook to get my width."
  (setq fill-column 79))

(add-hook 'text-mode-hook 'mike-fill-column-hook)

;;; * My utilities

;;; ** Buffer utilities

(defun mike-swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun mike-clean-buffer ()
  "Untabify and strip trailing whitespace in current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))))

(defun mike-rename-buffer-file ()
  "Renames current buffer and file it is visiting.

From URL `http://whattheemacsd.com/'."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun mike-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
  logical line.  This is useful, e.g., for use with
  `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;;; * Theme

;;; Load solarized-light in a graphical environment. Load the wombat
;;; them in a terminal.

(if window-system
    (progn
      ;; Set some options that work better with solarized.
      (setq
       ;; Looks better in Xorg.
       x-underline-at-descent-line t
       ;; Make the fringe stand out from the background.
       solarized-distinct-fringe-background t
       ;; Make the modeline high contrast.
       solarized-high-contrast-mode-line t
       ;; Use less bolding.
       solarized-use-less-bold t
       ;; Don't change size of org-mode headlines (but keep other size changes).
       solarized-scale-org-headlines nil)
      ;; Load solarized.
      (load-theme 'solarized-light t))
  ;; Load wombat for terminal use.
  (load-theme 'wombat t))

;;; * Customize

(defvar mike-custom-file
  (expand-file-name "emacs-custom.el" user-emacs-directory)
  "Load customizations from this file. Set in before-init.el to override.")

;;; Load customizations from emacs-custom.el except on Aquamacs.

(when (not (featurep 'aquamacs))
  (setq custom-file mike-custom-file)
  (load custom-file 'noerror))

;;; * Finish

;;; after-init.el, if it exists, will run after the configurations
;;; listed here. Use it to override defaults and set machine or
;;; environment specific parameters after libraries are loaded.

(let ((after-file (expand-file-name "after-init.el" user-emacs-directory)))
  (when (file-exists-p after-file)
    (load-file after-file)))

;;; Time Emacs startup complete. Calculate and display the number of
;;; seconds it took to start up.

(message "Emacs startup in %ds"
         (let ((time (current-time)))
           (let ((current-hi (cl-first time))
                 (current-lo (cl-second time))
                 (startup-hi (cl-first mike-emacs-load-start-time))
                 (startup-lo (cl-second mike-emacs-load-start-time)))
             (- (+ current-hi current-lo) (+ startup-hi startup-lo)))))
