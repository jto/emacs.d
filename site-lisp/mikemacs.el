(require 'custom)
(require 'package)

;; * CUSTOM DEFINITIONS

(defgroup mikemacs nil
  "Mikemacs: Opinionated defaults for Mike's Emacs happiness.
Customize these options as desired.")

(defcustom mike-package-repositories
  '(("marmalade" . "http://marmalade-repo.org/packages/"))
  "A list of repositories to add to `package-archives'."
  :type '(repeat (cons string string)))

(defcustom mike-packages
  '(starter-kit
    yasnippet
    magit
    autopair
    autocomplete)
  "A list of packages to ensure are installed in Emacs."
  :type '(repeat symbol))

(defcustom mike-path-paths
  '("/usr/local/bin")
  "Paths to safely add to $PATH environment variable.

Useful for graphical Emacs that may not pick up $PATH from user's bashrc."
  :type '(set string)
  :group 'mikemacs)

(defcustom mike-exec-paths
  (map 'list 'expand-file-name mike-path-paths)
  "Paths to add to Emacs' `exec-path'. Defaults to `mike-path-paths'."
  :type '(set string)
  :group 'mikemacs)

(defcustom mike-plugins-dir
  (file-name-as-directory (expand-file-name "site-lisp" user-emacs-directory))
  "Plugins directory. Defaults to `user-emacs-directory'/plugins/."
  :type 'string
  :group 'mikemacs)

(defcustom mike-backup-dir
  (expand-file-name "backups" user-emacs-directory)
  "Directory for Emacs backups. Default is `user-emacs-directory'/backups."
  :type 'string
  :group 'mikemacs)


;; * FUNCTIONS

(defvar mike-missing-packages '()
  "A list of missing packages set by `try-require'.")

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
  (loop for pkg in mike-packages
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
  "Install uninstalled packages in `mike-packages'."
  (interactive)
  (dolist (repo mike-package-repositories)
    (add-to-list 'package-archives repo t))
  (package-refresh-contents)
  (dolist (pkg mike-packages)
    (when (mike-package-to-install-p pkg)
      (condition-case-unless-debug err
          (package-install pkg)
        (error (message "%s" (error-message-string err)))))))

;; Separate custom file.
(defun mike-load-custom ()
  "Load customizations from emacs-custom.el except on Aquamacs."
  (when (not (featurep 'aquamacs))
    (setq custom-file (expand-file-name "emacs-custom.el"
                                        user-emacs-directory))
    (load custom-file 'noerror)))

(defun mike-setup-global-keys ()
  "Initialize global keys"
  (global-set-key (kbd "M-o" 'other-window))
  (global-set-key (kbd "C-c g" 'magit-status)))

(defun mike-load-packages ()
  "Load mikemacs packages."
  (dolist (p mike-packages)
    (let ((pkg           (if (consp p) (car p) p))
          (init-function (if (consp p) (cdr p) nil)))
      (require pkg)
      (when init-function (funcall init-function)))))

(defun mike-init ()
  "Load customizations, load mikemacs packages and setup global modes and keybindings."
  (mike-load-custom)
  (mike-load-packages)
  (mike-setup-global-keys)
  (mike-init-globals)
  (unless (mike-all-packages-installed-p)
    (message "Package(s) missing, run M-x mike-install-packages to install.")))


;; * BUFFER UTILS

(defun mike-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

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

(global-set-key (kbd "C-c .") 'mike-clean-buffer)

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

(global-set-key (kbd "C-x C-r") 'mike-rename-buffer-file)

(defun mike-last-mode-buffer (mode l)
  "Get latest buffer with major-mode MODE in buffer list L."
  (when l
    (if (eq mode (with-current-buffer (car l) major-mode))
        (car l)
      (mike-last-mode-buffer mode (cdr l)))))

(defun mike-get-mode-buffer (mode f)
  "Pop to or create a buffer with major-mode MODE.

Create with creation function F."
  (let ((b (mike-last-mode-buffer mode (buffer-list))))
    (if (or (not b) (eq mode major-mode))
        (funcall f)
      (let ((vis (get-buffer-window-list b)))
        (if vis
            (pop-to-buffer b 'display-buffer-reuse-window)
          (switch-to-buffer b))))))

(defun mike-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
  logical line.  This is useful, e.g., for use with
  `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;; * PATH

(defun mike-add-to-path (dirname)
  "Prepend DIRNAME to $PATH.

Do nothing if $PATH already contains DIRNAME."
  (let ((path (split-string (getenv "PATH") ":")))
    (if (member dirname path)
        (getenv "PATH")
      (setenv "PATH"
              (mapconcat 'identity (cons dirname path) ":")))))

(defun mike-string-ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
         (let ((elength (length ending)))
           (string= (substring s (- 0 elength)) ending)))
        (t nil)))

(defun mike-update-plugins ()
  "Update `load-path' with plugins from `mike-plugins-dir'."
  (interactive)
  (add-to-list 'load-path mike-plugins-dir)
  (dolist (dirname (directory-files mike-plugins-dir t))
    (when (and (not (mike-string-ends-with "." dirname))
               (file-directory-p dirname))
      (add-to-list 'load-path dirname))))

(defun mike-update-paths ()
  "Update $PATH and `exec-path'.

Update $PATH environment variable with `mike-path-paths'.
Update `exec-path' with `mike-exec-paths'.
Update `load-path' with `mike-plugins-dir'."
  (interactive)
  ;; $PATH
  (dolist (path mike-path-paths) (mike-add-to-path path))
  ;; exec-path
  (dolist (path mike-exec-paths)
    (add-to-list 'exec-path path))
  ;; plugins
  (mike-update-plugins))

(defun mike-init-paths ()
  (mike-update-paths)
  (when (and (memq window-system '(mac ns))
             (try-require 'exec-path-from-shell))
    (exec-path-from-shell-initialize)))


;; * GLOBAL SETTINGS

(defun mike-init-backups ()
  "Initialize backup directory and temporary files"
  (make-directory mike-backup-dir t)
  (setq backup-directory-alist `(("." . ,mike-backup-dir))
        auto-save-file-name-transforms `((".*" ,mike-backup-dir t))
        vc-make-backup-files t
        comment-auto-fill-only-comments t))

(defun mike-init-global-settings ()
  (mike-init-backups)
  (setq-default show-trailing-whitespace t
                column-number-mode t))

(defun mike-init-global-modes ()
  ;; smart indentation
  (electric-indent-mode 1)

  (try-require 'diminish)
  (try-require 'uniquify)
  (try-require 'auto-rsync)
  (try-require 'flx-ido)
  (try-require 'bookmark+)
  (try-require 'prelude-lite)
  (try-require 'ack-and-a-half)

  (when (try-require 'smart-mode-line)
    (setq sml/theme 'dark)
    (sml/setup))

  (when (try-require 'undo-tree)
    (global-undo-tree-mode)
    (when (try-require 'diminish)
      (diminish 'undo-tree-mode)))

  (when (try-require 'yasnippet)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand-from-trigger-key)
    (yas-global-mode 1)
    (when (try-require 'diminish)
      (diminish 'yas-minor-mode)))

  ;; Recent files
  (when (try-require 'recentf)
    (recentf-mode))

  (when (try-require 'autopair)
    (autopair-global-mode)
    (diminish 'autopair-mode))

  (when (try-require 'ido-vertical-mode)
    (ido-mode 1)
    (ido-vertical-mode))

  (when (try-require 'projectile)
    (projectile-global-mode)
    (diminish 'projectile-mode))

  (when (try-require 'undohist)
    (undohist-initialize))

  (when (try-require 'auto-complete)
    (global-auto-complete-mode)
    (diminish 'auto-complete-mode))

  (when (try-require 'smartparens)
    (show-smartparens-global-mode 1)))

(defun mike-init-globals ()
  (mike-init-global-settings)
  (mike-init-global-modes)
  ;; Remove auto-fill from text-mode added by emacs-starter-kit
  (when (member #'turn-on-auto-fill text-mode-hook)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill))
  ;; I like the menu bar, disabled in starter-kit
  (when window-system
    (menu-bar-mode)))


(provide 'mikemacs)
