;; * GLOBAL SETTINGS

;; Backups

(defcustom mike-backup-dir
  (expand-file-name "backups" user-emacs-directory)
  "Directory for Emacs backups. Default is `user-emacs-directory'/backups."
  :type 'string
  :group 'mikemacs)

;; ** GLOBAL MODES

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

  (when (try-require 'smart-mode-line)
    (setq sml/theme 'dark)
    (sml/setup))

  (when (try-require 'undo-tree)
    (global-undo-tree-mode)
    (when (try-require 'diminish)
      (diminish 'undo-tree-mode)))

  (when (try-require 'yasnippet)
    (define-key yas-minor-mode-map [(tab)] nil)
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


(defun mike-init-global-keybindings ()
  (global-set-key (kbd "C-c g") 'magit-status))


(defun mike-init-globals ()
  (mike-init-global-settings)
  (mike-init-global-modes)
  (mike-init-global-keybindings)

  ;; Remove auto-fill from text-mode added by emacs-starter-kit
  (when (member #'turn-on-auto-fill text-mode-hook)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill))

  ;; I like the menu bar, disabled in starter-kit
  (when window-system
    (menu-bar-mode)))


(provide 'mike-globals)
