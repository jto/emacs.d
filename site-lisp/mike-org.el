;; * ORG-MODE

(defcustom mike-dropbox-dir
  (expand-file-name "Dropbox" (getenv "HOME"))
  "The location of the dropbox folder. Default is ~/Dropbox."
  :type 'string
  :group 'mikemacs)

(defcustom mike-org-use-dropbox t
  "If true, setup `org-mode' to use `mike-dropbox-dir'/org.
Default is true."
  :type 'boolean
  :group 'mikemacs)

(defun mike-org-mode-hook ()
  ;; Redefine arrow keys, since promoting/demoting and moving
  ;; subtrees up and down are less frequent tasks then
  ;; navigation and visibility cycling
  (org-defkey org-mode-map
              (kbd "M-<left>") 'outline-hide-more)
  (org-defkey org-mode-map
              (kbd "M-<right>") 'outline-show-more)
  (org-defkey org-mode-map
              (kbd "M-<up>") 'outline-previous-visible-heading)
  (org-defkey org-mode-map
              (kbd "M-<down>") 'outline-next-visible-heading))

(defun mike-init-org ()
  (try-require 'org-autoloads)
  (try-require 'org-mobile)

  ;; Org-mode and mobile-org customizations
  ;; set org-mobile-encryption-password in custom
  (when mike-org-use-dropbox
    (setq org-directory (expand-file-name "org" mike-dropbox-dir))
    (setq org-agenda-file (expand-file-name "agendafiles.txt" org-directory))
    (setq org-mobile-directory
          (expand-file-name "MobileOrg" mike-dropbox-dir))
    (setq org-mobile-inbox-for-pull
          (expand-file-name "flagged.org" org-directory)))

  (add-hook 'org-mode-hook 'mike-org-mode-hook 'append)

  (when (try-require 'outline)
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
    (add-hook 'sh-mode-hook 'outline-minor-mode)
    (define-key outline-minor-mode-map (kbd "<tab>") nil)
    (define-key outline-minor-mode-map
      (kbd "C-c C-o") 'outline-toggle-children)
    (diminish 'outline-minor-mode))
  (when (try-require 'outshine)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)))

(add-hook 'after-init-hook 'mike-init-org)


(provide 'mike-org)
