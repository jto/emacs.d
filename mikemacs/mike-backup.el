;;; * Backup files

;;; Usage:
;;;   (require 'mike-backup)
;;;   (mike-backup-init)
;;;   (add-hook 'before-save-hook #'mike-force-backup-of-buffer)

;;; We make two kinds of backup.

;;; 1. Per-session backups: once on the first save of the buffer in
;;;    each Emacs session. This simulates Emacs' default backup
;;;    behavior.

;;; 2. Per-save backups: once on every save. Useful when leaving Emacs
;;;    running for a long time.

;;; The backups go in difference places and Emacs creates the backup
;;; directories automatically if they don't exists.

(defvar mike-backup-dir
  (expand-file-name "backups" user-emacs-directory)
  "Directory for Mikemacs backups.")

(defvar mike-backup-per-save-directory-alist
  `(("" . ,(expand-file-name "per-save" mike-backup-dir)))
  "Default and per-save backups go here.")

(defvar mike-backup-per-session-directory-alist
  `(("" . ,(expand-file-name "per-session" mike-backup-dir)))
  "Per-session backup override goes here.")

(defun mike-force-backup-of-buffer ()
  "Make a special per-session backup at the first save of each
Emacs session."
  (when (not buffer-backed-up)
    ;; Override default parameters for per-session backups.
    (let ((backup-directory-alist mike-backup-per-session-directory-alist)
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per-save" backup on each save. The first save results
  ;; in both a per-session and per-save backup, to keep the
  ;; numbering of per-save backups conistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(defun mike-backup-init ()
  "Backup suggestions from URL
`http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files'"
  ;; Create backup directory and all parent directories if it doesn't
  ;; exist.
  (make-directory mike-backup-dir t)
  (setq
   ;; Use version numbers for backups.
   version-control t
   ;; Number of newest versions to keep.
   kept-new-versions 10
   ;; number of oldest versions to keep.
   kept-old-versions 0
   ;; Don't ask to delete excess backup versions.
   delete-old-versions t
   ;; Copy files, don't rename them.
   backup-by-copying t
   ;; Also backup versioned files.
   vc-make-backup-files t
   ;; Set default and per-save backup directory.
   backup-directory-alist mike-backup-per-save-directory-alist))

(provide 'mike-backup)
