;; * PATH

(require 's)

;; ** VARIABLE DEFINITIONS

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

;; ** FUNCTIONS

(defun mike-add-to-path (dirname)
  "Prepend DIRNAME to $PATH.

Do nothing if $PATH already contains DIRNAME."
  (let ((path (split-string (getenv "PATH") ":")))
    (if (member dirname path)
        (getenv "PATH")
      (setenv "PATH"
              (mapconcat 'identity (cons dirname path) ":")))))

(defun mike-update-plugins ()
  "Update `load-path' with plugins from `mike-plugins-dir'."
  (interactive)
  (add-to-list 'load-path mike-plugins-dir)
  (dolist (dirname (directory-files mike-plugins-dir t))
    (when (and (not (s-ends-with? "." dirname))
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

(provide 'mike-path-utils)
