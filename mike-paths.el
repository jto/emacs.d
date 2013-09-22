;;; mike-paths.el --- Setup paths for mikemacs

;;; ===========
;;; = REQUIRE =
;;; ===========

(require 'cl)
(require 'mike-utils)

;;; ========================
;;; = VARIABLE DEFINITIONS =
;;; ========================

(defvar mike-path-paths
  '("/usr/local/bin")
  "Paths to safely add to $PATH environment variable.

Useful for graphical Emacs that may not pick up $PATH from user's bashrc.")

(defvar mike-exec-paths
  (map 'list 'expand-file-name mike-path-paths)
  "Paths to add to Emacs `exec-path'. Defaults to `mike-path-paths'.")

(defvar mike-plugins-dir
  (file-name-as-directory (expand-file-name "plugins" mike-emacs-dir))
  "Plugins directory. Defaults to `mike-emacs-dir'/plugins/.")

;;; =============
;;; = FUNCTIONS =
;;; =============

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
  (pushnew mike-plugins-dir load-path)
  (dolist (dirname (directory-files mike-plugins-dir t))
    (when (and (not (mike-string-ends-with dirname "."))
               (not (mike-string-ends-with dirname ".."))
               (file-directory-p dirname))
      (pushnew dirname load-path))))

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
    (pushnew path exec-path))
  ;; plugins
  (mike-update-plugins))

;;; ===========
;;; = PROVIDE =
;;; ===========

(provide 'mike-paths)
