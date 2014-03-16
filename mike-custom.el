;; * CUSTOM

(require 'custom)

;; ** CUSTOM DEFINITIONS

(defgroup mikemacs nil
  "Mikemacs: Opinionated defaults for Mike's Emacs happiness.
Customize these options as desired.")

(defcustom mike-hostname system-name
  "Hostname of current system. Default is `system-name'."
  :type 'string)

(defcustom mike-username (user-login-name)
  "Short account username of current user. Default is the value
returned by `user-login-name'."
  :type 'string)

(defcustom mike-email ""
  "Email of current user. Default is the empty string."
  :type 'string)

(defcustom mike-name (user-full-name)
  "Full name of current user. Default is the value returned by
`user-full-name'."
  :type 'string)

;; ** CUSTOM FILE

;; Separate custom file.
(defun mike-init-custom ()
  (when (not (featurep 'aquamacs))
    (setq custom-file (expand-file-name "emacs-custom.el"
                                        user-emacs-directory))
    (load custom-file 'noerror)))


(provide 'mike-custom)
