;; * EMACS SERVER

(defcustom mike-server-start t
  "Start server after initialization.  Default is true."
  :type 'boolean
  :group 'mikemacs)

(defun mike-start-server-after-init ()
  (require 'server)
  (when (and mike-server-start (not (server-running-p)))
    (server-start)))

(add-hook 'after-init-hook 'mike-start-server-after-init)

(provide 'mike-server)
