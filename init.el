;; * init.el --- Mikemacs: Opinionated defaults for Mike's Emacs happiness.

;; * STARTUP

(require 'cl)

(defvar mike-emacs-load-start-time (current-time)
  "The time at which Emacs was started.

See URL `http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html'.")

;; Use before-init.el to set variables to override defaults.
(let ((before-file (expand-file-name "before-init.el" user-emacs-directory)))
  (when (file-exists-p before-file)
    (load-file before-file)))


(pushnew (expand-file-name "site-lisp" user-emacs-directory) load-path)


;; * PACKAGES

(require 'mikemacs)


;; * FINISH

(let ((after-file (expand-file-name "after-init.el" user-emacs-directory)))
  (when (file-exists-p after-file)
    (load-file after-file)))

;; Final machine-specific settings.

;; Time Emacs startup complete.
(message "Emacs startup in %ds"
         (let ((time (current-time)))
           (let ((hi (first time))
                 (lo (second time)))
             (- (+ hi lo)
                (+ (first mike-emacs-load-start-time)
                   (second mike-emacs-load-start-time))))))

(provide 'init)

;; init.el ends here
