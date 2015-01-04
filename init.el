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

(let ((before-file (expand-file-name "before-init.el" user-emacs-directory)))
  (when (file-exists-p before-file)
    (load-file before-file)))

;;; Starting in Emacs 24.3, the official documentation recommends we
;;; require cl-lib instead of cl.

;;; Package cl-lib gives us access to nifty macros and functions from
;;; Common Lisp. My favorite is `pushnew'.

(require 'cl-lib)

;;; * User Details

(setq user-full-name "Mike Prentice")

;;; * Environment

;;; Add site-lisp to the load path to pick up libraries I install
;;; manually, outside of package management tools.

(pushnew (expand-file-name "site-lisp" user-emacs-directory) load-path)

;;; * Packages

;;; Include mikemacs package. I'll be reviewing the contained modules
;;; and gradually incorporating them into this configuration file.

(require 'mikemacs)


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
           (let ((current-hi (first time))
                 (current-lo (second time))
                 (startup-hi (first mike-emacs-load-start-time))
                 (startup-lo (second mike-emacs-load-start-time)))
             (- (+ current-hi current-lo) (+ startup-hi startup-lo)))))
