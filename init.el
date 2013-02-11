(require 'cl)

;; Time Emacs startup.
;; From http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time))

;; Separate custom file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Time Emacs startup complete.
(message "Emacs startup in %ds"
	 (destructuring-bind (hi lo ms) (current-time)
			     (- (+ hi lo)
				(+ (first *emacs-load-start*)
				   (second *emacs-load-start*)))))
