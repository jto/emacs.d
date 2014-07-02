;; * ANSI-COLOR

(defun mike-init-ansi-color ()
  (when (try-require 'ansi-color)
    (defadvice mike-display-message-or-buffer (before ansi-color activate)
      "Process ANSI color codes in shell output."
      (let ((buf (ad-get-arg 0)))
        (and (bufferp buf)
             (string= (buffer-name buf) "*Shell Command Output*")
             (with-current-buffer buf
               (ansi-color-apply-on-region (point-min) (point-max))))))))

(add-hook 'after-init-hook 'mike-init-ansi-color)

;; * TERM

;; ** ANSI-TERM

(defcustom mike-term-shell "/bin/bash"
  "Shell to run with `ansi-term', bash by default."
  :type 'file
  :group 'mikemacs)

(defun mike-visit-ansi-term ()
  "If the current buffer is:
1) a running `ansi-term' named *ansi-term*, rename it.
2) a stopped `ansi-term', kill it and create a new one.
3) a non `ansi-term',
go to an already running `ansi-term' or start a new one
while killing a defunct one.

From URL `http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/'."
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun mike-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(defun mike-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun mike-term-mode-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'mike-term-paste)
  (autopair-mode 0)
  (setq show-trailing-whitespace nil))

(defun mike-init-term ()
  (when (try-require 'term)
    (defadvice mike-ansi-term (before force-bash activate)
      (interactive (list mike-term-shell)))
    (add-hook 'term-exec-hook 'mike-term-use-utf8)
    (add-hook 'term-mode-hook 'mike-term-mode-hook)))

(add-hook 'after-init-hook 'mike-init-term)

;; ** MULTI-TERM

(defcustom mike-multi-term-program "/bin/bash"
  "Shell to run with `multi-term', bash by default."
  :type 'file)

(defun mike-get-term ()
  "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term.

From URL `http://www.emacswiki.org/emacs/MultiTerm'."
  (interactive)
  (mike-get-mode-buffer 'term-mode 'multi-term))

(defun mike-term-send-escape ()
  "Send <esc> in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun mike-term-send-tab ()
  "Send <tab> in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(defun mike-init-multi-term ()
  (when (try-require 'multi-term)
    (setq multi-term-program mike-multi-term-program)
    (setq multi-term-switch-after-close t)

    (add-to-list 'term-bind-key-alist '("C-c C-e" . mike-term-send-escape))
    (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
    (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
    (add-to-list 'term-bind-key-alist '("<tab>"   . mike-term-send-tab))

    (global-set-key (kbd "<f2>") 'mike-get-term)
    (global-set-key (kbd "C-c t") 'mike-get-term)))

(add-hook 'after-init-hook 'mike-init-multi-term)

(provide 'mike-term)
