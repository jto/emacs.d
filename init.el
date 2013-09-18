(require 'cl)

;; Time Emacs startup.
;; From http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
(defvar *emacs-load-start* (current-time))

;; ===============================================

(let ((before-file (expand-file-name "~/.emacs.d/before-init.el")))
  (when (file-exists-p before-file)
    (load-file before-file)))

;; ===============================================

;; Add /usr/local/bin to env path
(defun my-add-to-path (dirname)
  "Prepend DIRNAME to $PATH.

Do nothing if $PATH already contains DIRNAME.

(fn DIRNAME)"
  (let ((path (split-string (getenv "PATH") ":")))
    (if (member dirname path)
        (getenv "PATH")
      (setenv "PATH"
              (mapconcat 'identity (cons dirname path) ":")))))
(my-add-to-path "/usr/local/bin")

(push "/usr/local/bin" exec-path)

;; ===============================================

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Create a list of packages to install if not present.
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar *my-packages* '()
  "A list of packages to ensure are installed at launch.")
(add-to-list '*my-packages* 'starter-kit)
(add-to-list '*my-packages* 'starter-kit-lisp)
(add-to-list '*my-packages* 'starter-kit-bindings)
(add-to-list '*my-packages* 'starter-kit-eshell)
(add-to-list '*my-packages* 'magit)
(add-to-list '*my-packages* 'geiser)
(add-to-list '*my-packages* 'graphviz-dot-mode)
(add-to-list '*my-packages* 'autopair)
(add-to-list '*my-packages* 'melpa)
(add-to-list '*my-packages* 'exec-path-from-shell)
(add-to-list '*my-packages* 'auctex)
(add-to-list '*my-packages* 'yasnippet)
(add-to-list '*my-packages* 's)
(add-to-list '*my-packages* 'ht)
(add-to-list '*my-packages* 'dash)
(add-to-list '*my-packages* 'yaml-mode)
(add-to-list '*my-packages* 'color-theme-solarized)
(add-to-list '*my-packages* 'virtualenv)
(add-to-list '*my-packages* 'scala-mode2)
(add-to-list '*my-packages* 'haskell-mode)
(dolist (p *my-packages*)
  (when (not (package-installed-p p))
    (condition-case-unless-debug err
        (package-install p)
      (error (message "%s" (error-message-string err))))))

(require 'autopair)
(require 'yasnippet)
(yas/global-mode 1)

;; ===============================================

;; Utils from
;; http://emacswiki.org/emacs/ElispCookbook
(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))
(defun string/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
                    (t nil)))

;; Add plugins to load path
(let ((plugin-dir (expand-file-name "~/.emacs.d/plugins")))
  (when (file-directory-p plugin-dir)
    (when (not (memq plugin-dir load-path))
      (add-to-list 'load-path plugin-dir))
    (dolist (d (directory-files plugin-dir t))
      (when (and (not (string/ends-with d "."))
                 (not (string/ends-with d ".."))
                 (not (memq d load-path))
                 (file-directory-p d))
        (add-to-list 'load-path d)))))

;; ===============================================

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ===============================================

;; Autoloads

;; Modes
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.mysql\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.psgi\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . yaml-mode))

;; Add paredit-mode to IELM
(add-hook 'ielm-mode-hook 'paredit-mode)

;; Add ensime to scala
(defvar my-ensime-base-dir "~/.emacs.d/plugins/ensime"
  "Base directory for ensime, set in before-init.el")
(defvar my-ensime-elisp-path (concat my-ensime-base-dir "/elisp")
  "Elisp directory for ensime, automagic, leave alone")
(when (file-directory-p my-ensime-elisp-path)
  (when (not (memq my-ensime-elisp-path load-path))
    (add-to-list 'load-path my-ensime-elisp-path))
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(add-hook 'js-mode-hook 'autopair-mode)

;; ===============================================

;; From
;; http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
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
(global-set-key (kbd "<f2>") 'visit-ansi-term)
(global-set-key (kbd "C-c t") 'visit-ansi-term)

;; ===============================================

;; CPerl customizations
(defun my-cperl-mode-hook ()
  (setq cperl-indent-level                4
        cperl-close-paren-offset         -4
        cperl-continued-statement-offset  4
        cperl-indent-parens-as-block      t
        cperl-tab-always-indent           t
        cperl-merge-trailing-else         nil)
  (auto-fill-mode 0)
  (autopair-mode 1))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

;; ===============================================

;; I like the menu.
(when window-system
  (menu-bar-mode))

;; Recent files
(recentf-mode)

;; Load auctex settings
(when (featurep 'ns)
  (setq TeX-PDF-mode t
        TeX-view-program-list '(("Open" "open \"%o\""))
        TeX-view-program-selection '((output-pdf "Open"))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'reindent-then-newline-and-indent)

(global-set-key (kbd "C-c g") 'magit-status)

;; ===============================================

;; Geiser customizations (Scheme Slime-like environment)
;; Disable read-only prompt in Geiser.
;; The read-only prompt doesn't play nicely with custom REPLs
;; such as in SICP.
(setq geiser-repl-read-only-promp-p nil)
(eval-after-load "geiser"
  (progn
    (add-hook 'geiser-repl-mode-hook 'paredit-mode)))

;; ===============================================

;; Graphviz customizations
(setq graphviz-dot-auto-indent-on-braces nil)
(setq graphviz-dot-auto-indent-on-semi nil)
(setq graphviz-dot-indent-width 4)

;; ===============================================

;; Org-mode and mobile-org customizations
;; set org-mobile-encryption-password in custom
(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (expand-file-name "org" dropbox-dir))
(setq org-agenda-file (expand-file-name "agendafiles.txt" org-directory))
(setq org-mobile-directory (expand-file-name "MobileOrg" dropbox-dir))
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))

;; Enable column number mode everywhere.
(setq column-number-mode t)

;; ===============================================

(defcustom fixssh-data-file 
  (expand-file-name (concat "~/usr/bin/fixssh_"
                            (getenv "HOSTNAME")))
  "The name of the file that contains environment info from grabssh."
  :type '(string))

(defun fixssh ()
  "Fix SSH agent and X forwarding in GNU screen.

Requires grabssh to put SSH variables in the file identified by
`fixssh-data-file'."
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect fixssh-data-file)))
      (set-buffer buffer)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (while (re-search-forward
              "\\([A-Z_][A-Z0-9_]*\\) *= *\"\\([^\"]*\\)\"" nil t)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (setenv key val)))
      (kill-buffer buffer))))

;; ===============================================

;; Nifty functions from prelude package
;; See emacsredux.com/blog

(defun prelude-smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'prelude-smart-open-line)

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(global-set-key (kbd "C-c o") 'prelude-open-with)

(defun prelude-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun prelude-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (prelude-indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'prelude-indent-region-or-buffer)

(defun prelude-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") 'prelude-indent-defun)

(defun prelude-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; ===============================================

;; eshell customizations
(load (expand-file-name "~/.emacs.d/eshell-custom.el") 'noerror)

;; ansi-term improvements
;; http://emacs-journey.blogspot.com/2012/06/improving-ansi-term.html

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
;; (ad-activate 'term-sentinel)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))
(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))
(add-hook 'term-mode-hook 'my-term-hook)

;; ===============================================

(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))
;; (defun workon-postactivate (virtualenv)
;;   (require 'virtualenv)
;;   (virtualenv-workon virtualenv)
;;   (desktop-change-dir virtualenv))

;; ===============================================

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;; ===============================================

(load-theme 'solarized-light t)

;; ===============================================

(defun my-swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    ))

;; ===============================================

;; Separate custom file.
(when (not (featurep 'aquamacs))
  (setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el"))
  (load custom-file 'noerror))

(let ((after-file (expand-file-name "~/.emacs.d/after-init.el")))
  (when (file-exists-p after-file)
    (load-file after-file)))

(server-start)

;; ===============================================

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ===============================================

;; Time Emacs startup complete.
(message "Emacs startup in %ds"
         (let ((time (current-time)))
           (let ((hi (first time))
                 (lo (second time)))
             (- (+ hi lo)
                (+ (first *emacs-load-start*)
                   (second *emacs-load-start*))))))
(put 'narrow-to-region 'disabled nil)
