;; * init.el --- Mikemacs: Opinionated defaults for Mike's Emacs happiness.

;; * STARTUP

(defvar mike-emacs-load-start-time (current-time)
  "The time at which Emacs was started.

See URL `http://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html'.")

(require 'cl)

(defvar mike-emacs-dir
  (expand-file-name "~/.emacs.d")
  "The directory to use for Emacs configs.  Default is $HOME/.emacs.d/.")


;; * VARIABLE OVERRIDES

;; Use before-init.el to set variables to override defaults.
(let ((before-file (expand-file-name "before-init.el" mike-emacs-dir)))
  (when (file-exists-p before-file)
    (load-file before-file)))


;; * UTILS

;; ** STRING UTILS

;; Utils from
;; http://emacswiki.org/emacs/ElispCookbook
(defun mike-string-ends-with (str suffix)
  "Return t if STR ends with SUFFIX."
  (let ((elength (length suffix)))
    (string-equal suffix (substring str (- 0 elength)))))

(defun mike-string-starts-with (str prefix)
  "Return t if STR begins with PREFIX."
  (cond ((>= (length str) (length prefix))
         (string-equal prefix (substring str 0 (length prefix))))
        (t nil)))

;; ** BUFFER UTILS

(defun mike-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun mike-swap-buffers ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))


;; * PATH

(add-to-list 'load-path mike-emacs-dir)

;; ** VARIABLE DEFINITIONS

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
    (when (and (not (mike-string-ends-with dirname "."))
               (not (mike-string-ends-with dirname ".."))
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

(mike-update-paths)


;; * PACKAGES

;; Initialize with packages. Most importantly, emacs-starter-kit for
;; sane defaults.

(require 'package)

;; ** SETUP

;; Add package repositories.
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize package.
(package-initialize)

;; Create a list of packages to install if not present.
(when (not package-archive-contents)
  (package-refresh-contents))

;; ** VARIABLE DEFINITIONS

(defvar mike-package-packages '()
  "A list of packages to ensure are installed in Emacs.")
(dolist (p '(starter-kit
             starter-kit-lisp
             starter-kit-bindings
             starter-kit-eshell
             magit
             geiser
             graphviz-dot-mode
             autopair
             melpa
             exec-path-from-shell
             auctex
             yasnippet
             s
             ht
             dash
             yaml-mode
             color-theme-solarized
             virtualenv
             scala-mode2
             haskell-mode
             org
             outshine
             multi-term))
  (pushnew p mike-package-packages))

(defvar mike-exclude-packages '()
  "Packages to exclude from this Emacs instance.

Set this list in before-init.el to exclude unwanted packages.")

(defvar mike-missing-packages-list '()
  "A list of missing packages set by `try-require'.")

;; ** FUNCTIONS

(defun mike-package-to-install-p (pkg)
  "True if P should be installed and is not."
  (not (or (package-installed-p pkg)
           (member pkg mike-exclude-packages))))

(defun mike-check-packages ()
  "Check for uninstalled packages."
  (interactive)
  (let ((uninstalled-packages (cl-remove-if-not 'mike-package-to-install-p
                                                mike-package-packages)))
    (message
     (if (null uninstalled-packages)
         "All packages installed."
       "Uninstalled packages. Run `mike-install-packages' to install."))))

(defun mike-install-packages ()
  "Install uninstalled packages in `mike-package-packages'."
  (interactive)
  (dolist (pkg mike-package-packages)
    (when (mike-package-to-install-p pkg)
      (condition-case-unless-debug err
          (package-install pkg)
        (error (message "%s" (error-message-string err)))))))

(defun try-require (feature)
  "Attempt to load a library or module named FEATURE.

Return true if the library given as argument is successfully
loaded. If not, instead of an error, just add the package to a
list of missing packages.

From URL `http://www.mygooglest.com/fni/dot-emacs.html'."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature)
        feature)
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'mike-missing-packages-list feature 'append))
     nil)))

(mike-install-packages)

;; ** EXEC-PATH-FROM-SHELL

;; Pick up $PATH from environment on GUI systems.
(when (and (memq window-system '(mac ns))
           (try-require 'exec-path-from-shell))
  (exec-path-from-shell-initialize))



;; * GLOBAL MODES

;; Recent files
(when (try-require 'recentf)
  (recentf-mode))

(try-require 'yasnippet)

;; I like the menu bar, disabled in starter-kit
(when window-system
  (menu-bar-mode))

;; ** GLOBAL KEYBINDINGS

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'reindent-then-newline-and-indent)

(global-set-key (kbd "C-c g") 'magit-status)


;; * ORG-MODE

(try-require 'org)

;; Org-mode and mobile-org customizations
;; set org-mobile-encryption-password in custom
(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (expand-file-name "org" dropbox-dir))
(setq org-agenda-file (expand-file-name "agendafiles.txt" org-directory))
(setq org-mobile-directory (expand-file-name "MobileOrg" dropbox-dir))
(setq org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory))

;; Enable column number mode everywhere.
(setq column-number-mode t)

;; ** Outshine

(when (require 'outshine nil 'NOERROR)
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Redefine arrow keys, since promoting/demoting and moving
              ;; subtrees up and down are less frequent tasks then
              ;; navigation and visibility cycling
              (org-defkey org-mode-map
                          (kbd "M-<left>") 'outline-hide-more)
              (org-defkey org-mode-map
                          (kbd "M-<right>") 'outline-show-more)
              (org-defkey org-mode-map
                          (kbd "M-<up>") 'outline-previous-visible-heading)
              (org-defkey org-mode-map
                          (kbd "M-<down>") 'outline-next-visible-heading))
            'append)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'sh-mode-hook 'outline-minor-mode)
  (eval-after-load "outline"
    (progn
      (define-key outline-minor-mode-map (kbd "<tab>") nil))))


;; * PERL

(when (try-require 'cperl-mode)

  (try-require 'autopair)

  ;; modes
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\)\\'" . cperl-mode))
  (add-to-list 'auto-mode-alist '("\\.psgi\\'" . cperl-mode))

  ;; CPerl customizations
  (defun mike-cperl-mode-hook ()
    (setq cperl-indent-level                4
          cperl-close-paren-offset         -4
          cperl-continued-statement-offset  4
          cperl-indent-parens-as-block      t
          cperl-tab-always-indent           t
          cperl-merge-trailing-else         nil)
    (auto-fill-mode 0)
    (when (featurep 'autopair) (autopair-mode 1)))

  (add-hook 'cperl-mode-hook 'mike-cperl-mode-hook))


;; * SCALA

(when (and (try-require 'scala-mode)
           (try-require 'ensime))
  (add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
  ;; Add ensime to scala
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))


;; * YAML

(when (try-require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.cfg\\'" . yaml-mode)))


;; * GRAPHVIZ DOT

(when (try-require 'graphviz-dot-mode)

  (add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

  ;; Graphviz customizations
  (setq graphviz-dot-auto-indent-on-braces nil)
  (setq graphviz-dot-auto-indent-on-semi nil)
  (setq graphviz-dot-indent-width 4))


;; * SQL

;; ** MySQL

(add-to-list 'auto-mode-alist '("\\.mysql\\'" . sql-mode))


;; * ELISP

;; ** IELM
(require 'ielm)

;; Add paredit-mode to IELM
(when (and (try-require 'ielm)
           (try-require 'paredit))

  (defun mike-ielm-mode-hook ()
    (autopair-mode 0)
    (paredit-mode 1))
  (add-hook 'ielm-mode-hook 'mike-ielm-mode-hook))


;; * JAVASCRIPT

(try-require 'js)


;; * TERM

;; ** ANSI-TERM
(when (try-require 'term)

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
  (global-set-key (kbd "<f2>") 'mike-visit-ansi-term)
  (global-set-key (kbd "C-c t") 'mike-visit-ansi-term)

  (defvar mike-term-shell "/bin/bash")
  (defadvice ansi-term (before force-bash)
    (interactive (list mike-term-shell)))
  (ad-activate 'ansi-term)

  (defun mike-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'mike-term-use-utf8)

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
  (add-hook 'term-mode-hook 'mike-term-mode-hook))

;; ** MULTI-TERM

(when (try-require 'multi-term)
  (defvar mike-multi-term-program "/bin/bash"
    "Shell to run with multi-term, bash by default.")

  (defun mike-last-term-buffer (l)
    "Return most recently used term buffer.

From URL `http://www.emacswiki.org/emacs/MultiTerm'."
    (when l
      (if (eq 'term-mode (with-current-buffer (car l) major-mode))
          (car l) (mike-last-term-buffer (cdr l)))))

  (defun mike-get-term ()
    "Switch to the term buffer last used, or create a new one if
none exists, or if the current buffer is already a term.

From URL `http://www.emacswiki.org/emacs/MultiTerm'."
    (interactive)
    (let ((b (mike-last-term-buffer (buffer-list))))
      (if (or (not b) (eq 'term-mode major-mode))
          (multi-term)
        (let ((vis (get-buffer-window-list b)))
          (if vis
              (pop-to-buffer b 'display-buffer-reuse-window)
            (switch-to-buffer b))))))

  (setq multi-term-program mike-multi-term-program)
  (setq multi-term-switch-after-close t)

  (global-set-key (kbd "<f2>") 'mike-get-term)
  (global-set-key (kbd "C-c t") 'mike-get-term))


;; * TEX

(try-require 'auctex-autoloads)

;; Load auctex settings
(when (featurep 'ns)
  ;; Settings work on OS X; work on making these xplaf
  (setq TeX-PDF-mode t
        TeX-view-program-list '(("Open" "open \"%o\""))
        TeX-view-program-selection '((output-pdf "Open"))))


;; * SCHEME

;; Geiser customizations (Scheme Slime-like environment)
;; Disable read-only prompt in Geiser.
;; The read-only prompt doesn't play nicely with custom REPLs
;; such as in SICP.
(setq geiser-repl-read-only-prompt-p nil)
(eval-after-load "geiser"
  (progn
    (add-hook 'geiser-repl-mode-hook 'paredit-mode)))


;; * SSH

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


;; * PRELUDE

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
  "Open the underlying file of a buffer in an external program."
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


;; * ESHELL

;; eshell customizations
(load (expand-file-name "~/.emacs.d/eshell-custom.el") 'noerror)

;; ansi-term improvements
;; http://emacs-journey.blogspot.com/2012/06/improving-ansi-term.html

(defadvice term-sentinel (around mike-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
;; (ad-activate 'term-sentinel)


;; * PYTHON

(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))
;; (defun workon-postactivate (virtualenv)
;;   (require 'virtualenv)
;;   (virtualenv-workon virtualenv)
;;   (desktop-change-dir virtualenv))


;; * THEME

(load-theme 'solarized-light t)


;; * HASKELL

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; * CUSTOM FILE

;; Separate custom file.
(when (not (featurep 'aquamacs))
  (setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el"))
  (load custom-file 'noerror))

;; Final machine-specific settings.
(let ((after-file (expand-file-name "after-init.el" mike-emacs-dir)))
  (when (file-exists-p after-file)
    (load-file after-file)))


;; * EMACS SERVER

(defvar mike-server-start t
  "Start server after initialization.  Default is true.")

;; Start Emacs server.
(require 'server)
(when (and mike-server-start (not (server-running-p)))
  (server-start))


;; * FINISH

;; Time Emacs startup complete.
(message "Emacs startup in %ds"
         (let ((time (current-time)))
           (let ((hi (first time))
                 (lo (second time)))
             (- (+ hi lo)
                (+ (first mike-emacs-load-start-time)
                   (second mike-emacs-load-start-time))))))

(provide 'init)

;;; init.el ends here
