;; From
;; http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/

(require 'cl)
(require 'em-hist)                   ; So the history vars are defined
(require 'eshell)

(setq eshell-history-size 1024
      eshell-prompt-regexp "^[^#$]*[#$] "
      eshell-save-history-on-exit t
      eshell-highlight-prompt nil
      my-eshell-prompt-face 'default
      my-eshell-prompt-pwd-face 'default
      my-eshell-prompt-git-face 'default)

(defun eshell/git-dir (pwd)
  "Return the git directory dominating PWD"
  (let ((git-dir (locate-dominating-file pwd ".git")))
    (when git-dir
      (concat (file-name-as-directory git-dir) ".git"))))

;; thanks to “Pascal J Bourguignon” and
;; “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
(defun eshell/get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun eshell/git-merge-interactive-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-merge/interactive")))

(defun eshell/git-merge-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-merge")))

(defun eshell/git-merge-head-name (git-dir)
  (eshell/get-string-from-file (concat git-dir "/rebase-merge/head-name")))

(defun eshell/git-status-vars (git-dir)
  (destructuring-bind (r b)
      (cond ((eshell/git-merge-interactive-p git-dir)
             (list "|REBASE-i"
                   (eshell/git-merge-head-name git-dir)))
            ((eshell/git-merge-p git-dir)
             (list "|REBASE-m"
                   (eshell/git-merge-head-name git-dir)))
            (t (let ((r))
                 (list r b))))
    ))

(defun eshell/git-prompt (pwd)
  "Mimics functionality of __git_ps1 in git-completion.bash"
  (let ((git-dir (my-eshell-git-dir pwd)))
    (when (and (eshell-search-path "git")
               git-dir)
      (destructuring-bind (&key r b) (eshell/git-status-vars git-dir)
        
        )))))

(defun eshell/pwd-repl-home (pwd)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
                        (if (> (length p-lst) 3)
                            (concat
                             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                   (substring elm 0 1)))
                                        (butlast p-lst 3)
                                        "/")
                             "/"
                             (mapconcat (lambda (elm) elm)
                                        (last p-lst 3)
                                        "/"))
                          (mapconcat (lambda (elm) elm)
                                     p-lst
                                     "/")))
                      (split-string (eshell/pwd-repl-home (eshell/pwd)) "/")) 'face my-eshell-prompt-pwd-face)
         (my-curr-dir-git-branch-string (eshell/pwd))
         (propertize "$ " 'face my-eshell-prompt-face))))

(provide 'eshell-git)
