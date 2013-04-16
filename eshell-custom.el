;; From
;; http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/

(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(require 'em-hist)                   ; So the history vars are defined
(setq eshell-save-history-on-exit t
      eshell-highlight-prompt nil
      my-eshell-prompt-face 'default
      my-eshell-prompt-pwd-face 'default
      my-eshell-prompt-git-face 'default)

;; path manipulation

(defun my-pwd-repl-home (pwd)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))



(defun my-curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
                          (if (> (length git-output) 0)
                              (substring git-output 0 -1)
                            "(no branch)")
                          "]")
                  'face my-eshell-prompt-git-face))))

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
                      (split-string (my-pwd-repl-home (eshell/pwd)) "/")) 'face my-eshell-prompt-pwd-face)
         (my-curr-dir-git-branch-string (eshell/pwd))
         (propertize "$ " 'face my-eshell-prompt-face))))
