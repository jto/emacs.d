;; From
;; http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/

(require 'cl)
(require 'eshell)

(defgroup eshell-git nil
  "Git customizations for eshell.
   Similar to git-completion.bash"
  :prefix "eshell-git/")

(defcustom eshell-git/prompt-face 'default
  "Overall face for eshell prompt"
  :type 'face
  :group 'eshell-git)

(defcustom eshell-git/pwd-face 'default
  "Face for working directory in eshell prompt"
  :type 'face
  :group 'eshell-git)

(defcustom eshell-git/branch-face 'default
  "Face for git branch name in eshell prompt"
  :type 'face
  :group 'eshell-git)

(defcustom eshell-git/describe-style nil
  "Style for git describe used in branch display name"
  :type 'symbol
  :options '(contains branch describe nil)
  :group 'eshell-git)

(defcustom eshell-git/show-dirty-state t
  "Show working directory and index dirty state"
  :type 'boolean
  :group 'eshell-git)

(defcustom eshell-git/show-stash-state t
  "Show if there are contents in the stash"
  :type 'boolean
  :group 'eshell-git)

(defcustom eshell-git/show-untracked-files t
  "Show if there are untracked files in the working directory"
  :type 'boolean
  :group 'eshell-git)

(defcustom eshell-git/show-upstream t
  "Show sync status of branch with upstream"
  :type 'boolean
  :group 'eshell-git)

(defun eshell-git/git-dir (pwd)
  "Return the git directory dominating PWD"
  (let ((git-dir (locate-dominating-file pwd ".git")))
    (when git-dir
      (concat (file-name-as-directory git-dir) ".git"))))

;; thanks to “Pascal J Bourguignon” and
;; “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
(defun eshell-git/get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun eshell-git/last-char (s)
  (let ((l (length s)))
    (if (> l 0)
        (substring s -1)
      "")))

(defun eshell-git/chomp (s)
  (let ((l (length s)))
    (if (member (eshell-git/last-char s) '("\n" " " "\t" "\r"))
        (eshell-git/chomp (substring s 0 (- l 1)))
      s)))

(defun eshell-git/get-line-from-file (filePath)
  (eshell-git/chomp (eshell-git/get-string-from-file filePath)))

(defun eshell-git/rebase-merge-head-name (git-dir)
  (eshell-git/get-line-from-file
   (concat git-dir "/rebase-merge/head-name")))

(defun eshell-git/head (git-dir)
  (eshell-git/get-line-from-file
   (concat git-dir "/HEAD")))

(defun eshell-git/rebase-merge-interactive-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-merge/interactive")))

(defun eshell-git/rebase-merge-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-merge")))

(defun eshell-git/rebase-apply-rebasing-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-apply/rebasing")))

(defun eshell-git/rebase-apply-applying-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-apply/applying")))

(defun eshell-git/rebase-apply-p (git-dir)
  (file-exists-p (concat git-dir "/rebase-apply")))

(defun eshell-git/merge-head-p (git-dir)
  (file-exists-p (concat git-dir "/MERGE_HEAD")))

(defun eshell-git/cherry-pick-head-p (git-dir)
  (file-exists-p (concat git-dir "/CHERRY_PICK_HEAD")))

(defun eshell-git/bisect-log-p (git-dir)
  (file-exists-p (concat git-dir "/BISECT_LOG")))

(defun eshell-git/rebase-display (git-dir)
  (cond
   ((eshell-git/rebase-merge-interactive-p git-dir) "|REBASE-i")
   ((eshell-git/rebase-merge-p git-dir)             "|REBASE-m")
   ((eshell-git/rebase-apply-rebasing-p git-dir)    "|REBASE")
   ((eshell-git/rebase-apply-applying-p git-dir)    "|AM")
   ((eshell-git/rebase-apply-p git-dir)             "|AM/REBASE")
   ((eshell-git/merge-head-p git-dir)               "|MERGING")
   ((eshell-git/cherry-pick-head-p git-dir)         "|CHERRY-PICKING")
   ((eshell-git/bisect-log-p git-dir)               "|BISECTING")
   (t "")))

(defun eshell-git/do (&rest args)
  (with-temp-buffer
    (apply 'process-file "git" nil '(t nil) t args)
    (eshell-git/chomp (buffer-string))))

(defun eshell-git/do-exit (&rest args)
  "Call git with ARGS and return exit status"
  (apply 'process-file "git" nil nil nil args))

(defmacro eshell-git/string-or (&rest args)
  (if (not args)
      ""
    (let ((s (make-symbol "s")))
      `(let ((,s ,(car args)))
         (if (not (string= "" ,s))
             ,s
           (eshell-git/string-or ,@(cdr args)))))))

(defun eshell-git/branch-display (git-dir r)
  (replace-regexp-in-string
   "refs/heads/"
   ""
   (if (or (string= r "|REBASE-i")
           (string= r "|REBASE-m"))
       (eshell-git/rebase-merge-head-name git-dir)
     (eshell-git/string-or
      (eshell-git/do "symbolic-ref" "HEAD")
      (concat
       "("
       (eshell-git/string-or
        (case eshell-git/describe-style
          ('contains (eshell-git/do "describe" "--contains" "HEAD"))
          ('branch (eshell-git/do "describe" "--contains" "--all" "HEAD"))
          ('describe (eshell-git/do "describe" "HEAD"))
          (t (eshell-git/do "describe" "--tags" "--exact-match" "HEAD")))
        (substring (eshell-git/head git-dir) 0 7)
        "unknown")
       ")")))))

(defun eshell-git/inside-git-dir-p ()
  (string= "true" (eshell-git/do "rev-parse" "--is-inside-git-dir")))

(defun eshell-git/inside-work-tree-p ()
  (string= "true" (eshell-git/do "rev-parse" "--is-inside-work-tree")))

(defun eshell-git/bare-repo-p ()
  (string= "true" (eshell-git/do "rev-parse" "--is-bare-repository")))

(defun eshell-git/dirty-work-tree-p ()
  (not (= 0 (eshell-git/do-exit "diff" "--no-ext-diff"
                                "--quiet" "--exit-code"))))

(defun eshell-git/dirty-index-p ()
  (not (= 0 (eshell-git/do-exit "diff-index" "--cached" "--quiet" "HEAD"))))

(defun eshell-git/verify-head-p ()
  (= 0 (eshell-git/do-exit "rev-parse" "--quiet" "--verify" "HEAD")))

(defun eshell-git/stash-exists-p ()
  (= 0 (eshell-git/do-exit "rev-parse" "--verify" "refs/stash")))

(defun eshell-git/untracked-files-exist-p ()
  (> (length (eshell-git/do "ls-files" "--others" "--exclude-standard")) 0))

(defun eshell-git/upstream-status ()
  (let ((count (eshell-git/do "rev-list"
                              "--count" "--left-right"
                              "@{upstream}...HEAD")))
    (when (string-match "\\([[:digit:]]+\\)\t\\([[:digit:]]+\\)" count)
      (let ((left (string-to-number (match-string 1 count)))
            (right (string-to-number (match-string 2 count))))
        (cond ((and (= 0 left) (= 0 right)) 'equal)
              ((= 0 left) 'ahead)
              ((= 0 right) 'behind)
              (t 'diverged))))))

(defun eshell-git/upstream-status-string ()
  (case (eshell-git/upstream-status)
    ('equal "=")
    ('ahead ">")
    ('behind "<")
    ('diverged "<>")
    (t "")))

(defun eshell-git/status-string (git-dir)
  (let* ((r (eshell-git/rebase-display git-dir))
         (b (eshell-git/branch-display git-dir r)))
    (let ((w "") (i "") (s "") (u "") (c "") (p ""))
      (if (eshell-git/inside-git-dir-p)
          (if (eshell-git/bare-repo-p)
              (setq c "BARE:")
            (setq b "GIT_DIR!"))
        (when (eshell-git/inside-work-tree-p)
          (when eshell-git/show-dirty-state
            (when (eshell-git/dirty-work-tree-p)
              (setq w "*"))
            (if (eshell-git/verify-head-p)
                (when (eshell-git/dirty-index-p)
                  (setq i "+"))
              (setq i "#")))
          (when eshell-git/show-stash-state
            (when (eshell-git/stash-exists-p)
              (setq s "$")))
          (when eshell-git/show-untracked-files
            (when (eshell-git/untracked-files-exist-p)
              (setq u "%")))
          (when eshell-git/show-upstream
            (setq p (eshell-git/upstream-status-string)))))
      (let ((f (concat w i s u)))
        (concat c b (if (> 0 (length f)) (concat " " f) "") r p)))))

(defun eshell-git/git-prompt (&optional fmt pwd)
  "Mimics functionality of __git_ps1 in git-completion.bash"
  (let* ((fmt (or fmt " (%s)"))
         (pwd (or pwd (eshell/pwd)))
         (git-dir (eshell-git/git-dir pwd)))
    (if (and git-dir (eshell-search-path "git"))
        (format fmt (eshell-git/status-string pwd))
      "")))

(defun eshell-git/pwd-repl-home (pwd)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun eshell-git/prompt-function ()
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
                (split-string (eshell-git/pwd-repl-home
                               (eshell/pwd))
                              "/"))
               'face eshell-git/pwd-face)
   (propertize (eshell-git/git-prompt) 'face eshell-git/branch-face)
   (propertize "$ " 'face eshell-git/prompt-face)))


;; The following are from:
;; http://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))


(provide 'eshell-git)
