;; * SSH

(defcustom fixssh-data-file
  (expand-file-name (concat "fixssh_"
                            (getenv "HOSTNAME"))
                    (expand-file-name "local/bin" (getenv "HOME")))
  "The name of the file that contains environment info from grabssh."
  :type 'file
  :group 'mikemacs)

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

(provide 'mike-ssh)
