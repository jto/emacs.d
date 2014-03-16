;; * SQL

(defcustom mike-sql-upcase-keywords t
  "If non-nil will convert keywords to upper case in `mike-sql-electric-space'."
  :type 'boolean
  :group 'mikemacs)

(defconst mike-sql-keywords
  (map 'list 'symbol-name
       '( select  as      from    where   group   by      sort    asc     desc
                  between and     or      having  create  table   if      not
                  exists  int     not     null    primary key     constraint
                  delete  update ))
  "Keywords to upcase")

(defun mike-toggle-sql-upcase-keywords ()
  "Toggle whether to automatically upcase keywords in `sql-mode'."
  (interactive)
  (setq mike-sql-upcase-keywords (not mike-sql-upcase-keywords)))

(defun mike-sql-interactive-mode-hook ()
  (setq show-trailing-whitespace nil))

(defun mike-sql-upcase-keyword ()
  (when (and mike-sql-upcase-keywords
             (member (current-word t) mike-sql-keywords))
    (upcase-word -1)))

(defun mike-sql-electric-space (&optional n)
  (interactive)
  (mike-sql-upcase-keyword)
  (insert-char (string-to-char " ") (or n 1)))

(defun mike-init-sql ()
  (when (try-require 'sql)
    ;; Make _ part of a word
    (modify-syntax-entry ?_ "w" sql-mode-syntax-table)

    (define-key sql-mode-map (kbd "SPC") 'mike-sql-electric-space)
    (define-key sql-interactive-mode-map (kbd "SPC") 'mike-sql-electric-space)

    (add-hook 'sql-interactive-mode-hook 'mike-sql-interactive-mode-hook)))

(add-hook 'after-init-hook 'mike-init-sql)

;; ** MySQL

(add-to-list 'auto-mode-alist '("\\.mysql\\'" . sql-mode))

;; ** Postgres

(provide 'mike-sql)
