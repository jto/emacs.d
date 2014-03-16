;; * BUFFER UTILS

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

(defun mike-clean-buffer ()
  "Untabify and strip trailing whitespace in current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))))

(global-set-key (kbd "C-c .") 'mike-clean-buffer)

(defun mike-rename-buffer-file ()
  "Renames current buffer and file it is visiting.

From URL `http://whattheemacsd.com/'."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'mike-rename-buffer-file)

(defun mike-last-mode-buffer (mode l)
  "Get latest buffer with major-mode MODE in buffer list L."
  (when l
    (if (eq mode (with-current-buffer (car l) major-mode))
        (car l)
      (mike-last-mode-buffer mode (cdr l)))))

(defun mike-get-mode-buffer (mode f)
  "Pop to or create a buffer with major-mode MODE.

Create with creation function F."
  (let ((b (mike-last-mode-buffer mode (buffer-list))))
    (if (or (not b) (eq mode major-mode))
        (funcall f)
      (let ((vis (get-buffer-window-list b)))
        (if vis
            (pop-to-buffer b 'display-buffer-reuse-window)
          (switch-to-buffer b))))))

(defun mike-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
  logical line.  This is useful, e.g., for use with
  `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(provide 'mike-buffer-utils)
