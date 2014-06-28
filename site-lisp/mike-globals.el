;; * GLOBAL SETTINGS

;; Backups

;; ** GLOBAL MODES




(defun mike-init-global-keybindings ()
  (global-set-key (kbd "C-c g") 'magit-status))


(defun mike-init-globals ()
  (mike-init-global-settings)
  (mike-init-global-keybindings)



(provide 'mike-globals)
