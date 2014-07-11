(setq x-underline-at-descent-line t
      solarized-distinct-fringe-background t
      solarized-high-contrast-mode-line t)

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'solarized-light)

(provide 'mike-solarized)
