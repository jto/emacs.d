(require 'eshell)
(require 'em-hist)
(require 'eshell-git)

(setq eshell-history-size 1024
      eshell-prompt-regexp "^[^#$]*[#$] "
      eshell-save-history-on-exit t
      eshell-highlight-prompt nil
      eshell-git/prompt-face 'default
      eshell-git/pwd-face 'default
      eshell-git/branch-face 'default)

(setq eshell-prompt-function 'eshell-git/prompt-function)
