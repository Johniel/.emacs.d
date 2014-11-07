(require 'lua-mode)
(require 'lua-eldoc-mode)

(setq lua-indent-level 2)
(add-hook 'lua-mode-hook 'lua-eldoc-mode)

(provide 'init-lua-mode)
