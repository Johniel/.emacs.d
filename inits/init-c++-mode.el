;;
;;

(require 'util)

(def-face c++-macro "cyan1" "C++ macro form")
(add-keywords 'c++-mode '("each" "unless") 'c++-macro)

(add-hook 'c++-mode-hook
          '(lambda()
             (local-set-key (kbd "C-x C-e") nil)
             (local-set-key (kbd "C-x C-a") nil)
             (local-set-key (kbd "C-c C-k") nil)
             (c-set-style "ellemtel")
             (c-toggle-electric-state +1)
             (setq c-basic-offset 2)
             (setq tab-width 2)))

(provide 'init-c++-mode)
