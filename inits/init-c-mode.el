;;
;;

(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "ellemtel")
             (c-toggle-electric-state +1)
             (setq c-basic-offset 2)
             (setq tab-width 2)))

(provide 'init-c-mode)
