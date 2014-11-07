;;
;;

(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "ellemtel")
             (c-toggle-electric-state +1)
             (setq c-basic-offset 2)
             (setq tab-width 2)))

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'c-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-dictionary
                                ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers))))

;;;

(provide 'init-c-mode)
