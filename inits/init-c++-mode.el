;;
;;

(require 'util)

(def-face c++-macro "cyan1" "C++ macro form")
(add-keywords 'c++-mode '("each" "unless") 'c++-macro)

(add-hook 'c++-mode-hook
          '(lambda()
             (local-set-key (kbd "C-x C-e") 'eval-and-replace)
             (local-set-key (kbd "C-x C-a") nil)
             (c-set-style "ellemtel")
             (c-toggle-electric-state +1)
             (setq c-basic-offset 2)
             (setq tab-width 2)))

;;

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'c++-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-dictionary
                                ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers))))

;;
(require 'aggressive-indent)
(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

;;;

(provide 'init-c++-mode)
