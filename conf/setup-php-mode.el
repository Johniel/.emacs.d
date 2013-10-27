(require 'php-mode)
(require 'php-eldoc)

(add-hook 'php-mode-hook 'php-eldoc-enable)

(add-hook 'php-mode-hook (lambda ()
                           (setq indent-tabs-mode nil)
                           (setq tab-width 4)
                           (setq c-basic-offset 4)))

(provide 'setup-php-mode)
