(require 'php-mode)
(require 'php-eldoc)

(add-hook 'php-mode-hook 'php-eldoc-enable)

(provide 'setup-php-mode)
