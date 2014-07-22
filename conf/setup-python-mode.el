(require 'jedi)

(add-hook 'pytho-mode-hook 'jedi::setup)
(setq jedi:tooltip-method 'nil)

(provide 'setup-python-mode)
