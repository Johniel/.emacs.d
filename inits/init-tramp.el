(require 'tramp)
(require 'vagrant-tramp)

(setq tramp-default-method "ssh")
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(eval-after-load 'tramp '(vagrant-tramp-enable))

(provide 'init-tramp)
