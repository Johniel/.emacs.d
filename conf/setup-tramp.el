(require 'tramp)

(setq tramp-default-method "ssh")
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(provide 'setup-tramp)
