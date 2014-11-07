(require 'sequential-command)
(require 'sequential-command-config)

(sequential-command-setup-keys)

(define-sequential-command seq-home
;;  back-to-indentation
  beginning-of-line
  beginning-of-buffer
  seq-return)

(global-set-key "\C-a" 'seq-home)

(provide 'init-sequential-command)
