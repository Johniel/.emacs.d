(require 'auto-save-buffers)

(run-with-idle-timer 5 t 'auto-save-buffers)

(provide 'setup-auto-save-buffers)
