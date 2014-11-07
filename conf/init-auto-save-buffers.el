(require 'auto-save-buffers)

(run-with-idle-timer 5 t 'auto-save-buffers)

(provide 'init-auto-save-buffers)
