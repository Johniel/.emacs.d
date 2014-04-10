(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 18)

(push "*Backtrace*" popwin:special-display-config)
(push "*helm mini*" popwin:special-display-config)

(require 'popwin-browse-kill-ring)

(provide 'setup-popwin)
