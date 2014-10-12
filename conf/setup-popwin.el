(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 25)

(push "*Backtrace*" popwin:special-display-config)
(push "*helm mini*" popwin:special-display-config)

(require 'popwin-browse-kill-ring)

(provide 'setup-popwin)
