(require 'popwin)

(popwin-mode 1)
(setq popwin:popup-window-height 25)

(push "*Backtrace*" popwin:special-display-config)

(require 'popwin-browse-kill-ring)

(provide 'init-popwin)
