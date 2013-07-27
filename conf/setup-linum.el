(require 'linum)

(setq linum-format "%4d.".)

(set-face-foreground 'linum "#7a7a7a")
(set-face-background 'linum "#2D0922")

(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

(provide 'setup-linum)
