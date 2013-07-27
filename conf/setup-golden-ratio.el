(require 'golden-ratio)

(defun other-window-or-split-golden-ratio ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1)
  (golden-ratio))

(defun enable-global-golden-ratio ()
  (interactive)
  (global-set-key "\C-t" 'other-window-or-split-golden-ratio))

(provide 'setup-golden-ratio)
