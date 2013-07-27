;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-indentation.el
;; https://github.com/antonj/Highlight-Indentation-for-Emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-indentation)

;; (add-hook 'highlight-indentation-mode-hook
;;           'highlight-indentation-current-column-mode)

(set-face-background 'highlight-indentation-face "#1a1a1a")
(set-face-background 'highlight-indentation-current-column-face "#2a2040")

(provide 'setup-highlight-indentation)
