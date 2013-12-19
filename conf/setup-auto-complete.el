;; http://cx4a.org/software/auto-complete/manual.ja.html

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(custom-set-variables '(ac-ignore-case nil))

(setq ac-use-comphist nil)
(setq ac-auto-start t)
(setq ac-delay 0.0)
(setq popup-use-optimized-column-computation nil)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help nil)

(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") nil)
(define-key ac-menu-map (kbd "C-p") nil)

(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)

(set-face-background 'ac-candidate-face "gray30")
(set-face-underline  'ac-candidate-face "gray50")
(set-face-background 'ac-selection-face "RoyalBlue4")

(defadvice ac-inline-show (around my-ac-conf activate)
  "Call ac-inline-show, if the cursor is in end of line."
  (if (eolp) ad-do-it nil))

(unless (performance-saving-p) (global-auto-complete-mode t))

(make-local-variable 'ac-auto-start)

(provide 'setup-auto-complete)
