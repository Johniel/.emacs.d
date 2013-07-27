;;; https://github.com/kbkbkbkb1/guide-key

(require 'guide-key)

(setq guide-key/popup-window-position 'bottom)

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key-mode 1)
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(defun guide-key/my-hook-function-for-howm-mode ()
  (guide-key-mode 1)
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c ,")
  (guide-key/add-local-highlight-command-regexp "howm-"))
(add-hook 'howm-mode-hook 'guide-key/my-hook-function-for-howm-mode)

(provide 'setup-guide-key)
