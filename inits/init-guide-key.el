;;; https://github.com/kbkbkbkb1/guide-key

(require 'guide-key)

(setq guide-key/popup-window-position 'bottom)

(defun guide-key/my-hook-function-for-howm-mode ()
  (guide-key-mode 1)
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c ,")
  (guide-key/add-local-highlight-command-regexp "howm-"))
(add-hook 'howm-mode-hook 'guide-key/my-hook-function-for-howm-mode)

(setq guide-key/guide-key-sequence '("C-x r" "C-c o"))
(guide-key-mode 1)

(provide 'init-guide-key)
