;;; setup-ace-jump-mode.el ---

(require 'ace-jump-mode)

(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode
        ace-jump-char-mode
        ace-jump-line-mode))

(setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i))

;; https://github.com/magnars/.emacs.d/blob/master/setup-ace-jump-mode.el
(defun define-ace-jump-char-mode-for-all (c)
  (global-set-key (read-kbd-macro (concat "C-M-g " (string c)))
    `(lambda ()
       (interactive)
       (setq ace-jump-query-char ,c)
       (setq ace-jump-current-mode 'ace-jump-word-mode)
       (ace-jump-do (concat "\\b"
                            (regexp-quote (make-string 1 ,c)))))))

;; (loop for c from ?0 to ?9 do (define-ace-jump-char-mode-for-all c))
;; (loop for c from ?A to ?Z do (define-ace-jump-char-mode-for-all c))
;; (loop for c from ?a to ?z do (define-ace-jump-char-mode-for-all c))

(provide 'setup-ace-jump-mode)
