;;; setup-helm.el

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-command)

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

(require 'tabbar+)
(defun my-standard-helm ()
  (interactive)
  (helm :sources '(
                   ;; helm-c-source-buffers-list
                   ;; helm-c-source-tabbar+buffers-list
                   helm-c-source-tabbar+current-group-buffers-list
                   helm-c-source-tab-groups-list
                   ;; helm-c-source-imenu
                   helm-c-source-recentf
                   ;; helm-c-source-buffer-not-found
    )
        :buffer "*helm*"
        :keymap helm-c-buffer-map))

(require 'popwin)
(push "*helm*" popwin:special-display-config)

;;
;; http://d.hatena.ne.jp/syohex/20121207/1354885367
(require 'yasnippet)
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  coice)))
         (selected (helm-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*helm yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))
(custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

(provide 'setup-helm)
