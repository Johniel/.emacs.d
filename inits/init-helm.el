;;; init-helm.el

(require 'cl-lib)

;; Workaround for helm-regexp.el bug: helm-source-occur is referenced before definition
(defvar helm-source-occur nil)

(require 'helm)
(require 'helm-command)
;; (require 'helm-config)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-ls-git)
;; helm-occur is autoloaded when helm-occur command is called

;;

(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

;; (define-key company-mode-map (kbd "C-S-F") 'helm-company)

;;

(setq helm-mini-default-sources (append '(helm-source-recentf)
                                        (if (windows-p)
                                            '()
                                          '(helm-source-ls-git-status
                                            helm-source-ls-git))))

(require 'popwin)
(push "*helm*" popwin:special-display-config)

;;
;; http://d.hatena.ne.jp/syohex/20121207/1354885367
(require 'yasnippet)
(defun my-yas-prompt (prompt choices &optional display-fn)
  (let* ((names (cl-loop for choice in choices
                         collect (or (and display-fn (funcall display-fn choice))
                                     choice)))
         (selected (helm-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*helm yas-prompt*")))
    (if selected
        (let ((n (cl-position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))

(setq yas-prompt-functions '(my-yas-prompt))

;;;

(provide 'init-helm)
