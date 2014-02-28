;;; setup-helm.el

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-command)
(require 'helm-ls-git)
(require 'ac-helm)

;;

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

;;

(require 'tabbar+)
(setq helm-mini-default-sources '(helm-c-source-tabbar+current-group-buffers-list
                                  helm-c-source-tab-groups-list
                                  helm-c-source-recentf
                                  helm-source-ls-git-status
                                  helm-source-ls-git))

(require 'popwin)
(push "*helm*" popwin:special-display-config)

;;
;; http://d.hatena.ne.jp/syohex/20121207/1354885367
(require 'yasnippet)
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  choice)))
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

;;

(defun helm-etags-insert-action-php (candidate)
  (insert (let* ((split (helm-etags-split-line candidate))
                 (name (s-split " " (car (cdr split)))))
            (cond ((string= (car name) "function") (cadr name))
                  ((string= (car name) "class") (->> (car split)
                                                  (s-split "/")
                                                  (--last (string-match "\\.php\\'" it))
                                                  (s-chop-suffixes '(".php" ".class"))))
                  (t "ERROR")))))

(defvar helm-source-etags-select-php
  `((name . "Etags")
    (header-name . helm-etags-get-header-name)
    (init . helm-etags-init)
    (candidates-in-buffer)
    (match-part . (lambda (candidate)
                    ;; Match only the tag part of CANDIDATE
                    ;; and not the filename.
                    (if helm-etags-match-part-only
                        (cadr (helm-etags-split-line candidate))
                        candidate)))
    (mode-line . helm-etags-mode-line-string)
    (keymap . ,helm-etags-map)
    (action ("Tag Jump" . helm-etags-default-action)
            ("Insert" . helm-etags-insert-action))
    (persistent-action . (lambda (candidate)
                           (helm-etags-default-action candidate)
                           (helm-highlight-current-line))))
  "Helm source for Etags.")

(defun helm-etags-select-php (arg)
  "Preconfigured helm for etags.
If called with a prefix argument or if any of the tag files have
been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories, by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
  (interactive "P")
  (let ((tag-files (helm-etags-all-tag-files))
        (helm-execute-action-at-once-if-one helm-etags-execute-action-at-once-if-one)
        (str (thing-at-point 'symbol)))
    (if (cl-notany 'file-exists-p tag-files)
        (message "Error: No tag file found. Create with etags shell command, or visit with `find-tag' or `visit-tags-table'.")
      (cl-loop for k being the hash-keys of helm-etags-cache
               unless (member k tag-files)
               do (remhash k helm-etags-cache))
      (mapc (lambda (f)
              (when (or (equal arg '(4))
                        (and helm-etags-mtime-alist
                             (helm-etags-file-modified-p f)))
                (remhash f helm-etags-cache)))
            tag-files)
      (helm :sources 'helm-source-etags-select-php
            :keymap helm-etags-map
            :default (list (concat "\\_<" str "\\_>") str)
            :buffer "*helm etags*"))))

;;;

(provide 'setup-helm)
