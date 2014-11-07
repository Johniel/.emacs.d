;; http://qiita.com/yewton@github/items/d9e686d2f2a092321e34
(require 'gtags)
(require 'helm)
(require 'helm-gtags)

(defun update-gtags (&optional prefix)
  (interactive "P")
  (let ((rootdir (gtags-get-rootpath))
        (args (if prefix "-v" "-iv")))
    (when rootdir
      (let* ((default-directory rootdir)
             (buffer (get-buffer-create "*update GTAGS*")))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer)
          (let ((result (process-file "gtags" nil buffer nil args)))
            (if (= 0 result)
                (message "GTAGS successfully updated.")
              (message "update GTAGS error with exit status %d" result))))))))

(defun gtags-parse-file2 ()
  (interactive)
  (if (gtags-get-rootpath)
      (let*
          ((root (gtags-get-rootpath))
           (path (buffer-file-name))
           (gtags-path-style 'root)
           (gtags-rootdir root))
        (if (string-match (regexp-quote root) path)
            (gtags-goto-tag
             (replace-match "" t nil path)
             "f" nil)
          ;; delegate to gtags-parse-file
          (gtags-parse-file)))
    ;; delegate to gtags-parse-file
    (gtags-parse-file)))

;; get the path of gtags root directory.
(defun gtags-get-rootpath ()
  (let (path buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (setq n (process-file "global" nil t nil "-pr"))
      (if (= n 0)
          (setq path (file-name-as-directory (buffer-substring (point-min)(1- (point-max))))))
      (kill-buffer buffer))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p default-directory))
        (with-parsed-tramp-file-name default-directory tr
          (when path
            (concat (substring default-directory 0 (* -1 (length tr-localname)))
                    path)))
      path)))

(defun make-gtags ()
  (interactive)
  (if (= 0 (shell-command "gtags --version"))
      (progn (add-hook 'after-save-hook 'update-gtags)
             (setq gtags-prefix-key "\C-c")
             (setq gtags-mode-hook
                   '(lambda ()
                      (define-key gtags-mode-map "\C-cs" 'gtags-find-symbol)
                      (define-key gtags-mode-map "\C-cr" 'gtags-find-rtag)
                      (define-key gtags-mode-map "\C-ct" 'gtags-find-tag)
                      (define-key gtags-mode-map "\C-cf" 'gtags-parse-file)))
             (add-hook 'c-mode-common-hook
                       '(lambda()
                          (gtags-mode 1))))
    (message "[setup-gtags]: not found command gtags")))

(defun my-helm-gtags ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags)))

(add-hook 'helm-gtags-mode-hook '(lambda() (local-set-key (kbd "C-M-p") 'my-helm-gtags)))

 ;; helm-source-gtags-files
 ;; helm-source-gtags-gsyms
 ;; helm-source-gtags-parse-file
 ;; helm-source-gtags-rtags
 ;; helm-source-gtags-select
 ;; helm-source-gtags-show-stack
 ;; helm-source-gtags-tags

(provide 'init-gtags)
