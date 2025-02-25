;;; init-flymake.el ---

(require 'flymake)
(require 'flymake-proc)

;;
(setq flymake-start-syntax-check-on-newline nil)

;; -1 = NONE,
;;  0 = ERROR,
;;  1 = WARNING,
;;  2 = INFO,
;;  3 = DEBUG
(setq flymake-log-level -1)

;; display flymake error in minibuffer
;; https://gist.github.com/415429
(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

;;
;; C++
(defun flymake-cpp-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-fsyntax-only" "-std=c++20" local-file))))

(push '("\\.cpp$" flymake-cpp-init) flymake-proc-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cpp-init) flymake-proc-allowed-file-name-masks)

(add-hook 'c++-mode-hook '(lambda() (flymake-mode t)))

;;
;; C
(defun flymake-c-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "gcc" (list "-Wall" "-fsyntax-only" local-file))))

(push '("\\.c$" flymake-c-init) flymake-proc-allowed-file-name-masks)
(push '("\\.h$" flymake-c-init) flymake-proc-allowed-file-name-masks)

(add-hook 'c-mode-hook '(lambda() (flymake-mode t)))

;; Emacs Lisp
;; http://www.lunaport.net/blog/2010/02/windowsflymake-elisp-1.html
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval"
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (emacs-lisp-mode)
              (let ((parse-sexp-ignore-comments t))
                (condition-case data
                    (scan-sexps (point-min) (point-max))
                  (scan-error
                   (goto-char(nth 2 data))
                   (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                  file (line-number-at-pos))))))))))
        local-file)))))
(push '("\\.el$" flymake-elisp-init) flymake-proc-allowed-file-name-masks)
(add-hook 'emacs-lisp-mode-hook
          ;; workaround for (eq buffer-file-name nil)
          (function (lambda () (if buffer-file-name (flymake-mode)))))

;; buffer has a running process: kill it?
;; http://stackoverflow.com/questions/7299893/getting-rid-of-buffer-has-running-process-confirmation-when-the-process-is-a-f
(defadvice flymake-start-syntax-check-process (after
                                               cheeso-advice-flymake-start-syntax-check-1
                                               (cmd args dir)
                                               activate compile)
  ;; set flag to allow exit without query on any
  ;;active flymake processes
  (set-process-query-on-exit-flag ad-return-value nil))


(provide 'init-flymake)
