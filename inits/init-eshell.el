;;; init-eshell.el ---

(require 'eshell)

(setq eshell-modules-list '(
                            eshell-banner
                            eshell-basic
                            eshell-cmpl
                            eshell-dirs
                            eshell-glob
                            eshell-hist
                            eshell-ls
                            eshell-pred
                            eshell-prompt
                            eshell-script
                            eshell-term
                            eshell-alias
                            ))

(setq eshell-banner-message "")
(setq eshell-ls-initial-args "-h")
(setq eshell-cmpl-ignore-case t)
(setq eshell-hist-ignoredups t)
;; Prompt
(setq eshell-prompt-function
      '(lambda ()
         (let ((user (or (getenv "USER") user-login-name "ted"))
               (host (car (split-string (or (getenv "HOST")
                                            system-name
                                            "unknown")
                                        "\\."))))
           (concat ".[" user "@" host "]% "))))

(setq eshell-prompt-regexp "^[^#%]*[%#] ")

(eval-after-load "em-alias"
  '(progn (eshell/alias "ll" "ls -lhF")
          (eshell/alias "la" "ls -aF")
          (eshell/alias "lla" "ls -alhF")
          (eshell/alias "cd" "cd $1; ls")))

;;
(add-hook 'eshell-mode-hook
          '(lambda ()
             (set-face-foreground 'eshell-prompt "RoyalBlue")))

;; http://djlab.sakura.ne.jp/mydiary/?p=401
(add-hook 'set-language-environment-hook
          (lambda ()
            (when (equal "ja_JP.UTF-8" (getenv "LANG"))
              (setq default-process-coding-system '(utf-8 . utf-8))
              (setq default-file-name-coding-system 'utf-8))
            (when (equal "Japanese" current-language-environment)
              (setq default-buffer-file-coding-system 'euc-jp))))
(set-language-environment "Japanese")

;; complete after 'sudo'
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-help "complete after sudo"))
    (pcomplete-here (pcomplete-here (eshell-complete-commands-list)))))

;; clear
;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9#content_1_46
(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; less
;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; From: http://www.emacswiki.org/cgi-bin/wiki.pl/EshellEnhancedLS
;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9
(eval-after-load "em-ls"
  '(progn
     ;; (defun ted-eshell-ls-find-file-at-point (point)
     ;;          "RET on Eshell's `ls' output to open files."
     ;;          (interactive "d")
     ;;          (find-file (buffer-substring-no-properties
     ;;                      (previous-single-property-change point 'help-echo)
     ;;                      (next-single-property-change point 'help-echo))))
     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))
     (defun ted-eshell-ls-find-file ()
       (interactive)
       (let ((fname (buffer-substring-no-properties
                     (previous-single-property-change (point) 'help-echo)
                     (next-single-property-change (point) 'help-echo))))
         ;; Remove any leading whitespace, including newline that might
         ;; be fetched by buffer-substring-no-properties
         (setq fname (replace-regexp-in-string "^[ \t\n]*" "" fname))
         ;; Same for trailing whitespace and newline
         (setq fname (replace-regexp-in-string "[ \t\n]*$" "" fname))
         (cond
          ((equal "" fname)
           (message "No file name found at point"))
          (fname
           (find-file fname)))))
     (let ((map (make-sparse-keymap)))
       ;;          (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       ;;          (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))
     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

;; http://whattheemacsd.com/setup-shell.el-01.html
;;
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "M-h") 'eshell-next-input)
            (local-set-key (kbd "M-n") 'eshell-previous-input)))

;;;
(provide 'init-eshell)
