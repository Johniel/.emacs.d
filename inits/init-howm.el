;; howm
;; http://www.bookshelf.jp/soft/meadow_38.html#SEC563
(require 'howm)

(setq howm-directory "~/Dropbox/howm/")
(setq howm-menu-lang 'ja)

(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))

;; (eval-after-load "howm-mode"
;;   '(progn
;;      (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
;;      (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))

(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))

(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-c C-c
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))

(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

(add-to-list 'auto-mode-alist '("\\.howm$" . markdown-mode))
;; (define-key howm-menu-mode-map [tab] nil)
(setq action-lock-goto-next-link nil)
(defalias 'hm 'howm-menu)

(provide 'init-howm)
