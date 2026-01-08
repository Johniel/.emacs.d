(require 'browse-kill-ring)
(require 'util)
(require 'cl-lib)

(browse-kill-ring-default-keybindings)

(add-hook-fn 'browse-kill-ring-hook
             (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
             (define-key browse-kill-ring-mode-map (kbd "h") 'browse-kill-ring-previous))

(setq browse-kill-ring-separator (concat "\n" (make-string 100 ?\\)))
(setq browse-kill-ring-highlight-current-entry t)

(defface my-browse-kill-ring-current-entry-face
  '((t (:background "dark green" :highlight t))) nil)

;; from ver. 1.4
(defun my-browse-kill-ring-forward (&optional arg)
  "Move forward by ARG `kill-ring' entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (if (< arg 0)
        (progn
          (cl-incf arg)
          (if (overlays-at (point))
              (progn
                (goto-char (overlay-start (car (overlays-at (point)))))
                (goto-char (previous-overlay-change (point)))
                (goto-char (previous-overlay-change (point))))
            (progn
              (goto-char (1- (previous-overlay-change (point))))
              (unless (bobp)
                (goto-char (overlay-start (car (overlays-at (point)))))))))
      (progn
        (cl-decf arg)
        (if (overlays-at (point))
            (progn
              (goto-char (overlay-end (car (overlays-at (point)))))
              (goto-char (next-overlay-change (point))))
          (goto-char (next-overlay-change (point)))
          (unless (eobp)
            (goto-char (overlay-start (car (overlays-at (point))))))))))
  ;; This could probably be implemented in a more intelligent manner.
  ;; Perhaps keep track over the overlay we started from?  That would
  ;; break when the user moved manually, though.
  (when browse-kill-ring-highlight-current-entry
    (let ((overs (overlay-lists))
          (current-overlay (car (overlays-at (point)))))
      (mapcar #'(lambda (o)
                  (overlay-put o 'face nil))
              (nconc (car overs) (cdr overs)))
      (overlay-put current-overlay 'face 'my-browse-kill-ring-current-entry-face)))
  (when browse-kill-ring-recenter
    (recenter 1)))

(add-hook 'browse-kill-ring-mode-hook
          (lambda ()
            (setq-local hl-line-range-function
                        (lambda () '(0 . 0)))))

(advice-add 'browse-kill-ring-forward :override
            (lambda (&optional arg)
              (interactive "p")
              (my-browse-kill-ring-forward arg)))

(provide 'init-browse-kill-ring)
