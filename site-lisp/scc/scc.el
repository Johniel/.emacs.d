(require 'dash)
(require 'tramp)

(defvar scc-default-color "purple2")

(defvar scc-color-detector '())

(add-to-list 'scc-color-detector '(lambda () scc-default-color))

(defun scc-get-cursor-color ()
  (-first 'identity (--map (funcall it) scc-color-detector)))

(defadvice other-window (after scc-other-window disable)
  (set-cursor-color (scc-get-cursor-color))
  ad-return-value)

(defadvice switch-to-buffer (after scc-switch-to-buffer disable)
  (set-cursor-color (scc-get-cursor-color))
  ad-return-value)

(defun get-tramp-hostname ()
  (let ((name (buffer-file-name)))
    (if (and name (tramp-tramp-file-p name))
        (tramp-file-name-real-host (tramp-dissect-file-name name)))))

(add-to-list 'scc-color-detector '(lambda ()
                                    (let ((hostname (get-tramp-hostname)))
                                      (cond ((member hostname '()) "red")
                                            ((member hostname '())  "deep sky blue")
                                            (t nil)))))

;;;###autoload
(define-minor-mode scc-mode
  ""
  :group      'scc
  :init-value nil
  :global     t
  :lighter    scc-mode
  (progn (if scc-mode
             (progn (ad-enable-advice 'other-window     'after 'scc-other-window)
                    (ad-enable-advice 'switch-to-buffer 'after 'scc-switch-to-buffer))
           (progn (ad-disable-advice 'other-window     'after 'scc-other-window)
                  (ad-disable-advice 'switch-to-buffer 'after 'scc-switch-to-buffer)
                  (set-cursor-color scc-default-color)))
         (ad-activate 'other-window)
         (ad-activate 'switch-to-buffer)))

(defun scc--turn-on ()
  (scc-mode +1))

;;;###autoload
(define-globalized-minor-mode global-scc-mode scc-mode scc--turn-on
  :group 'scc)

(provide 'scc)
