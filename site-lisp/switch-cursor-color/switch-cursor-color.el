(require 'dash)
(require 'tramp)

(defvar scc-default-color "purple2")

(setq scc-color-detector '())

(add-to-list 'scc-color-detector '(lambda () scc-default-color))

(defun scc-get-cursor-color ()
  (-first 'identity (--map (funcall it) scc-color-detector)))

(defadvice other-window (after switch-cursor-color activate)
  (set-cursor-color (scc-get-cursor-color))
  ad-return-value)

(defun get-tramp-hostname ()
  (let ((name (buffer-file-name)))
    (if (and name (tramp-tramp-file-p name))
        (tramp-file-name-real-host (tramp-dissect-file-name name)))))

(add-to-list 'scc-color-detector '(lambda ()
                                    (let ((hostname (get-tramp-hostname)))
                                      (cond ((member hostname '()) "red")
                                            ((member hostname '())  "blue")
                                            (t nil)))))

(provide 'scc)
