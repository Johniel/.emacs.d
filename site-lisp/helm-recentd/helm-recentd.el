(require 'helm)
(require 'dash)
(require 'f)

(defun recentd-list ()
  (->> recentf-list
    (-map 'f-dirname)
    (-filter 'file-exists-p)))

(defvar helm-c-source-recentd
  `((name . "Recentd")
    (init . (lambda ()
              (require 'recentf)
              (recentf-mode 1)))
    (candidates . recentd-list)
    (volatile)
    (action ("cd" . cd))))

;;;###autoload
(defun helm-recentd ()
  (interactive)
  (helm :sources 'helm-c-source-recentd
        :buffer "*helm*"
        :keymap helm-c-buffer-map))

(provide 'helm-recentd)
