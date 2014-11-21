(require 'bm)
(require 'dash)
(require 'helm-swoop)
(require 's)

(defconst helm-swoop-bm-buffer-name "*helm-swoop-bm*")

(defun helm-c-source-swoop-bm-fn ()
  `((name . ,(buffer-name (current-buffer)))
    (candidates . ,(->> (-flatten (bm-lists))
                     (-filter 'overlayp)
                     (--map (let* ((line (buffer-substring (overlay-start it)
                                                           (overlay-end it)))
                                   (line-num (count-lines (point-min)
                                                          (overlay-start it))))
                              (s-trim (concat (format "%d" line-num) " " line))))))
    (keymap . ,helm-swoop-map)
    (header-line . "[C-c C-e] Edit mode, [M-i] apply all buffers")
    (action . (lambda ($line)
                (helm-swoop--goto-line
                 (when (string-match "^[0-9]+" $line)
                   (string-to-number (match-string 0 $line))))
                (let (($regex (mapconcat 'identity
                                         (split-string helm-pattern " ")
                                         "\\|")))
                  (when (or (and (and (featurep 'migemo) (featurep 'helm-migemo))
                                 (migemo-forward $regex nil t))
                            (re-search-forward $regex nil t))
                    (goto-char (match-beginning 0))))
                (helm-swoop--recenter)))))

(defun helm-swoop-bm ()
  (interactive)
  (let ((helm-display-function helm-swoop-split-window-function)
        (helm-display-source-at-screen-top nil)
        (helm-completion-window-scroll-margin 5))
    (helm :sources (helm-c-source-swoop-bm-fn)
          :buffer helm-swoop-bm-buffer-name
          :prompt helm-swoop-prompt
          :preselect
          (if (string-match "^[\t\n\s]*$" (helm-swoop--get-string-at-line))
              (save-excursion
                (if (re-search-forward "[^\t\n\s]" nil t)
                    (format "^%s\s" (line-number-at-pos))
                  (re-search-backward "[^\t\n\s]" nil t)
                  (format "^%s\s" (line-number-at-pos))))
            (format "^%s\s" (line-number-at-pos)))
          :candidate-number-limit helm-swoop-candidate-number-limit)))

(provide 'helm-bm)
