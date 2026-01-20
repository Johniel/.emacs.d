;;; init-helm.el

(require 'cl-lib)

;; Workaround for helm-regexp.el bug: helm-source-occur is referenced before definition
(defvar helm-source-occur nil)

(require 'helm)
(require 'helm-command)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-ls-git)

(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

(setq helm-mini-default-sources (append '(helm-source-recentf)
                                        (if (windows-p)
                                            '()
                                          '(helm-source-ls-git-status
                                            helm-source-ls-git))))

;; 現在のウィンドウ内で分割（フレーム全体ではなく）
(setq helm-split-window-inside-p t)
;; 下方向に分割（カーソル位置を上部に維持）
(setq helm-split-window-default-side 'below)
;; 高さをウィンドウの50%に設定
(setq helm-display-buffer-default-height 0.5)

;;;

(provide 'init-helm)
