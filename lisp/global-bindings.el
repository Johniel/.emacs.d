(require 'util)
(require 'commands)

(keyboard-translate ?\C-\[ ?\C-m)
(keyboard-translate ?\C-m ?\C-\[)
(keyboard-translate ?\C-i ?\C-\])
(keyboard-translate ?\C-\] ?\C-i)

;; redo+
(global-set-key (kbd "C-.") 'redo)

;; killing
(global-set-key (kbd "C-c C-k") 'kill-ring-save)
(global-set-key (kbd "C-w") 'kill-word-or-kill-region)

;; helm
(global-set-key (kbd "C-_") 'helm-mini)
(global-set-key (kbd "C-S-x") 'helm-M-x)
(global-set-key (kbd "C-c C-SPC") 'helm-imenu)
(global-set-key (kbd "C-\\") 'helm-ls-git-ls)
(global-set-key (kbd "C-c SPC") 'helm-lisp-completion-at-point)
(global-set-key (kbd "C-=") 'helm-swoop)
(global-set-key (kbd "C-0") 'helm-mark-ring)

;;
(global-set-key (kbd "C-|") 'yas-insert-snippet)

;;
(global-set-key (kbd "C-c ;") 'comment-dwim)

;;
(global-set-key (kbd "C-S-r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;;
(global-set-key (kbd "C-c C-y") 'browse-kill-ring)

;;
(global-set-key (kbd "C-S-y") 'yank-unindented)

;;
(global-set-key (kbd "C-x b")   'list-buffers)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; magit
(global-set-key (kbd "C-x m") 'magit-status)

;;
(global-set-key (kbd "C-f") 'hippie-expand)

;; cua-mode
(define-key cua-global-keymap (kbd "C-<return>") nil)
(define-key cua-global-keymap (kbd "C-S-<return>") 'cua-set-rectangle-mark)

;; cursor
(global-set-key (kbd "C-c C-g") 'goto-line)
(global-set-key (kbd "C-h") 'previous-line)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-t") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-u") 'forward-word)
(global-set-key (kbd "C--") 'backward-word)
;;
(global-set-key (kbd "C-M-t") 'forward-list)
(global-set-key (kbd "C-M-b") 'backward-list)
;;
(global-set-key (kbd "C-c C-h") 'avy-goto-line)
(global-set-key (kbd "C-c C-t") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-r") 'avy-goto-word-0)
(global-set-key (kbd "C-c C-.") #'(lambda () (interactive) (avy-goto-char ?\n)))

;;
(global-set-key (kbd "C-S-n") 'point-undo)
(global-set-key (kbd "C-S-h") 'point-redo)
;;
(global-set-key (kbd "C-9") 'insert-parentheses)

(global-set-key (kbd "C-S-l") 'highlight-symbol-at-point)

;;
(global-set-key (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-p") 'delete-backward-char)

;;
(global-set-key (kbd "M-s o") 'isearch-occur)

;;
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;;
(global-set-key (kbd "C-x <tab>") 'indent-region)

;;
(global-set-key (kbd "C-x C-h") 'mark-whole-buffer)

;;
(global-set-key (kbd "C-;") 'universal-argument)
(global-set-key (kbd "C-]") 'other-window-or-split) ; C-] <-> C-i
(global-set-key (kbd "C-M-]") 'kill-buffer-and-window) ; C-] <-> C-i
(global-set-key [f2] 'swap-screen)
(global-set-key [C-f2] 'swap-screen-with-cursor)
(global-set-key (kbd "C-S-i") 'swap-screen)

;; scroll
(global-set-key (kbd "C-'") 'scroll-down)
(global-set-key (kbd "C-,") 'scroll-up-command)
(global-set-key (kbd "C-v") 'scroll-up-command)

;; expand-region
(global-set-key (kbd "C-8") 'er/expand-region)
(global-set-key (kbd "C-7") 'er/contract-region)

;; multiple-cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

;; flymake
(global-set-key (kbd "C-M-h") 'flymake-goto-prev-error)
(global-set-key (kbd "C-M-n") 'flymake-goto-next-error)

;;
(global-set-key (kbd "C-c C-8") 'join-line)
(global-set-key (kbd "<S-return>") #'(lambda ()
                                       (interactive)
                                       (move-end-of-line nil)
                                       (newline-and-indent)))
(global-set-key (kbd "C-o") #'(lambda ()
                                (interactive)
                                (move-beginning-of-line 1)
                                (open-line 1)
                                (indent-according-to-mode)))

(require 'cc-cmds)
(global-set-key (kbd "<S-backspace>") 'c-hungry-backspace)

(global-set-key (kbd "C-M-z") 'zap-to-char)

(global-set-key (kbd "M-SPC") 'just-one-space)

;; all
(global-set-key (kbd "C-M-p") 'transpose-words)

;; font size
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;; tabbar
(global-set-key [f9] 'tabbar-mode)
(global-set-key (kbd "C-S-t") 'tabbar-forward)
(global-set-key (kbd "C-S-b") 'tabbar-backward)
(global-set-key (kbd "C-<left>")  'tabbar+move-left)
(global-set-key (kbd "C-<right>") 'tabbar+move-right)
(global-set-key (kbd "C-<next>")  'tabbar-forward-group)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-group)

;;
(global-set-key (kbd "C-S-r") 'revert-buffer)
(global-set-key (kbd "C-z") #'(lambda () (interactive)
                                (switch-to-buffer "*scratch*")))

;;
(global-set-key (kbd "M-o") 'case-convert-at-point)

(global-set-key (kbd "C-6") 'shell-pop)

;;
(global-set-key (kbd "C-x C-d") 'dired)

;;
(global-set-key (kbd "C-S-g") 'toggle-debug-on-quit)

;; dmacro
(defconst *dmacro-key* (kbd "C-1"))
(require 'dmacro)
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

(when (require 'mozc nil t)
  (global-set-key (kbd "C-x C-j") 'mozc-mode))

(global-set-key (kbd "C-3") 'switch-to-previous-buffer)
