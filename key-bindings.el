;;; key-bindings.el

;;  _____      _            ______                      _
;; |  ___|    (_)           |  _  \                    | |
;; | |__ _ __  _  ___  _   _| | | |_   _____  _ __ __ _| | __
;; |  __| '_ \| |/ _ \| | | | | | \ \ / / _ \| '__/ _` | |/ /
;; | |__| | | | | (_) | |_| | |/ / \ V / (_) | | | (_| |   <
;; \____/_| |_| |\___/ \__, |___/   \_/ \___/|_|  \__,_|_|\_\
;;           _/ |       __/ |
;;          |__/       |___/

(require 'util)
(require 'commands)

(defun call-either (condition command-a command-b)
  `(lambda ()
     (interactive)
     (if (,condition)
         (call-interactively ',command-a)
       (call-interactively ',command-b))))

(keyboard-translate ?\C-\[ ?\C-m)
(keyboard-translate ?\C-m ?\C-\[)
(keyboard-translate ?\C-i ?\C-\])
(keyboard-translate ?\C-\] ?\C-i)

;; ispell
(global-set-key (kbd "<f10>") 'ispell-word)
(global-set-key (kbd "C-<f10>") 'ispell-region)

;; linum
(global-set-key [S-f9] 'linum-mode)

;; redo+
(global-set-key (kbd "C-.") 'redo)

;; killing
(global-set-key (kbd "C-M-k") 'copy-line)
(global-set-key (kbd "C-w") 'kill-word-or-kill-region)
(global-set-key (kbd "C-M-w") 'kill-ring-save)

;; helm
(global-set-key (kbd "C-_") 'my-standard-helm)
(global-set-key (kbd "C-S-f") 'ac-complete-with-helm)
(global-set-key (kbd "C-S-x") 'helm-M-x)
(global-set-key (kbd "C-M-p") 'helm-imenu)
(global-set-key (kbd "C-c SPC") 'helm-lisp-completion-at-point)
(global-set-key (kbd "C-|") 'helm-etags-select)

;;
(global-set-key (kbd "C-\\") 'yas-insert-snippet)

;;
(global-set-key (kbd "C-c ;") 'comment-dwim)

;;
(global-set-key (kbd "S-C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;;
(global-set-key (kbd "C-c C-y") 'browse-kill-ring)

;;
(global-set-key (kbd "C-S-y") 'yank-unindented)

;;
(global-set-key [f8] 'howm-menu)

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
(global-set-key (kbd "C-c C-h") 'ace-jump-line-mode)
(global-set-key (kbd "C-c C-t") 'ace-jump-word-mode)
;;
(global-set-key (kbd "C-S-n") 'point-undo)
(global-set-key (kbd "C-S-h") 'point-redo)

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
(global-set-key (kbd "C-=") 'insert-parentheses)

;;
(global-set-key (kbd "C-;") 'universal-argument)
(global-set-key (kbd "C-]") 'other-window-or-split) ; C-] <-> C-i
(global-set-key (kbd "C-M-]") 'kill-buffer-and-window) ; C-] <-> C-i
(global-set-key [f2] 'swap-screen)
(global-set-key [C-f2] 'swap-screen-with-cursor)

;; scroll
(global-set-key (kbd "C-'") 'scroll-down)
(global-set-key (kbd "C-v") 'scroll-up-command)

;; expand-region
(global-set-key (kbd "C-8") 'er/expand-region)
(global-set-key (kbd "C-7") 'er/contract-region)

;; multiple-cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-*") 'mc/mark-all-like-this)

;; flymake
(global-set-key (kbd "M-h") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)

;;
(global-set-key (kbd "C-c C-8") 'join-line)
(global-set-key (kbd "<S-return>") '(lambda ()
                                      (interactive)
                                      (move-end-of-line nil)
                                      (newline-and-indent)))

(require 'cc-cmds)
(global-set-key (kbd "<S-backspace>") 'c-hungry-backspace)

(global-set-key (kbd "M-SPC") 'just-one-space)

;; fold-this
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-transpose)

;; all
;; (global-set-key (kbd "M-t l") 'transpose-lines)
;; (global-set-key (kbd "M-t w") 'transpose-words)

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

;; dmacro
(defconst *dmacro-key* (kbd "C-1"))
(require 'dmacro)
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;; ddskk
(when (require 'skk nil t)
  (global-set-key (kbd "C-x C-j") 'skk-mode))
