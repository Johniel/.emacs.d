;;; integrate-yas-ac.el --- Integration for Auco-Complete and Yasnippet

(require 'auto-complete)
(require 'auto-complete-config)
(require 'dash)
(require 'util)
(require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove Yasnippet Keywords Into `ac-ignores' For Each Buffers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-yas-keys-to-ac-ignores ()
  (interactive)
  (progn
    (make-local-variable 'ac-ignores)
    (->> (yas--get-snippet-tables)
      (-map 'yas--table-hash)
      (--map (loop for k being the hash-keys in it collect k))
      (-flatten)
      (--map (add-to-list 'ac-ignores it)))))

(defadvice yas--load-pending-jits (after my-ac-conf activate)
  (add-yas-keys-to-ac-ignores))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stup Auto-Complete While Expanding Yasnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; https://gist.github.com/750077
;;; yasnippet and auto-complete

(defvar ac-yas-expand-autostart-backup nil "s")

(defun ac-yas-expand-start ()
  ""
  (setq ac-yas-expand-autostart-backup ac-auto-start)
  (setq ac-auto-start nil))

(defun ac-yas-expand-end ()
  ""
  (setq ac-auto-start ac-yas-expand-autostart-backup))

(defun ac-yas-expand-install ()
  (interactive)
  (add-hook 'yas-before-expand-snippet-hook 'ac-yas-expand-start)
  (add-hook 'yas-after-exit-snippet-hook 'ac-yas-expand-end))

(defun ac-yas-expand-uninstall ()
  (interactive)
  (remove-hook 'yas-before-expand-snippet-hook 'ac-yas-expand-start)
  (remove-hook 'yas-after-exit-snippet-hook 'ac-yas-expand-end))

(ac-yas-expand-install)

;;;
(provide 'integrate-yas-ac)
