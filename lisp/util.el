;;; util.el ---

(require 'cl-lib)
(require 'dash)

;;
;; Report time to require
(defun report-time (original &rest args)
  (let* ((before (current-time))
         (returned (apply original args))
         (after  (current-time))
         (time (+ (* (- (nth 1 after) (nth 1 before)) 1000)
                  (/ (- (nth 2 after) (nth 2 before)) 1000))))
    (progn
	    (when (>= time 50.0)
	      (message "%s: %d msec" (car args) time))
	    returned)))
(advice-add 'require :around #'report-time)

;;
;; https://github.com/purcell/emacs.d/blob/master/init-clojure.el
(defmacro def-face (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

;;
;;
(defun add-keywords (mode-name keyword-list face-name)
  (let ((reg (regexp-opt keyword-list 'words)))
    (font-lock-add-keywords mode-name `((,reg . ',face-name)))))

;; Add load path recursively
(defun add-to-load-path-r (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;
;; http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;; example
;; (eval-safe (some-suspicious-code))
;; ;; nesting
;; (eval-safe
;;  (insert "1")
;;  (eval-safe
;;   (insert "2")
;;   (no-such-function))
;;  (insert "3")
;;  (no-such-function))

(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

;; example
;; (when (autoload-if-found 'bs-show "bs" "buffer selection" t)
;;   (global-set-key [(control x) (control b)] 'bs-show)
;;   (setq bs-max-window-height 10))

(defmacro exec-if-bound (sexplist)
  "eval given forms only when a function is defined"
  `(if (fboundp (car ',sexplist))
       ,sexplist))


(defun load-safe (loadlib)
  ""
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

;; ;; example
;; (exec-if-bound (set-file-coding-system '*euc-japan*))
;; (exec-if-bound (set-buffer-file-coding-system 'euc-japan))

;; (load-safe "its/han-kata")



;;
(defun linux-p   () (eq 'gnu/linux  system-type))
(defun windows-p () (eq 'windows-nt system-type))
(defun darwin-p () (eq 'darwin system-type))
(defalias 'mac-p 'darwin-p)

;;
(defalias 'null-p 'null)
(defalias 'null?  'null)

(defconst true  (= 1 1))
(defconst false (= 1 0))

;;
(defun now (&optional fmt)
  "Return current time with given format"
  (format-time-string (or fmt "%Y-%m-%d %H-%M-%S")))

;;
(defun font-exists-p (font)
  "Return true if FONT exists"
  (if (null (x-list-fonts font)) nil t))


;;;

(provide 'util)
