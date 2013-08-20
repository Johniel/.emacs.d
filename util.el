;;; util.el ---

(require 'cl-lib)
(require 'dash)

;;
;; Report time to require
(defadvice require (around require-benchmark activate)
  (let* ((before (current-time))
         (result ad-do-it)
         (after  (current-time))
         (time (+ (* (- (nth 1 after) (nth 1 before)) 1000)
                  (/ (- (nth 2 after) (nth 2 before)) 1000))))
    (when (>= time 50.0)
      (message "%s: %d msec" (ad-get-arg 0) time))))

;;
(defmacro lazyload (func lib &rest body)
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
          ,@body))))

;;
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))

;;
(defmacro when-require (lib &rest body)
  (declare (indent 1))
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body t))

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

(defmacro defun-add-hook (hookname &rest sexplist)
  "alias of add-hook."
  `(add-hook ,hookname
             (function (lambda () ,@sexplist))))

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

;; (defun-add-hook 'perl-mode-hook
;;   (exec-if-bound (set-buffer-file-coding-system 'euc-japan-unix)))

;; http://tomykaira.hatenablog.com/entry/2013/01/25/000057
;;
(defun increment-decimal-number (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

;;
;; http://www.bookshelf.jp/soft/meadow_15.html#SEC119
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))


;;
(defun linux-p    () (eq 'gnu/linux  system-type))
(defun windowns-p () (eq 'windows-nt system-type))


;;
(defun nil-p (n)
  ""
  (not n))

;;
(defun range (a b)
  "(a ... b)"
  (if (<= a b)
      (cons a (range (+ a 1) b))
    '()))

;;
(defun product (s1 s2)
  "(a b) (c d) -> ((a c) (a d) (b c) (b d))"
  (let (result)
    (dolist (a s1)
      (dolist (b s2)
        (push (-flatten (list a b)) result)))
    (nreverse result)))

;;
(defun product-list (&rest list)
  (if (null list)
      list
    (let ((a (car list)))
      (dolist (b (cdr list))
        (setq a (product a b)))
      a)))

;;;

(provide 'util)
