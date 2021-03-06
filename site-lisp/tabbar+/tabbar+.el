;;; tabbar+.el ---

;; Copyright (C)

;; Author:
;; Keywords:

;; Version 0.2.2

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'tabbar)
(require 'dash)

(defun tabbar+sort-tab ()
  "Sort current tab group in lexicographically order"
  (interactive)
  (let* ((ctabset (tabbar-current-tabset 't))
         (ctabs (tabbar-tabs ctabset)))
    (if (and ctabset ctabs)
        (progn
          (set ctabset (sort ctabs (lambda (b1 b2)
                                     (string-lessp (buffer-name (car b1))
                                                   (buffer-name (car b2))))))
          (put ctabset 'template nil)
          (tabbar-display-update)))))

;;
;; http://d.hatena.ne.jp/tequilasunset/20110103/p1
;;

(defvar tabbar+displayed-buffers
  '("*slime-repl clojure*"
    "*Backtrace*"
    "*Colors*"
    "*Faces*"
    "eshell"
    "*grep*"
    "*ielm*"
    "*nrepl*")
  "*Reagexps matches buffer names always included tabs.")

(defun tabbar+buffer-list-function ()
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt tabbar+displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (and (not (string-match "*howm:.**" name))
                                            (or (string-match re name)
                                                (not (memq (aref name 0) hides))))
                                   buf)))
                             (buffer-list)))))
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(defun tabbar+buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))))

(defun tabbar+buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

(setq tabbar-help-on-tab-function 'tabbar+buffer-help-on-tab)
(setq tabbar-select-tab-function  'tabbar+buffer-select-tab)
(setq tabbar-buffer-list-function 'tabbar+buffer-list-function)

;;
;; Tab Group
;;

(defconst tabbar+default-group-name "Default")

(defvar tabbar+group nil)
(make-variable-buffer-local 'tabbar+group)

(defun tabbar+init-group ()
  ""
  (--map (with-current-buffer it
           (setq tabbar+group tabbar+default-group-name))
         (buffer-list)))

(defun tabbar+get-group (buff)
  "Return BUFF's tab group."
  (with-current-buffer buff
    (or tabbar+group
        (let ((project-name (projectile-project-name)))
          (if (string= "" project-name)
              tabbar+default-group-name
            project-name)))))

(defun tabbar+get-all-group-name ()
  "Return tab group name list."
  (->> (buffer-list)
    (-map 'tabbar+get-group)
    (-distinct)
    (-remove 'null)))

;;
;; Tabbar grouping function
;;

(defvar tabbar+default-group-name-list
  '(("/ssh:.*" "TRAMP")
    ("//*scratch//*"  ,tabbar+default-group-name)
    ("//*Messages//*" ,tabbar+default-group-name)
    ("//*Packages//*" ,tabbar+default-group-name)))

(defun tabbar+get-default-group-name (buffer-name)
  (cadr (--first (not (eq nil (string-match (car it) buffer-name))) tabbar+default-group-name-list)))

(defun tabbar+buffer-groups-function ()
  "Return current buffer's group name."
  (if (not tabbar+group)
      (setq tabbar+group (or (tabbar+get-default-group-name (buffer-name))
                             (tabbar+get-group (current-buffer)))))
  (list tabbar+group))

;;
;; Group commands
;;

(defun tabbar+change-group (name)
  "Change current buffer's group to NAME."
  (interactive
   (list (completing-read "Tab group: " (tabbar+get-all-group-name))))
  (setq tabbar+group name)
  (tabbar-display-update))

(defun tabbar+switch-group (name)
  "Change current group to NAME."
  (interactive
   (list (completing-read "Tab group: " (tabbar+get-all-group-name))))
  (switch-to-buffer (--first (string= name (tabbar+get-group it)) (buffer-list))))

(defun tabbar+rename-group (new-name)
  (interactive
   (list (completing-read "Rename tab group: " (tabbar+get-all-group-name))))
  (let ((old-name tabbar+group))
    (->> (buffer-list)
      (--filter (string= old-name (tabbar+get-group it)))
      (--map (with-current-buffer it
               (setq tabbar+group new-name))))))

(defun tabbar+group-buffers (group-name)
  "Return list of buffers belonging to GROUP-NAME"
  (->> (buffer-list)
    (--filter (string= group-name (tabbar+get-group it)))))

;;
;; Tab Position
;;

(defun tabbar+get-current-buffer-index ()
  (let* ((ctabset (tabbar-current-tabset 't))
         (ctabs (tabbar-tabs ctabset))
         (ctab (tabbar-selected-tab ctabset)))
    (length (--take-while (not (eq it ctab)) ctabs))))

(defun insert- (list-object index element)
  (append (-take index list-object) (list element) (-drop index list-object)))

(defun tabbar+move (direction)
  "Move current tab to (+ index DIRECTION)"
  (interactive)
  (let* ((ctabset (tabbar-current-tabset 't))
         (ctabs (tabbar-tabs ctabset))
         (ctab (tabbar-selected-tab ctabset))
         (index (tabbar+get-current-buffer-index))
         (others (--remove (eq it ctab) ctabs))
         (ins (mod (+ index direction (+ 1 (length others))) (+ 1 (length others)))))
    (set ctabset (insert- others ins ctab))
    (put ctabset 'template nil)
    (tabbar-display-update)))

(defun tabbar+move-right ()
  "Move current tab to right"
  (interactive)
  (tabbar+move +1))

(defun tabbar+move-left ()
  "Move current tab to left"
  (interactive)
  (tabbar+move -1))

;;
;; Kill Buffer or Group
;;

(defun tabbar+remove-right ()
  "Remove right side buffers"
  (interactive)
  (let* ((ctabset (tabbar-current-tabset 't))
         (ctabs (tabbar-tabs ctabset))
         (ctab (tabbar-selected-tab ctabset)))
    (--map (kill-buffer (car it)) (cdr (--drop-while (not (eq ctab it)) ctabs)))))

(defun tabbar+remove-left ()
  "Remove left side buffers"
  (interactive)
  (let* ((ctabset (tabbar-current-tabset 't))
         (ctabs (tabbar-tabs ctabset))
         (ctab (tabbar-selected-tab ctabset)))
    (--map (kill-buffer (car it)) (--take-while (not (eq ctab it)) ctabs))))

(defun tabbar+kill-group (group)
  "Kill all buffers belonging to GROUP."
  (interactive
   (list (completing-read "Tab Group: " (tabbar+get-all-group-name))))
  (->> (buffer-list)
    (--filter (string= group (tabbar+get-group it)))
    (-map 'kill-buffer)))

;;
;;
;;

(defvar tabbar+group-mode nil)

(defun tabbar+add-group-name-to-mode-line ()
  (add-to-list 'default-mode-line-format
               '(:eval
                 (concat " [" (format "%s" (tabbar-current-tabset t)) "]"))))

(defun tabbar+enable-tab-group ()
  ""
  (interactive)
  (unless tabbar+group-mode
    (tabbar+init-group)
    (setq tabbar-buffer-groups-function 'tabbar+buffer-groups-function)
    (setq tabbar+group-mode +1)))

;;
;; Helm interface
;;
(require 'helm)

(defun tabbar+get-group-buffers ()
  (->> (current-buffer)
    (tabbar+get-group)
    (tabbar+group-buffers)
    (-map 'buffer-name)
    (--filter (not (or (string= " " (substring it 0 1))
                       (string= it "*helm*"))))))

(defvar tabbar+group-buffers-cache nil)

(defvar helm-c-source-tabbar+current-group-buffers
  `((name . "Tabbar+ Current Group Buffers")
    (init . (lambda ()
              (setq tabbar+group-buffers-cache (tabbar+get-group-buffers))
              (let ((len-buf  (-reduce 'max (-map 'length tabbar+group-buffers-cache)))
                    (len-mode (-reduce 'max (--map (length (with-current-buffer it
                                                             (symbol-name major-mode)))
                                                   tabbar+group-buffers-cache))))
                (unless helm-buffer-max-length
                  (setq helm-buffer-max-length (+ len-buf 5)))
                (unless helm-buffer-max-len-mode
                  (setq helm-buffer-max-len-mode len-mode)))))
    (candidates . tabbar+group-buffers-cache)
    (action ("Switch to buffer" . switch-to-buffer)
            ("Change group"     . tabbar+change-group)
            ("Kill buffer"      . kill-buffer)
            ("Kill right side"  . tabbar+remove-right)
            ("Kill left side"   . tabbar+remove-left))
    (type . buffer)
    (volatile)))

(defun tabbar+rename-group-action (selected)
  (->> (buffer-list)
    (--first (string= selected (tabbar+get-group it)))
    ((lambda (buff)
       (with-current-buffer buff
         (command-execute 'tabbar+rename-group))))))

(defvar helm-c-source-tab-groups-list
  `((name . "Tab Group")
    (candidates . tabbar+get-all-group-name)
    (volatile)
    (action ("Switch group" . tabbar+switch-group)
            ("Change group" . tabbar+change-group)
            ("Kill group"   . tabbar+kill-group)
            ("Rename group" . tabbar+rename-group-action))))

;;

(provide 'tabbar+)

;;; tabbar+.el ends here
