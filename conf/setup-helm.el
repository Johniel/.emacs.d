;;; setup-helm.el

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-command)

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

(require 'tabbar+)
(defun my-standard-helm ()
  (interactive)
  (helm :sources '(
                   ;; helm-c-source-buffers-list
                   ;; helm-c-source-tabbar+buffers-list
                   helm-c-source-tabbar+current-group-buffers-list
                   helm-c-source-tab-groups-list
                   helm-c-source-imenu
                   helm-c-source-recentf
                   ;; helm-c-source-buffer-not-found
    )
        :buffer "*helm*"
        :keymap helm-c-buffer-map))

(require 'popwin)
(push "*helm*" popwin:special-display-config)

(provide 'setup-helm)

;;; ----- helm sources 2012/12/08
;; helm-apt.el       helm-c-source-apt
;; helm-bbdb.el      helm-c-source-bbdb
;; helm-bmkext.el    helm-c-source-bmkext-addressbook
;; helm-bmkext.el    helm-c-source-bookmark-files&dirs
;; helm-bmkext.el    helm-c-source-bookmark-gnus
;; helm-bmkext.el    helm-c-source-bookmark-images
;; helm-bmkext.el    helm-c-source-bookmark-info
;; helm-bmkext.el    helm-c-source-bookmark-man
;; helm-bmkext.el    helm-c-source-bookmark-ssh-files&dirs
;; helm-bmkext.el    helm-c-source-bookmark-su-files&dirs
;; helm-bmkext.el    helm-c-source-bookmark-w3m
;; helm-bookmark.el  helm-c-source-bookmark-set
;; helm-bookmark.el  helm-c-source-bookmarks
;; helm-bookmark.el  helm-c-source-bookmarks-local
;; helm-bookmark.el  helm-c-source-bookmarks-ssh
;; helm-bookmark.el  helm-c-source-bookmarks-su
;; helm-buffers.el   helm-c-source-buffer-not-found
;; helm-buffers.el   helm-c-source-buffers-list
;; helm-buffers.el   helm-c-source-ido-virtual-buffers
;; helm-call-tree.el helm-c-source-simple-call-tree-callers-functions
;; helm-call-tree.el helm-c-source-simple-call-tree-functions-callers
;; helm-color.el     helm-c-source-colors
;; helm-color.el     helm-c-source-customize-face
;; helm-elisp.el     helm-c-source-absolute-time-timers
;; helm-elisp.el     helm-c-source-advice
;; helm-elisp.el     helm-c-source-complex-command-history
;; helm-elisp.el     helm-c-source-elisp-library-scan
;; helm-elisp.el     helm-c-source-idle-time-timers
;; helm-elscreen.el  helm-c-source-elscreen
;; helm-emms.el      helm-c-source-emms-dired
;; helm-emms.el      helm-c-source-emms-files
;; helm-emms.el      helm-c-source-emms-streams
;; helm-eshell.el    helm-c-source-esh
;; helm-eshell.el    helm-c-source-eshell-history
;; helm-eval.el      helm-c-source-calculation-result
;; helm-eval.el      helm-c-source-evaluation-result
;; helm-files.el     helm-c-source-copy-files
;; helm-files.el     helm-c-source-ff-file-name-history
;; helm-files.el     helm-c-source-ffap-guesser
;; helm-files.el     helm-c-source-ffap-line
;; helm-files.el     helm-c-source-file-cache
;; helm-files.el     helm-c-source-file-name-history
;; helm-files.el     helm-c-source-files-in-all-dired
;; helm-files.el     helm-c-source-files-in-current-dir
;; helm-files.el     helm-c-source-find-files
;; helm-files.el     helm-c-source-hardlink-files
;; helm-files.el     helm-c-source-insert-file
;; helm-files.el     helm-c-source-recentf
;; helm-files.el     helm-c-source-rename-files
;; helm-files.el     helm-c-source-session
;; helm-files.el     helm-c-source-symlink-files
;; helm-files.el     helm-c-source-write-file
;; helm-firefox.el   helm-c-source-firefox-bookmarks
;; helm-font.el      helm-c-source-ucs
;; helm-font.el      helm-c-source-xfonts
;; helm-gentoo.el    helm-c-source-gentoo
;; helm-gentoo.el    helm-c-source-use-flags
;; helm-imenu.el     helm-c-source-imenu
;; helm-info.el      helm-c-source-info-pages
;; helm-locate.el    helm-c-source-locate
;; helm-man.el       helm-c-source-man-pages
;; helm-misc.el      helm-c-source-eev-anchor
;; helm-misc.el      helm-c-source-emacs-lisp-expectations
;; helm-misc.el      helm-c-source-emacs-lisp-toplevels
;; helm-misc.el      helm-c-source-emacs-source-defun
;; helm-misc.el      helm-c-source-fixme
;; helm-misc.el      helm-c-source-jabber-contacts
;; helm-misc.el      helm-c-source-lacarte
;; helm-misc.el      helm-c-source-latex-math
;; helm-misc.el      helm-c-source-mac-spotlight
;; helm-misc.el      helm-c-source-minibuffer-history
;; helm-misc.el      helm-c-source-oddmuse-headline
;; helm-misc.el      helm-c-source-picklist
;; helm-misc.el      helm-c-source-ratpoison-commands
;; helm-misc.el      helm-c-source-rd-headline
;; helm-misc.el      helm-c-source-stumpwm-commands
;; helm-misc.el      helm-c-source-time-world
;; helm-misc.el      helm-c-source-tracker-search
;; helm-net.el       helm-c-source-google-suggest
;; helm-net.el       helm-c-source-yahoo-suggest
;; helm-org.el       helm-c-source-org-headline
;; helm-org.el       helm-c-source-org-keywords
;; helm-plugin.el    helm-c-source-info-wget
;; helm-plugin.el    helm-c-source-test-file
;; helm-regexp.el    helm-c-source-browse-code
;; helm-regexp.el    helm-c-source-moccur
;; helm-regexp.el    helm-c-source-occur
;; helm-regexp.el    helm-c-source-regexp
;; helm-ring.el      helm-c-source-global-mark-ring
;; helm-ring.el      helm-c-source-kill-ring
;; helm-ring.el      helm-c-source-mark-ring
;; helm-ring.el      helm-c-source-register
;; helm-semantic.el  helm-c-source-semantic
;; helm-sys.el       helm-c-source-emacs-process
;; helm-sys.el       helm-c-source-top
;; helm-sys.el       helm-c-source-xrandr-change-resolution
;; helm-tags.el      helm-c-source-ctags
;; helm-tags.el      helm-c-source-etags-select
;; helm-w3m.el       helm-c-source-w3m-bookmarks
;; helm-yaoddmuse.el helm-c-source-yaoddmuse-emacswiki-edit-or-view
;; helm-yaoddmuse.el helm-c-source-yaoddmuse-emacswiki-post-library
