;;; okf-mode.el --- Major mode for Open Knowledge Format (OKF) bundles -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.5"))
;; Keywords: text, markdown, knowledge
;; URL: https://github.com/GoogleCloudPlatform/knowledge-catalog/blob/main/okf/SPEC.md

;;; Commentary:

;; `okf-mode' is a major mode for editing concept documents of an Open
;; Knowledge Format (OKF v0.2) bundle: markdown files with YAML
;; frontmatter.  It derives from `markdown-mode' and adds:
;;
;; - Font-lock for OKF frontmatter keys, status values, actors
;;   (`human:' / `process:' / agent), and ISO 8601 timestamps.
;; - A Flymake backend checking the conformance rules of the spec
;;   (section 11) plus soft warnings (stale concepts, timestamps without a
;;   UTC offset, footnotes without a matching `sources[].id', broken
;;   links, ...).
;; - Trust tier (unverified / machine-confirmed / human-reviewed),
;;   status, and staleness shown in the mode line.
;; - Bundle-aware link following (`/foo/bar.md' is relative to the bundle
;;   root), backlink search, and helpers to insert frontmatter, verification
;;   events, footnotes keyed to `sources', and `log.md' entries.
;;
;; Setup:
;;
;;   (add-to-list 'load-path "/path/to/okf-mode")
;;   (require 'okf-mode)
;;   (okf-setup)   ; switch markdown files inside OKF bundles to `okf-mode'
;;
;; A directory is treated as a bundle when an ancestor `index.md' declares
;; `okf_version', or when `okf-bundle-root' is set (e.g. in .dir-locals.el).
;; You can always run M-x okf-mode manually.

;;; Code:

(require 'markdown-mode)
(require 'flymake)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'iso8601)

(defgroup okf nil
  "Open Knowledge Format editing."
  :group 'text
  :prefix "okf-")

(defcustom okf-actor (format "human:%s" (user-login-name))
  "Actor string used for `generated.by' and `verified[].by' (spec section 7)."
  :type 'string)

(defcustom okf-common-types
  '("BigQuery Table" "BigQuery Dataset" "API Endpoint" "Metric" "Playbook"
    "Reference" "Attested Computation")
  "Type names offered when inserting frontmatter.  Types are not registered."
  :type '(repeat string))

(defvar-local okf-bundle-root nil
  "Bundle root directory.  Set it in .dir-locals.el to override detection.")
(put 'okf-bundle-root 'safe-local-variable #'stringp)

(defconst okf-reserved-files '("index.md" "log.md")
  "Reserved filenames that are not concept documents (spec section 3.1).")

(defconst okf-statuses '("draft" "stable" "deprecated"))

(defconst okf-known-keys
  '("type" "title" "description" "resource" "tags"
    "sources" "usage_window" "generated" "verified" "status" "stale_after"
    "runtime" "parameters" "computation" "executor" "attester"
    "okf_version" "timestamp")
  "Frontmatter keys defined by the spec (plus legacy `timestamp').")

;;;; Faces

(defface okf-key-face '((t :inherit font-lock-keyword-face :weight bold))
  "Face for OKF-defined frontmatter keys.")
(defface okf-human-face '((t :inherit success :weight bold))
  "Face for `human:' actors.")
(defface okf-process-face '((t :inherit font-lock-function-name-face))
  "Face for `process:' actors and agents.")
(defface okf-timestamp-face '((t :inherit font-lock-constant-face))
  "Face for ISO 8601 timestamps.")
(defface okf-status-stable-face '((t :inherit success))
  "Face for `status: stable'.")
(defface okf-status-draft-face '((t :inherit warning))
  "Face for `status: draft'.")
(defface okf-status-deprecated-face '((t :inherit error))
  "Face for `status: deprecated'.")

;;;; Frontmatter parsing (light-weight, no YAML library)

(defun okf--frontmatter-bounds ()
  "Return (START . END) of the frontmatter content, or nil.
START is just after the opening `---' line and END is the beginning of the
closing `---' line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at "---[ \t]*\n")
        (let ((start (match-end 0)))
          (goto-char start)
          (when (re-search-forward "^---[ \t]*$" nil t)
            (cons start (match-beginning 0))))))))

(defun okf--key-region (key)
  "Return (BEG . END) of the top-level frontmatter KEY, including its
continuation lines, or nil."
  (when-let* ((fm (okf--frontmatter-bounds)))
    (save-excursion
      (goto-char (car fm))
      (when (re-search-forward (format "^%s:" (regexp-quote key)) (cdr fm) t)
        (let ((beg (match-beginning 0)))
          (forward-line 1)
          (while (and (< (point) (cdr fm))
                      (not (looking-at "[A-Za-z_][^:\n]*:")))
            (forward-line 1))
          (cons beg (min (point) (cdr fm))))))))

(defun okf--key-value (key)
  "Return the inline scalar value of top-level KEY, unquoted, or nil."
  (when-let* ((fm (okf--frontmatter-bounds)))
    (save-excursion
      (goto-char (car fm))
      (when (re-search-forward
             (format "^%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key)) (cdr fm) t)
        (let ((v (match-string-no-properties 1)))
          (when (string-match "\\`\\([\"']\\)\\(.*\\)\\1\\'" v)
            (setq v (match-string 2 v)))
          v)))))

(defun okf--source-ids ()
  "Return the list of `id' values in frontmatter."
  (when-let* ((region (okf--key-region "sources")))
    (save-excursion
      (goto-char (car region))
      (let (ids)
        (while (re-search-forward "^[ \t]*-?[ \t]*id:[ \t]*[\"']?\\([^ \t\n\"']+\\)" (cdr region) t)
          (push (match-string-no-properties 1) ids))
        (nreverse ids)))))

(defun okf--source-title (id)
  "Return the title of the source with ID, or ID itself."
  (or (when-let* ((fm (okf--frontmatter-bounds)))
        (save-excursion
          (goto-char (car fm))
          (when (re-search-forward
                 (format "^[ \t]*-?[ \t]*id:[ \t]*[\"']?%s[\"']?[ \t]*$" (regexp-quote id))
                 (cdr fm) t)
            (let ((end (save-excursion
                         (or (re-search-forward "^[ \t]*-[ \t]*id:\\|^[A-Za-z_]" (cdr fm) t)
                             (cdr fm)))))
              (when (re-search-forward "title:[ \t]*[\"']?\\(.+?\\)[\"']?[ \t]*$" end t)
                (match-string-no-properties 1))))))
      id))

;;;; Trust, status, staleness

(defun okf--parse-time (string)
  "Parse ISO 8601 STRING into a time value, or nil."
  (condition-case nil
      (apply #'encode-time (iso8601-parse string))
    (error nil)))

(defun okf-trust-tier ()
  "Return the trust tier of the current concept (spec section 5.3).
One of `unverified', `machine-confirmed', `human-reviewed'."
  (let ((region (okf--key-region "verified")) actors)
    (when region
      (save-excursion
        (goto-char (car region))
        (while (re-search-forward "\\bby:[ \t]*[\"']?\\([^ \t,}\"'\n]+\\)" (cdr region) t)
          (push (match-string-no-properties 1) actors))))
    (cond ((null actors) 'unverified)
          ((seq-some (lambda (a) (string-prefix-p "human:" a)) actors) 'human-reviewed)
          (t 'machine-confirmed))))

(defun okf-status ()
  "Return the lifecycle status string; `stable' when absent (section 5.4)."
  (or (okf--key-value "status") "stable"))

(defun okf-stale-p ()
  "Non-nil if the concept's `stale_after' is on or before now (section 5.5)."
  (when-let* ((v (okf--key-value "stale_after"))
              (time (okf--parse-time v)))
    (not (time-less-p (current-time) time))))

(defun okf--mode-line ()
  "Build the `mode-line-process' string."
  (if (okf--reserved-file-p)
      ""
    (let ((tier (okf-trust-tier)) (status (okf-status)))
      (concat ":"
              (propertize (pcase tier
                            ('human-reviewed "human")
                            ('machine-confirmed "machine")
                            (_ "unverified"))
                          'face (pcase tier
                                  ('human-reviewed 'okf-human-face)
                                  ('machine-confirmed 'okf-process-face)
                                  (_ 'warning)))
              (unless (equal status "stable") (concat "/" status))
              (when (okf-stale-p) (propertize "/STALE" 'face 'error))))))

(defun okf--refresh-mode-line ()
  "Refresh the mode line, ignoring errors."
  (when (derived-mode-p 'okf-mode)
    (setq mode-line-process (ignore-errors (okf--mode-line)))
    (force-mode-line-update)))

(defun okf-describe-concept ()
  "Echo type, status, trust tier and staleness of the current concept."
  (interactive)
  (message "type=%s  status=%s  trust=%s  stale=%s  bundle=%s"
           (or (okf--key-value "type") "-") (okf-status) (okf-trust-tier)
           (if (okf-stale-p) "YES" "no") (okf-bundle-root)))

;;;; Bundle

(defun okf--reserved-file-p ()
  "Non-nil if the current buffer visits a reserved file (index.md / log.md)."
  (and buffer-file-name
       (member (file-name-nondirectory buffer-file-name) okf-reserved-files)))

(defun okf--declares-version-p (dir)
  "Non-nil if DIR/index.md declares `okf_version' in its frontmatter."
  (let ((index (expand-file-name "index.md" dir)))
    (when (file-readable-p index)
      (with-temp-buffer
        (insert-file-contents index nil 0 2048)
        (and (okf--frontmatter-bounds)
             (okf--key-value "okf_version")
             t)))))

(defun okf-bundle-root ()
  "Return the bundle root directory of the current buffer.
Uses `okf-bundle-root' when set, else the nearest ancestor whose `index.md'
declares `okf_version', else the topmost contiguous ancestor holding an
`index.md', else the current directory."
  (file-name-as-directory
   (expand-file-name
    (or okf-bundle-root
        (locate-dominating-file default-directory #'okf--declares-version-p)
        (let ((dir default-directory) root)
          (while (and dir (file-exists-p (expand-file-name "index.md" dir)))
            (setq root dir
                  dir (let ((parent (file-name-directory (directory-file-name dir))))
                        (unless (equal parent dir) parent))))
          root)
        default-directory))))

(defun okf--resolve-link (target)
  "Resolve markdown link TARGET to an absolute file name."
  (let ((path (replace-regexp-in-string "[#?].*\\'" "" target)))
    (if (string-prefix-p "/" path)
        (expand-file-name (substring path 1) (okf-bundle-root))
      (expand-file-name path default-directory))))

(defun okf-follow-link ()
  "Follow the markdown link at point; `/x.md' is relative to the bundle root."
  (interactive)
  (let ((url (markdown-link-url)))
    (if (and url (not (string-match-p "\\`[a-z][a-z0-9+.-]*:" url))
             (string-prefix-p "/" url))
        (let ((file (okf--resolve-link url)))
          (if (file-exists-p file)
              (find-file file)
            (message "Broken link (not yet written?): %s" file)))
      (markdown-follow-thing-at-point nil))))

(defun okf-find-backlinks ()
  "List concepts in the bundle that link to the current file."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (let ((name (file-name-nondirectory buffer-file-name)))
    (rgrep (format "[(/]%s[#)]" (regexp-quote name))
           "*.md" (okf-bundle-root))))

(defun okf-open-index ()
  "Open the `index.md' of the current directory."
  (interactive)
  (find-file (expand-file-name "index.md" default-directory)))

;;;; Editing helpers

(defun okf--now ()
  "Current time as a UTC ISO 8601 string."
  (format-time-string "%FT%TZ" nil t))

(defun okf-insert-frontmatter (type)
  "Insert a new frontmatter block of TYPE at the top of the buffer."
  (interactive
   (list (completing-read "Concept type: " okf-common-types nil nil)))
  (when (okf--frontmatter-bounds)
    (user-error "Frontmatter already present"))
  (save-excursion
    (goto-char (point-min))
    (insert (format "---\ntype: %s\ntitle: \ndescription: \ntags: []\nstatus: draft\ngenerated: { by: %s, at: %s }\n---\n\n"
                    type okf-actor (okf--now)))
    (when (equal type "Attested Computation")
      (insert "# Computation\n\n    \n")))
  (goto-char (point-min))
  (re-search-forward "^title: " nil t))

(defun okf-update-generated ()
  "Set `generated' to the current actor and time (spec section 5.2)."
  (interactive)
  (let ((line (format "generated: { by: %s, at: %s }" okf-actor (okf--now)))
        (region (okf--key-region "generated")))
    (save-excursion
      (if region
          (progn (delete-region (car region) (cdr region))
                 (goto-char (car region))
                 (insert line "\n"))
        (goto-char (or (cdr (okf--frontmatter-bounds))
                       (user-error "No frontmatter")))
        (insert line "\n")))
    (okf--refresh-mode-line)))

(defun okf-add-verified (&optional actor)
  "Record a verification event by ACTOR (default `okf-actor') now."
  (interactive (list (read-string "Verified by: " okf-actor)))
  (let* ((entry (format "{ by: %s, at: %s }" (or actor okf-actor) (okf--now)))
         (region (okf--key-region "verified")))
    (save-excursion
      (cond
       ((null region)
        (goto-char (or (cdr (okf--frontmatter-bounds)) (user-error "No frontmatter")))
        (insert (format "verified: %s\n" entry)))
       ;; bare mapping on the key line -> list of two entries
       ((save-excursion (goto-char (car region))
                        (looking-at "verified:[ \t]*\\({.*}\\)[ \t]*$"))
        (let ((old (match-string 1)))
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (insert (format "verified:\n  - %s\n  - %s\n" old entry))))
       (t
        (goto-char (cdr region))
        (skip-chars-backward " \t\n")
        (forward-line 1)
        (insert (format "  - %s\n" entry)))))
    (okf--refresh-mode-line)))

(defun okf-insert-footnote (id)
  "Insert a footnote reference to source ID at point and define it at the end."
  (interactive
   (list (completing-read "Source id: " (or (okf--source-ids)
                                            (user-error "No sources[].id in frontmatter")))))
  (insert (format "[^%s]" id))
  (save-excursion
    (goto-char (point-max))
    (unless (re-search-backward (format "^\\[\\^%s\\]:" (regexp-quote id)) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n[^%s]: %s\n" id (okf--source-title id))))))

(defun okf-insert-log-entry (kind text)
  "Add a `log.md' entry of KIND (e.g. Update) with TEXT under today's date."
  (interactive
   (list (completing-read "Kind: " '("Update" "Creation" "Deprecation" "Initialization")
                          nil nil nil nil "Update")
         (read-string "Entry: ")))
  (unless (and buffer-file-name
               (equal (file-name-nondirectory buffer-file-name) "log.md"))
    (user-error "Not visiting a log.md"))
  (let ((today (format-time-string "%F"))
        (bullet (format "* **%s**: %s\n" kind text)))
    (goto-char (point-min))
    (if (re-search-forward "^## \\(.*\\)$" nil t)
        (if (equal (match-string 1) today)
            (progn (forward-line 1) (insert bullet))
          (goto-char (match-beginning 0))
          (insert (format "## %s\n%s\n" today bullet)))
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (insert (format "## %s\n%s" today bullet)))))

;;;; Flymake

(defconst okf--timestamp-regexp
  (concat "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}"
          "\\(?::[0-9]\\{2\\}\\(?:\\.[0-9]+\\)?\\)?"
          "\\(Z\\|[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)?")
  "Match an ISO 8601 datetime; group 1 is the UTC offset.")

(defun okf--diagnostics ()
  "Return a list of Flymake diagnostics for the current buffer."
  (let ((buf (current-buffer)) diags)
    (cl-flet ((add (beg end type text)
                (push (flymake-make-diagnostic buf beg (max end (1+ beg)) type text) diags))
              (line-at (pos) (save-excursion (goto-char pos)
                                             (cons (line-beginning-position)
                                                   (line-end-position)))))
      (save-excursion
        (save-restriction
          (widen)
          (if (okf--reserved-file-p)
              (when (equal (file-name-nondirectory buffer-file-name) "log.md")
                (goto-char (point-min))
                (while (re-search-forward "^## \\(.*\\)$" nil t)
                  (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                                          (match-string 1))
                    (add (match-beginning 0) (match-end 0) :warning
                         "log.md date headings must be YYYY-MM-DD (spec section 9)"))))
            (let ((fm (okf--frontmatter-bounds)))
              (if (null fm)
                  (add (point-min) (cdr (line-at (point-min))) :error
                       "Missing YAML frontmatter (spec section 4)")
                ;; type
                (let ((type (okf--key-value "type")))
                  (when (or (null type) (string-empty-p type))
                    (add (car fm) (car fm) :error "Missing or empty `type' (spec section 4.1)")))
                ;; legacy
                (when-let* ((r (okf--key-region "timestamp")))
                  (add (car r) (save-excursion (goto-char (car r)) (line-end-position)) :note
                       "`timestamp' is superseded by `generated.at' in v0.2"))
                ;; status
                (when-let* ((s (okf--key-value "status")))
                  (unless (member s okf-statuses)
                    (let ((l (line-at (car (okf--key-region "status")))))
                      (add (car l) (cdr l) :warning
                           (format "status should be one of %s" (string-join okf-statuses " | "))))))
                ;; attested computation
                (when (equal (okf--key-value "type") "Attested Computation")
                  (unless (okf--key-region "runtime")
                    (add (car fm) (car fm) :error "Attested Computation requires `runtime' (section 10.2)")))
                ;; timestamps without offset
                (goto-char (car fm))
                (while (re-search-forward okf--timestamp-regexp (cdr fm) t)
                  (unless (match-string 1)
                    (add (match-beginning 0) (match-end 0) :warning
                         "Timestamp needs an explicit UTC offset, e.g. 2026-06-30T14:00:00Z (section 5)")))
                ;; stale
                (when (okf-stale-p)
                  (let ((l (line-at (car (okf--key-region "stale_after")))))
                    (add (car l) (cdr l) :warning "Concept is stale (now >= stale_after)")))
                ;; footnotes vs sources ids
                (let ((ids (okf--source-ids)))
                  (goto-char (cdr fm))
                  (while (re-search-forward "\\[\\^\\([^]\n]+\\)\\]" nil t)
                    (unless (member (match-string-no-properties 1) ids)
                      (add (match-beginning 0) (match-end 0) :warning
                           (format "Footnote `%s' has no matching sources[].id (section 5.1)"
                                   (match-string-no-properties 1))))))
                ;; broken links (allowed, but worth a note)
                (goto-char (cdr fm))
                (while (re-search-forward "\\]\\(([^)\s]+\\.md\\(?:#[^)]*\\)?)\\)" nil t)
                  (let ((target (substring (match-string-no-properties 1) 1 -1)))
                    (unless (or (string-match-p "\\`[a-z][a-z0-9+.-]*:" target)
                                (file-exists-p (okf--resolve-link target)))
                      (add (match-beginning 1) (match-end 1) :note
                           "Link target does not exist (tolerated by spec section 6.1)"))))))))))
    diags))

(defun okf-flymake (report-fn &rest _args)
  "Flymake backend for OKF; calls REPORT-FN with diagnostics."
  (funcall report-fn (okf--diagnostics)))

;;;; Font-lock

(defun okf--fm-matcher (regexp)
  "Return a font-lock matcher for REGEXP restricted to the frontmatter."
  (lambda (limit)
    (let ((fm (okf--frontmatter-bounds)))
      (when (and fm (< (point) (cdr fm)))
        (re-search-forward regexp (min limit (cdr fm)) t)))))

(defconst okf-font-lock-keywords
  (list
   (cons (okf--fm-matcher
          (format "^\\(%s\\):" (regexp-opt okf-known-keys)))
         '(1 'okf-key-face prepend))
   (cons (okf--fm-matcher "^type:[ \t]*\\(.+?\\)[ \t]*$")
         '(1 font-lock-type-face prepend))
   (cons (okf--fm-matcher "^status:[ \t]*\\(stable\\)\\>")
         '(1 'okf-status-stable-face prepend))
   (cons (okf--fm-matcher "^status:[ \t]*\\(draft\\)\\>")
         '(1 'okf-status-draft-face prepend))
   (cons (okf--fm-matcher "^status:[ \t]*\\(deprecated\\)\\>")
         '(1 'okf-status-deprecated-face prepend))
   (cons (okf--fm-matcher "\\<\\(human:[^ \t,}\n]+\\)")
         '(1 'okf-human-face prepend))
   (cons (okf--fm-matcher "\\<\\(process:[^ \t,}\n]+\\)")
         '(1 'okf-process-face prepend))
   (cons (okf--fm-matcher (concat "\\(" okf--timestamp-regexp "\\)"))
         '(1 'okf-timestamp-face prepend))
   (cons (okf--fm-matcher "\\<\\(by\\|at\\|from\\|to\\|id\\|name\\|required\\|receipt\\):")
         '(1 font-lock-variable-name-face prepend)))
  "Extra font-lock keywords for OKF frontmatter.")

;;;; Mode

(defvar okf-mode-map
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix (kbd "f") #'okf-insert-frontmatter)
    (define-key prefix (kbd "g") #'okf-update-generated)
    (define-key prefix (kbd "v") #'okf-add-verified)
    (define-key prefix (kbd "n") #'okf-insert-footnote)
    (define-key prefix (kbd "l") #'okf-insert-log-entry)
    (define-key prefix (kbd "b") #'okf-find-backlinks)
    (define-key prefix (kbd "i") #'okf-open-index)
    (define-key prefix (kbd "d") #'okf-describe-concept)
    (define-key map (kbd "C-c C-v") prefix)
    (define-key map (kbd "C-c C-o") #'okf-follow-link)
    map)
  "Keymap for `okf-mode'.  Commands live under C-c C-v.")

;;;###autoload
(define-derived-mode okf-mode markdown-mode "OKF"
  "Major mode for Open Knowledge Format (OKF v0.2) concept documents.

\\{okf-mode-map}"
  (font-lock-add-keywords nil okf-font-lock-keywords 'append)
  (add-hook 'flymake-diagnostic-functions #'okf-flymake nil t)
  (add-hook 'after-save-hook #'okf--refresh-mode-line nil t)
  (add-hook 'after-change-functions
            (lambda (beg &rest _) (when (< beg 2000) (okf--refresh-mode-line))) nil t)
  (okf--refresh-mode-line)
  (flymake-mode 1))

;;;###autoload
(defun okf-maybe-enable ()
  "Switch a `markdown-mode' buffer to `okf-mode' when it is inside a bundle."
  (when (and buffer-file-name
             (memq major-mode '(markdown-mode gfm-mode))
             (or okf-bundle-root
                 (locate-dominating-file default-directory #'okf--declares-version-p)))
    (okf-mode)))

;;;###autoload
(defun okf-setup ()
  "Enable `okf-mode' automatically for markdown files inside OKF bundles."
  (add-hook 'find-file-hook #'okf-maybe-enable))

(provide 'okf-mode)
;;; okf-mode.el ends here
