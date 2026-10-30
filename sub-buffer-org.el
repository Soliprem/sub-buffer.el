;;; sub-buffer-org.el --- Persistent Org queues -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (sub-buffer "0.1.0"))
;; Keywords: multimedia, outlines

;;; Commentary:

;; Set `sub-buffer-use-org' before opening a queue.  Track IDs are Org
;; links, so ordinary file saving preserves the playlist.  No import step.

;;; Code:

(require 'sub-buffer)
(require 'org)
(require 'org-element)

;; These methods implement the queue format generics in sub-buffer.el.

(cl-defmethod sub-buffer--line-start (&context (major-mode sub-buffer-org-mode))
  "Return the beginning of this line's text after its Org list prefix."
  (save-excursion
    (beginning-of-line)
    (when (org-at-item-p)
      (goto-char (match-end 0))
      (when (looking-at "\\(?:\\[@[[:alnum:]]+\\][ \t]+\\)?\\(?:\\[[ X-]\\][ \t]+\\)?")
        (goto-char (match-end 0))))
    (point)))

(cl-defmethod sub-buffer--format (label song &context (major-mode sub-buffer-org-mode))
  "Format LABEL and SONG as an Org link, keeping the current list prefix."
  (let ((prefix (buffer-substring-no-properties
                 (line-beginning-position) (sub-buffer--line-start))))
    (concat (if (string-empty-p prefix) "- " prefix)
            (org-link-make-string
             (concat "sub-buffer:" (url-hexify-string (alist-get 'id song))) label))))

(cl-defmethod sub-buffer--entry (&context (major-mode sub-buffer-org-mode))
  "Read this line's unique track link, caching metadata only in text properties.
The saved link is authoritative; changing its ID invalidates cached metadata.
The cached entry is returned itself, so callers can compare entries with
`eq'; only its label is updated in place."
  (save-excursion
    (beginning-of-line)
    (let ((end (line-end-position)) entry conflict)
      (while (re-search-forward org-link-bracket-re end t)
        ;; The regexp also matches inside comments and source blocks.
        (let* ((begin (match-beginning 0)) (finish (match-end 0))
               (link (save-excursion (goto-char begin) (org-element-context))))
          (when (and (eq (org-element-type link) 'link)
                     (equal (org-element-property :type link) "sub-buffer"))
            (let* ((id (decode-coding-string
                        (url-unhex-string (org-element-property :path link)) 'utf-8))
                   (cached (get-text-property begin 'sub-buffer-track))
                   (label (if-let* ((start (org-element-property :contents-begin link)))
                              (buffer-substring-no-properties
                               start (org-element-property :contents-end link))
                            id)))
              (if entry (setq conflict t)
                (unless (string-empty-p id)
                  (setq entry (if (equal id (alist-get 'id (cdr cached)))
                                  cached
                                (cons label `((id . ,id)))))
                  (setcar entry label)
                  (with-silent-modifications
                    (put-text-property begin finish 'sub-buffer-track entry)
                    (put-text-property begin finish 'rear-nonsticky
                                       '(sub-buffer-track rear-nonsticky)))))))
          (goto-char finish)))
      (unless conflict entry))))

(cl-defmethod sub-buffer--detach (&context (major-mode sub-buffer-org-mode))
  "Replace this track link with its readable description for a fresh search."
  (when-let* ((entry (sub-buffer--entry)))
    (atomic-change-group
      (let ((start (sub-buffer--line-start)))
        (delete-region start (line-end-position))
        (goto-char start)
        (insert (substring-no-properties (car entry))))
      (remove-text-properties (line-beginning-position) (line-end-position)
                              '(sub-buffer-track nil)))))

(cl-defmethod sub-buffer--line-break (&context (major-mode sub-buffer-org-mode))
  "Continue the Org list at point with a new, adjacent item."
  (if (org-at-item-p)
      (let ((org-blank-before-new-entry '((plain-list-item . nil))))
        (org-insert-item (org-at-item-checkbox-p)))
    (cl-call-next-method)))

(cl-defmethod sub-buffer--check-server (record &context (major-mode sub-buffer-org-mode))
  "Reject a missing or mismatched playlist server.
When RECORD is non-nil, record the configured server if the header is missing."
  (let ((configured (sub-buffer--server)))
    (save-restriction
      (widen)
      (let ((servers (cdr (assoc "SUB_BUFFER_SERVER"
                                 (org-collect-keywords '("SUB_BUFFER_SERVER"))))))
        (unless servers
          (unless record (user-error "Playlist has no SUB_BUFFER_SERVER header"))
          (save-excursion
            (goto-char (point-min))
            (insert-before-markers "#+SUB_BUFFER_SERVER: " sub-buffer-server "\n")))
        (dolist (server servers)
          (unless (equal (string-trim-right server "/+") configured)
            (user-error "Playlist belongs to %s; set sub-buffer-server first" server)))))))

(defun sub-buffer-org--follow (_path _argument)
  "Play the track link at point in this playlist."
  (unless (derived-mode-p 'sub-buffer-org-mode) (sub-buffer-org-mode))
  (sub-buffer-play))

(defun sub-buffer-org-tab ()
  "Complete a draft query, or use Org folding on an existing entry or heading."
  (interactive)
  (if (or (sub-buffer--entry) (org-at-heading-p) (org-at-table-p)
          (org-in-src-block-p))
      (org-cycle)
    (completion-at-point)))

(defvar sub-buffer-org-mode-map
  (let ((map (copy-keymap sub-buffer-mode-map)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "TAB") #'sub-buffer-org-tab)
    (define-key map (kbd "C-c C-o") #'org-open-at-point)
    (define-key map (kbd "M-<up>") #'org-metaup)
    (define-key map (kbd "M-<down>") #'org-metadown)
    map))

;;;###autoload
(define-derived-mode sub-buffer-org-mode org-mode "sub-buffer Org"
  "Edit and play a persistent Org playlist.
Search and playback use the usual sub-buffer bindings.  Track links hold
IDs in plain text; saving and reopening the file preserves the queue.
Use M-x sub-buffer-new for a fresh unsaved queue.
Org headings, notes and list formatting remain editable.  C-c C-o plays
a track link, and M-up/M-down reorder Org items.

\\{sub-buffer-org-mode-map}"
  (when (= (point-min) (point-max))
    (insert "#+title: " (if buffer-file-name (file-name-base buffer-file-name) "Playlist") "\n")
    (when sub-buffer-server (insert "#+SUB_BUFFER_SERVER: " sub-buffer-server "\n"))
    (insert "\n"))
  (sub-buffer--setup-queue)
  (unless buffer-file-name
    (let ((directory (sub-buffer--playlist-directory)))
      (make-directory directory t)
      (setq-local default-directory (file-name-as-directory directory)))))

(org-link-set-parameters "sub-buffer" :follow #'sub-buffer-org--follow)

(provide 'sub-buffer-org)
;;; sub-buffer-org.el ends here
