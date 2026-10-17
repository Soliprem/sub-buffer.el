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

(defun sub-buffer-org--line-start ()
  "Return the beginning of this line's text after its Org list prefix."
  (save-excursion
    (beginning-of-line)
    (when (org-at-item-p)
      (goto-char (match-end 0))
      (when (looking-at "\\(?:\\[@[[:alnum:]]+\\][ \t]+\\)?\\(?:\\[[ X-]\\][ \t]+\\)?")
        (goto-char (match-end 0))))
    (point)))

(defun sub-buffer-org--format (label song)
  "Format LABEL and SONG as an Org link, keeping the current list prefix."
  (let ((prefix (buffer-substring-no-properties
                 (line-beginning-position) (sub-buffer-org--line-start))))
    (concat (if (string-empty-p prefix) "- " prefix)
            (org-link-make-string
             (concat "sub-buffer:" (url-hexify-string (alist-get 'id song))) label))))

(defun sub-buffer-org--entry ()
  "Read this line's unique track link, caching metadata only in text properties.
The saved link is authoritative; changing its ID invalidates cached metadata."
  (save-excursion
    (beginning-of-line)
    (let ((end (line-end-position)) entry conflict)
      (while (re-search-forward org-link-bracket-re end t)
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

(defun sub-buffer-org--edit ()
  "Replace this track link with its readable description for a fresh search."
  (when-let* ((entry (sub-buffer-org--entry)))
    (atomic-change-group
      (let ((start (sub-buffer-org--line-start)))
        (delete-region start (line-end-position))
        (goto-char start)
        (insert (substring-no-properties (car entry))))
      (remove-text-properties (line-beginning-position) (line-end-position)
                              '(sub-buffer-track nil)))))

(defun sub-buffer-org--check-server (&optional record)
  "Reject a missing or mismatched playlist server.
When RECORD is non-nil, record the configured server if the header is missing."
  (unless (and (stringp sub-buffer-server) (not (string-empty-p sub-buffer-server)))
    (user-error "Set sub-buffer-server first"))
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
        (unless (equal (string-trim-right server "/+")
                       (string-trim-right sub-buffer-server "/+"))
          (user-error "Playlist belongs to %s; set sub-buffer-server first" server))))))

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
      (setq-local default-directory (file-name-as-directory directory))))
  (sub-buffer--setup-keybindings))

(org-link-set-parameters "sub-buffer" :follow #'sub-buffer-org--follow)

(provide 'sub-buffer-org)
;;; sub-buffer-org.el ends here
