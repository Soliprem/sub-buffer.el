;;; sub-buffer.el --- An editable Subsonic queue -*- lexical-binding: t; -*-
;;; Homepage: https://github.com/Soliprem/sub-buffer.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: multimedia

;;; Commentary:

;; Set `sub-buffer-server', then run M-x sub-buffer.
;; Type a query on a line and press TAB.  Completed lines are tracks;
;; ordinary kill/yank and undo edit the queue.  mpv handles playback.

;;; Code:

(require 'auth-source)
(require 'button)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar org-directory)
(defvar doom-localleader-key)
(defvar doom-localleader-alt-key)
(declare-function evil-local-set-key "evil-core" (state key def))
(declare-function general-define-key "general" (&rest args))

;; Manual installs don't generate autoloads, and `magic-mode-alist' below
;; needs this before sub-buffer-org is loaded.
(autoload 'sub-buffer-org-mode "sub-buffer-org" nil t)

(defgroup sub-buffer nil "An editable Subsonic queue." :group 'multimedia)

(defcustom sub-buffer-server nil
  "Subsonic base URL, including any path prefix, without /rest."
  :type '(choice (const nil) string))

(defcustom sub-buffer-user nil
  "Subsonic username, or nil to choose a login from auth-source."
  :type '(choice (const nil) string))

(defcustom sub-buffer-use-org nil
  "Use an Org queue with persistent track links when non-nil.
Set this before opening a queue.  Org is loaded only when enabled."
  :type 'boolean)

(defcustom sub-buffer-playlist-directory nil
  "Directory for local Org playlists.
When nil, use the playlists subdirectory of `org-directory'.
Use `sub-buffer-open-playlist' to open or create one."
  :type '(choice (const :tag "Under org-directory" nil) directory))

(defun sub-buffer--playlist-directory ()
  "Return the playlist directory, loading Org to resolve its default."
  (expand-file-name
   (or sub-buffer-playlist-directory
       (progn
         (require 'org)
         (file-name-concat org-directory "playlists/")))))

(defvar sub-buffer--credentials nil "Cached (SERVER USER PASSWORD).")
;; Playback state.  `sub-buffer-stop' resets everything listed in
;; `sub-buffer--player-variables', so add new playback state there too.
(defvar-local sub-buffer--process nil "The mpv process playing this queue.")
(defvar-local sub-buffer--ipc nil "The connection to mpv's IPC socket.")
(defvar-local sub-buffer--ipc-directory nil "Temporary directory holding mpv's socket.")
(defvar-local sub-buffer--timer nil "Timer polling mpv for progress.")
(defvar-local sub-buffer--current nil
  "The playing entry, as returned by `sub-buffer--entry'.")
(defvar-local sub-buffer--position nil
  "Marker where the playing entry started, to continue after deleting it.")
(defvar-local sub-buffer--indicator nil "Marker for the playing entry's arrow.")
(defvar-local sub-buffer--paused nil "Non-nil when mpv is paused.")
(defvar-local sub-buffer--elapsed 0 "Seconds played of the current track.")
(defvar-local sub-buffer--duration nil "Length of the current track in seconds.")
(defconst sub-buffer--player-variables
  '(sub-buffer--process sub-buffer--ipc sub-buffer--ipc-directory sub-buffer--timer
    sub-buffer--current sub-buffer--position sub-buffer--indicator
    sub-buffer--paused sub-buffer--elapsed sub-buffer--duration)
  "Playback state that `sub-buffer-stop' resets to its defaults.")
(add-to-list 'overlay-arrow-variable-list 'sub-buffer--indicator)
(put 'sub-buffer--indicator 'overlay-arrow-string "▶")

(defvar-local sub-buffer--album-songs nil "Tracks shown in an album buffer.")
(defvar-local sub-buffer--queue nil "The queue an album buffer adds to.")
(defvar-local sub-buffer--completion nil
  "Pending completion, as (ANCHOR-MARKER . CANDIDATES).")
(defvar sub-buffer--hscroll-state nil
  "The (BUFFER . AUTO-HSCROLL-MODE) to restore when completion ends.")

(defun sub-buffer--queue-p ()
  "Return non-nil in a queue buffer, plain or Org."
  (derived-mode-p 'sub-buffer-mode 'sub-buffer-org-mode))

(defun sub-buffer--queues ()
  "Return the queue buffers, most recently visited first."
  (cl-remove-if-not (lambda (buffer) (with-current-buffer buffer (sub-buffer--queue-p)))
                    (buffer-list)))

(defun sub-buffer--setup-keybindings ()
  "Use Evil and localleader when they are available in a queue buffer.
Evil bindings are buffer-local so they take precedence over minor modes
such as evil-org, which binds < and > in Org buffers."
  (when (or (sub-buffer--queue-p) (derived-mode-p 'sub-buffer-album-mode))
    (let ((evil (bound-and-true-p evil-local-mode))
          (album (derived-mode-p 'sub-buffer-album-mode)))
      (when (and evil (fboundp 'evil-local-set-key))
        (evil-local-set-key 'normal (kbd "RET")
                            (if album #'push-button #'sub-buffer-play))
        (unless album
          ;; Use the mode's own TAB command, which differs in Org queues.
          (evil-local-set-key 'normal (kbd "TAB")
                              (lookup-key (current-local-map) (kbd "TAB")))
          (evil-local-set-key 'normal (kbd "K") #'sub-buffer-go-to-album)
          (evil-local-set-key 'normal (kbd "<") #'sub-buffer-seek-backward)
          (evil-local-set-key 'normal (kbd ">") #'sub-buffer-seek-forward)))
      (when (and (boundp 'doom-localleader-key) (stringp doom-localleader-key)
                 (fboundp 'general-define-key))
        (apply #'general-define-key
               :states (when evil '(normal visual motion emacs insert))
               :major-modes t :keymaps (intern (concat (symbol-name major-mode) "-map"))
               :prefix doom-localleader-key
               :non-normal-prefix (if (boundp 'doom-localleader-alt-key)
                                      doom-localleader-alt-key
                                    doom-localleader-key)
               (if album (list "a" #'sub-buffer-album-add)
                 (list "a" #'sub-buffer-search "p" #'sub-buffer-previous
                       "SPC" #'sub-buffer-pause "n" #'sub-buffer-next
                       "s" #'sub-buffer-switch "S" #'sub-buffer-stop
                       "RET" #'sub-buffer-newline "d" #'sub-buffer-remove "e" #'sub-buffer-edit
                       "k" #'sub-buffer-move-up "j" #'sub-buffer-move-down
                       "o" #'sub-buffer-go-to-album "b" #'sub-buffer-add-album
                       "l" #'sub-buffer-add-playlist "N" #'sub-buffer-new)))))))

;; Global `evil-mode' turns Evil on only after the major mode has started.
(with-eval-after-load 'evil
  (add-hook 'evil-local-mode-hook #'sub-buffer--setup-keybindings))

(defun sub-buffer--server ()
  "Return `sub-buffer-server' without trailing slashes, after validating it."
  (unless (and (stringp sub-buffer-server) (not (string-empty-p sub-buffer-server)))
    (user-error "Set sub-buffer-server first"))
  (let ((server (url-generic-parse-url sub-buffer-server)))
    (unless (and (member (url-type server) '("http" "https")) (url-host server)
                 (not (url-user server)) (not (url-target server))
                 (not (string-match-p "[?]" (url-filename server))))
      (user-error "Use an HTTP(S) base URL without credentials or a query")))
  (string-trim-right sub-buffer-server "/+"))

(defun sub-buffer--login ()
  "Return (SERVER USER PASSWORD), looking them up in auth-source if needed.
The result is cached in `sub-buffer--credentials' until the server or
`sub-buffer-user' changes."
  (let ((server (sub-buffer--server)))
    (unless (and (equal sub-buffer-server (car sub-buffer--credentials))
                 (or (null sub-buffer-user)
                     (equal sub-buffer-user (cadr sub-buffer--credentials))))
      (let* ((matches (auth-source-search
                       :host (list (url-host (url-generic-parse-url server)) server)
                       :user (or sub-buffer-user t)
                       :require '(:user :secret)
                       :max (if sub-buffer-user 1 most-positive-fixnum)))
             (users (delete-dups (mapcar (lambda (entry) (plist-get entry :user)) matches)))
             (user (or sub-buffer-user
                       (if (cdr users) (completing-read "Subsonic login: " users nil t)
                         (car users))))
             (auth (cl-find user matches :test #'equal
                            :key (lambda (entry) (plist-get entry :user))))
             (secret (plist-get auth :secret)))
        (unless (and (stringp user) (not (string-empty-p user)))
          (user-error "Set sub-buffer-user or add a matching authinfo login"))
        (setq sub-buffer--credentials
              (list sub-buffer-server user
                    (or (if (functionp secret) (funcall secret) secret)
                        (read-passwd "Subsonic password: "))))))
    sub-buffer--credentials))

(defun sub-buffer--url (endpoint &optional parameters)
  "Build an authenticated URL for ENDPOINT with PARAMETERS."
  (pcase-let* ((`(,_ ,user ,password) (sub-buffer--login))
               (salt (md5 (format "%s:%s:%s" (current-time) (random) (emacs-pid))))
               (token (md5 (encode-coding-string (concat password salt) 'utf-8)))
               (params (append `(("u" . ,user) ("t" . ,token)
                                 ("s" . ,salt) ("v" . "1.16.1")
                                 ("c" . "sub-buffer") ("f" . "json"))
                               parameters)))
    (concat (sub-buffer--server) "/rest/" endpoint
            "?" (mapconcat
                  (lambda (pair)
                    (concat (url-hexify-string (car pair)) "="
                            (url-hexify-string (cdr pair))))
                  params "&"))))

;;;###autoload
(defun sub-buffer-switch-login ()
  "Choose a login for subsequent requests to `sub-buffer-server'.
Override `sub-buffer-user' for this session.  Canceling keeps the current login."
  (interactive)
  (let ((credentials (let ((sub-buffer-user nil) (sub-buffer--credentials nil))
                       (sub-buffer--login))))
    (setq sub-buffer-user (cadr credentials)
          sub-buffer--credentials credentials)
    (message "Sub-buffer login: %s" sub-buffer-user)))

(defun sub-buffer--request (endpoint parameters)
  "Return the Subsonic response for ENDPOINT and PARAMETERS."
  ;; ponytail: bounded synchronous search; use async if latency gets in the way.
  (let* ((url-show-status nil)
         (url-request-method "GET")
         (url-request-data nil)
         (url-request-extra-headers nil)
         (buffer (url-retrieve-synchronously
                  (sub-buffer--url endpoint parameters) t t 5)))
    (unless buffer (user-error "Subsonic request timed out"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and (integerp url-http-response-status)
                       (<= 200 url-http-response-status 299)
                       url-http-end-of-headers)
            (user-error "Subsonic HTTP error: %s" url-http-response-status))
          (goto-char url-http-end-of-headers)
          (let* ((json (condition-case nil
                           (json-parse-buffer :object-type 'alist :array-type 'list)
                         (error (user-error "Invalid JSON from Subsonic"))))
                 (response (alist-get 'subsonic-response json)))
            (unless (equal (alist-get 'status response) "ok")
              (user-error "Subsonic error %s: %s"
                          (alist-get 'code (alist-get 'error response))
                          (alist-get 'message (alist-get 'error response))))
            response))
      (kill-buffer buffer))))

(defun sub-buffer--label (item &optional kind)
  "Return a readable label for ITEM of KIND."
  (replace-regexp-in-string
   "[[:cntrl:]]" " "
   (pcase kind
     ('artist (format "[Artist] %s" (alist-get 'name item)))
     ('album (format "[Album] %s — %s" (or (alist-get 'artist item) "Unknown artist")
                     (alist-get 'name item)))
     ('playlist (format "[Playlist] %s" (alist-get 'name item)))
     (_ (concat (format "%s — %s (%s)"
                        (or (alist-get 'artist item) "Unknown artist")
                        (or (alist-get 'title item) "Untitled")
                        (or (alist-get 'album item) "Unknown album"))
                (when-let* ((composer (alist-get 'composer item)))
                  (format " — %s" composer)))))))

(defun sub-buffer--candidates (items &optional kind)
  "Give ITEMS unique labels, retaining their KIND."
  (let (candidates)
    (dolist (item items)
      (when (and (stringp (alist-get 'id item))
                 (not (string-empty-p (alist-get 'id item))))
        (let* ((base (sub-buffer--label item kind)) (label base) (number 1))
          (while (assoc label candidates)
            (setq label (format "%s · %d" base (cl-incf number))))
          (push (cons label (cons (cons 'sub-buffer-kind kind) item)) candidates))))
    (nreverse candidates)))

(defun sub-buffer--search (query)
  "Return tracks, albums, and artists matching QUERY."
  (when (string-empty-p (string-trim query))
    (user-error "Type a track, artist, or album name first"))
  (let* ((response (sub-buffer--request
                    "search3" `(("query" . ,query) ("artistCount" . "20")
                                ("albumCount" . "20") ("songCount" . "50"))))
         (result (alist-get 'searchResult3 response)))
    (or (append (sub-buffer--candidates (alist-get 'song result))
                (sub-buffer--candidates (alist-get 'album result) 'album)
                (sub-buffer--candidates (alist-get 'artist result) 'artist))
        (user-error "No matching tracks, albums, or artists"))))

(defun sub-buffer--matches (string table predicate)
  "Match every word of STRING anywhere in TABLE's labels."
  (let* ((completion-ignore-case t)
         (words (split-string string nil t))
         (metadata (completion-metadata string table predicate))
         (query (completion-metadata-get metadata 'sub-buffer-query)))
    (cl-remove-if-not
     (lambda (label)
       ;; The server may match metadata omitted from the display label.
       (or (equal string query)
           (cl-every (lambda (word)
                       (let ((case-fold-search t))
                         (string-match-p (regexp-quote word) label)))
                     words)))
     (all-completions "" table predicate))))

(defun sub-buffer--try-completion (string table predicate point)
  "Complete STRING using word matches in TABLE at POINT."
  (let* ((matches (sub-buffer--matches string table predicate))
         (common (try-completion "" matches)))
    (cond ((member string matches) t)
          ((null common) nil)
          ((equal common "") (cons string point))
          (t (cons common (length common))))))

(defun sub-buffer--all-completions (string table predicate _point)
  "Return word matches for STRING in TABLE with a zero base size."
  (when-let* ((matches (sub-buffer--matches string table predicate)))
    (nconc matches 0)))

(add-to-list 'completion-styles-alist
             '(sub-buffer sub-buffer--try-completion sub-buffer--all-completions
                          "Match words anywhere in music metadata."))

(defun sub-buffer--read (prompt candidates)
  "Read a music candidate with PROMPT from CANDIDATES."
  (unless candidates (user-error "No matching entries"))
  (let ((completion-extra-properties '(:category sub-buffer-track)))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

(defun sub-buffer--album (id)
  "Fetch the album with ID."
  (alist-get 'album (sub-buffer--request "getAlbum" `(("id" . ,id)))))

(defun sub-buffer--songs (item)
  "Resolve a search ITEM into an ordered list of tracks."
  (pcase (alist-get 'sub-buffer-kind item)
    ('album (alist-get 'song (sub-buffer--album (alist-get 'id item))))
    ('playlist
     (alist-get 'entry (alist-get 'playlist
                        (sub-buffer--request "getPlaylist"
                                             `(("id" . ,(alist-get 'id item)))))))
    ('artist
     (let* ((artist (alist-get 'artist
                     (sub-buffer--request "getArtist" `(("id" . ,(alist-get 'id item))))))
            (album (sub-buffer--read "Choose album: "
                                    (sub-buffer--candidates (alist-get 'album artist) 'album))))
       (sub-buffer--songs album)))
    (_ (list item))))

;; A queue line holds at most one entry, (LABEL . SONG).  Plain queues
;; keep it in a text property; sub-buffer-org.el adds Org link methods.

(cl-defgeneric sub-buffer--entry ()
  "Return the unique track reference on this line, regardless of its label.
Unlinked characters are fine; joining different entries is ambiguous."
  (let ((position (line-beginning-position)) (end (line-end-position))
        entry conflict)
    (while (< position end)
      (when-let* ((reference (get-text-property position 'sub-buffer-track)))
        (if (and entry (not (eq entry reference)))
            (setq conflict t)
          (setq entry reference)))
      (setq position (next-single-property-change position 'sub-buffer-track nil end)))
    (unless conflict entry)))

(cl-defgeneric sub-buffer--line-start ()
  "Return the beginning of this line's query, after any list prefix."
  (line-beginning-position))

(cl-defgeneric sub-buffer--format (label _song)
  "Return the queue text for a line holding SONG, labeled LABEL."
  label)

(cl-defgeneric sub-buffer--line-break ()
  "Insert a line break at point for a new draft line."
  (insert "\n"))

(cl-defgeneric sub-buffer--detach ()
  "Remove this line's track reference, leaving its label as a query."
  (remove-text-properties (line-beginning-position) (line-end-position)
                          '(sub-buffer-track nil)))

(cl-defgeneric sub-buffer--check-server (_record)
  "Signal an error if this queue belongs to another server.
With RECORD non-nil, record the configured server if the queue has none."
  nil)

(defun sub-buffer--help-echo (window object position)
  "Show the full line at POSITION in OBJECT or WINDOW when hovering."
  (let ((buffer (if (bufferp object) object
                  (and (window-live-p window) (window-buffer window)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char position)
          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

(defun sub-buffer--font-lock-linked (limit)
  "Find a linked queue line before LIMIT for font locking."
  (catch 'found
    (while (< (point) limit)
      (let ((start (line-beginning-position))
            (end (min limit (line-end-position)))
            (entry (sub-buffer--entry)))
        (goto-char (min limit (1+ (line-end-position))))
        (when (and entry (< start end))
          (put-text-property start end 'help-echo #'sub-buffer--help-echo)
          (set-match-data (list start end))
          (throw 'found t))))
    nil))

(defun sub-buffer--insert (label song)
  "Replace this line with LABEL, retaining SONG as a text property."
  (let ((start (line-beginning-position))
        (text (sub-buffer--format label song)))
    (delete-region start (line-end-position))
    (goto-char start)
    (insert (propertize text 'sub-buffer-track (cons label song)
                        'rear-nonsticky '(sub-buffer-track rear-nonsticky)))))

(defun sub-buffer--accept-completion (&optional context draft)
  "Link an exact pending candidate from CONTEXT, returning non-nil on success.
With DRAFT non-nil, leave point after the new draft's list prefix;
otherwise leave it at the beginning of the first inserted track."
  (let* ((context (or context sub-buffer--completion))
         (anchor (car context))
         (label (buffer-substring-no-properties
                 (sub-buffer--line-start) (line-end-position)))
         (candidate (cdr (assoc label (cdr context)))))
    (when (and (not (sub-buffer--entry)) candidate (markerp anchor)
               (eq (marker-buffer anchor) (current-buffer))
               (= anchor (line-beginning-position)))
      ;; Resolve the collection before changing any text.
      (let ((songs (sub-buffer--songs candidate)))
        (sub-buffer--insert-songs songs t)
        (if draft
            (progn (end-of-line) (sub-buffer--open-line))
          (goto-char anchor))
        (setq sub-buffer--completion nil)
        t))))

(defun sub-buffer-accept ()
  "Accept the current completion candidate and attach its track reference."
  (interactive)
  (unless (or (sub-buffer--entry) (sub-buffer--accept-completion))
    (user-error "Choose a completion candidate first")))

(defun sub-buffer--open-line ()
  "Insert a line break at point, without carrying a track reference over."
  (let ((start (point)))
    (sub-buffer--line-break)
    (set-text-properties start (point) nil)))

(defun sub-buffer-newline ()
  "Accept a pending completion and start a draft, or insert a newline."
  (interactive)
  (unless (sub-buffer--accept-completion nil t)
    (when (sub-buffer--entry) (end-of-line))
    (sub-buffer--open-line)))

(defun sub-buffer-remove ()
  "Kill the whole current line."
  (interactive)
  (kill-whole-line))

(defun sub-buffer-edit ()
  "Detach this line's track reference to search for another recording."
  (interactive)
  (sub-buffer--detach))

(defun sub-buffer-move-up ()
  "Move this line up, retaining its track reference."
  (interactive)
  (when (= (line-beginning-position) (point-min)) (user-error "Already at the top"))
  (transpose-lines 1)
  (forward-line -2))

(defun sub-buffer-move-down ()
  "Move this line down, retaining its track reference."
  (interactive)
  (when (= (line-end-position) (point-max)) (user-error "Already at the bottom"))
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun sub-buffer--insert-songs (songs &optional replace)
  "Insert SONGS after this line, or replace this line when REPLACE is non-nil."
  (unless songs (user-error "This album or playlist has no tracks"))
  (atomic-change-group
    (sub-buffer--check-server t)
    (unless (or replace (= (sub-buffer--line-start) (line-end-position)))
      (end-of-line)
      (sub-buffer--open-line))
    (let ((first t))
      (pcase-dolist (`(,label . ,song) (sub-buffer--candidates songs))
        (if first (setq first nil) (sub-buffer--open-line))
        (sub-buffer--insert label song)))))

(defun sub-buffer--completion-hscroll ()
  "Suspend automatic horizontal scrolling until completion ends.
Necessary for corfu-like completors, which break with auto hscroll"
  ;; Completion mode is global and can end from a different buffer.
  (cond
   ((and completion-in-region-mode (sub-buffer--queue-p) (not sub-buffer--hscroll-state))
    (setq sub-buffer--hscroll-state (cons (current-buffer) auto-hscroll-mode))
    (setq-local auto-hscroll-mode nil))
   ((and (not completion-in-region-mode) sub-buffer--hscroll-state)
    (let ((buffer (car sub-buffer--hscroll-state))
          (scroll (cdr sub-buffer--hscroll-state)))
      (setq sub-buffer--hscroll-state nil)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local auto-hscroll-mode scroll)))))))

(add-hook 'completion-in-region-mode-hook #'sub-buffer--completion-hscroll)

(defun sub-buffer--completion-at-point ()
  "Complete a draft line against Subsonic."
  (unless (or (sub-buffer--entry)
              (string-empty-p
               (string-trim (buffer-substring-no-properties
                             (sub-buffer--line-start) (line-end-position)))))
    (let* ((start (sub-buffer--line-start))
           (end (line-end-position))
           (query (buffer-substring-no-properties start end))
           (candidates (sub-buffer--search query))
           (origin (current-buffer))
           (anchor (copy-marker (line-beginning-position)))
           (context (cons anchor candidates)))
      (setq sub-buffer--completion context)
      (list start end
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata (category . sub-buffer-track)
                             (sub-buffer-query . ,query))
                (complete-with-action action candidates string predicate)))
            :exclusive 'yes
            :exit-function
            (lambda (label status)
              (when (memq status '(finished sole))
                (when (and (assoc label candidates) (buffer-live-p origin))
                  (with-current-buffer origin
                    (save-excursion
                      (goto-char anchor)
                      (sub-buffer--accept-completion context))))))))))

(defun sub-buffer-search (query)
  "Search QUERY and insert a track or an album at point."
  (interactive (list (read-string "Search Subsonic: ")))
  (sub-buffer--insert-songs
   (sub-buffer--songs (sub-buffer--read "Insert: " (sub-buffer--search query)))))

(defun sub-buffer-add-playlist ()
  "Choose a server playlist and insert its tracks into the queue."
  (interactive)
  (sub-buffer--insert-songs
   (sub-buffer--songs
    (sub-buffer--read "Insert playlist: "
                     (sub-buffer--candidates
                      (alist-get 'playlist
                                 (alist-get 'playlists (sub-buffer--request "getPlaylists" nil)))
                      'playlist)))))

(defun sub-buffer--album-at-point ()
  "Return the album of the track at point."
  (sub-buffer--check-server nil)
  (let* ((entry (sub-buffer--entry))
         ;; Entries read back from a saved playlist hold only the track ID.
         (id (or (alist-get 'albumId (cdr entry))
                 (when entry
                   (alist-get 'albumId
                              (alist-get 'song
                                         (sub-buffer--request
                                          "getSong" `(("id" . ,(alist-get 'id (cdr entry)))))))))))
    (unless id (user-error "This entry has no album reference"))
    (sub-buffer--album id)))

(defun sub-buffer-add-album ()
  "Replace the current track with its entire album, in album order."
  (interactive)
  (sub-buffer--insert-songs (alist-get 'song (sub-buffer--album-at-point)) t))

(defun sub-buffer-album-add ()
  "Add this album view's tracks to its queue."
  (interactive)
  (unless (buffer-live-p sub-buffer--queue) (user-error "The queue was closed"))
  (let ((songs sub-buffer--album-songs))
    (with-current-buffer sub-buffer--queue (sub-buffer--insert-songs songs)))
  (message "Album added to queue"))

(defvar sub-buffer-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'sub-buffer-album-add)
    map))

(define-derived-mode sub-buffer-album-mode special-mode "sub-buffer Album"
  "Browse an album.  RET adds a track; a adds the whole album."
  (setq-local truncate-lines t)
  (setq-local header-line-format " RET: add track   a: add album   q: close")
  (sub-buffer--setup-keybindings))

(defun sub-buffer-go-to-album ()
  "Show the album of the current entry, with actions to enqueue tracks."
  (interactive)
  (let ((album (sub-buffer--album-at-point)) (queue (current-buffer)))
    (with-current-buffer (get-buffer-create "*sub-buffer-album*")
      (sub-buffer-album-mode)
      (setq sub-buffer--queue queue sub-buffer--album-songs (alist-get 'song album))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (sub-buffer--label album 'album) "\n\n")
        (dolist (song sub-buffer--album-songs)
          (insert-text-button
           (sub-buffer--label song) 'follow-link t 'help-echo #'sub-buffer--help-echo
           'action (lambda (_button)
                     (unless (buffer-live-p queue) (user-error "The queue was closed"))
                     (with-current-buffer queue (sub-buffer--insert-songs (list song)))
                     (message "Track added to queue")))
          (insert "\n")))
      (goto-char (point-min)))
    (pop-to-buffer "*sub-buffer-album*")))

(defun sub-buffer--current-line ()
  "Return the position of the playing entry in the edited queue."
  (when sub-buffer--current
    (save-excursion
      (goto-char (point-min))
      (let (found)
        ;; ponytail: duplicate current entries use the first occurrence.
        (while (and (not (eobp)) (not found))
          (if (eq (sub-buffer--entry) sub-buffer--current)
              (setq found (point))
            (forward-line 1)))
        found))))

(defun sub-buffer--time (seconds)
  "Format SECONDS as minutes and seconds, or an unknown duration."
  (if (numberp seconds)
      (format-seconds "%m:%02s" (max 0 (truncate seconds)))
    "--:--"))

(defun sub-buffer--status ()
  "Describe playback for the mode line."
  (if sub-buffer--current
      (format "%s %s / %s: %s" (if sub-buffer--paused "Paused" "Playing")
              (sub-buffer--time sub-buffer--elapsed)
              (sub-buffer--time sub-buffer--duration)
              (truncate-string-to-width (car sub-buffer--current) 40 nil nil "…"))
    "Stopped"))

(defun sub-buffer--update-indicator (&rest _)
  "Move the arrow to the playing entry, which edits may have moved.
This scans the queue, so it runs after edits rather than on every poll."
  (when sub-buffer--current
    (unless sub-buffer--indicator
      (setq sub-buffer--indicator (make-marker)))
    (set-marker sub-buffer--indicator (sub-buffer--current-line))
    (force-mode-line-update)))

(defun sub-buffer--ipc-send (command &optional request-id)
  "Send COMMAND with REQUEST-ID to mpv's IPC connection."
  (when (process-live-p sub-buffer--ipc)
    (process-send-string
     sub-buffer--ipc
     (concat (json-serialize `((command . ,(vconcat command))
                               (request_id . ,(or request-id 0)))
                             :false-object :false)
             "\n"))))

(defun sub-buffer--ipc-filter (process chunk)
  "Parse complete mpv JSON replies from PROCESS and CHUNK."
  (let ((pending (concat (or (process-get process 'pending) "") chunk))
        (origin (process-get process 'queue)))
    (while (string-match "\n" pending)
      (let* ((end (match-end 0))
             (line (substring pending 0 end)))
        (setq pending (substring pending end))
        (when (buffer-live-p origin)
          (with-current-buffer origin
            (when (eq process sub-buffer--ipc)
              (let ((reply (condition-case nil
                               (json-parse-string line :object-type 'alist
                                                  :null-object nil :false-object :false)
                             (error nil))))
                (when (equal (alist-get 'error reply) "success")
                  ;; Request IDs are assigned in `sub-buffer--poll'.
                  (let ((data (alist-get 'data reply)))
                    (pcase (alist-get 'request_id reply)
                      (1 (when (numberp data) (setq sub-buffer--elapsed data)))
                      (2 (when (numberp data) (setq sub-buffer--duration data)))
                      (3 (setq sub-buffer--paused (eq data t)))))
                  (force-mode-line-update))))))))
    (process-put process 'pending pending)))

(defun sub-buffer--poll (origin)
  "Connect to mpv as it starts, then poll playback state for ORIGIN."
  (when (buffer-live-p origin)
    (with-current-buffer origin
      (when (process-live-p sub-buffer--process)
        (unless (process-live-p sub-buffer--ipc)
          (setq sub-buffer--ipc
                (condition-case nil
                    (make-network-process
                     :name "sub-buffer-ipc" :family 'local
                     :service (expand-file-name "mpv.sock" sub-buffer--ipc-directory)
                     :coding 'utf-8-unix :noquery t
                     :filter #'sub-buffer--ipc-filter :sentinel #'ignore)
                  (file-error nil)))
          (when sub-buffer--ipc (process-put sub-buffer--ipc 'queue origin)))
        (when (process-live-p sub-buffer--ipc)
          (sub-buffer--ipc-send '("get_property" "time-pos") 1)
          (sub-buffer--ipc-send '("get_property" "duration") 2)
          (sub-buffer--ipc-send '("get_property" "pause") 3))))))

(defun sub-buffer-stop ()
  "Stop playback without advancing the queue."
  (interactive)
  (when (timerp sub-buffer--timer) (cancel-timer sub-buffer--timer))
  (when (process-live-p sub-buffer--ipc) (delete-process sub-buffer--ipc))
  (when sub-buffer--indicator (set-marker sub-buffer--indicator nil))
  (when sub-buffer--process
    (set-process-sentinel sub-buffer--process #'ignore)
    (when (process-live-p sub-buffer--process)
      (delete-process sub-buffer--process)))
  (when sub-buffer--ipc-directory (delete-directory sub-buffer--ipc-directory t))
  (when sub-buffer--position (set-marker sub-buffer--position nil))
  (mapc #'kill-local-variable sub-buffer--player-variables)
  (force-mode-line-update))

(defun sub-buffer-play ()
  "Play the completed track on this line."
  (interactive)
  (sub-buffer--check-server nil)
  (unless (sub-buffer--entry) (sub-buffer--accept-completion))
  (let ((entry (sub-buffer--entry))
        (mpv (executable-find "mpv")))
    (unless entry (user-error "Accept a completion first (RET or C-c RET)"))
    (unless mpv (user-error "Install mpv to play tracks"))
    (let ((url (sub-buffer--url "stream" `(("id" . ,(alist-get 'id (cdr entry))))))
          (origin (current-buffer)))
      (sub-buffer-stop)
      (setq sub-buffer--ipc-directory (make-temp-file "sub-buffer-" t))
      (condition-case err
          (setq sub-buffer--process
            (make-process
             :name "sub-buffer-mpv" :buffer nil :noquery t :connection-type 'pipe
             :command (list mpv "--no-video" "--no-terminal" "--really-quiet"
                            "--idle=no" "--keep-open=no"
                            (concat "--input-ipc-server="
                                    (expand-file-name "mpv.sock" sub-buffer--ipc-directory))
                            "--" url)
             :sentinel
             (lambda (process _event)
               (when (and (buffer-live-p origin)
                          (memq (process-status process) '(exit signal)))
                 (with-current-buffer origin
                   (when (eq process sub-buffer--process)
                     (if (and (eq (process-status process) 'exit)
                              (zerop (process-exit-status process)))
                         (condition-case err (sub-buffer-next)
                           (error (sub-buffer-stop)
                                  (message "%s" (error-message-string err))))
                       (sub-buffer-stop)
                       (message "mpv playback failed (exit %s)"
                                (process-exit-status process)))))))))
        (error (sub-buffer-stop) (signal (car err) (cdr err))))
      (setq sub-buffer--current entry
            sub-buffer--position (copy-marker (line-beginning-position) t)
            sub-buffer--duration (alist-get 'duration (cdr entry))
            sub-buffer--timer (run-at-time 0.1 0.5 #'sub-buffer--poll origin))
      (sub-buffer--update-indicator))))

(defun sub-buffer-next ()
  "Play the next completed line, following the buffer's current order."
  (interactive)
  (save-excursion
    (if sub-buffer--current
        (if-let* ((found (sub-buffer--current-line)))
            (progn (goto-char found) (forward-line 1))
          ;; A deleted entry's marker lands at the next remaining line.
          (goto-char sub-buffer--position))
      (forward-line 1))
    (while (and (not (eobp)) (not (sub-buffer--entry))) (forward-line 1))
    (if (and (not (eobp)) (sub-buffer--entry))
        (sub-buffer-play)
      (sub-buffer-stop))))

(defun sub-buffer-previous ()
  "Play the previous completed line, following the buffer's current order."
  (interactive)
  (save-excursion
    (when sub-buffer--current
      (goto-char (or (sub-buffer--current-line) sub-buffer--position)))
    (unless (zerop (forward-line -1)) (user-error "No previous track"))
    (while (and (not (bobp)) (not (sub-buffer--entry))) (forward-line -1))
    (unless (sub-buffer--entry) (user-error "No previous track"))
    (sub-buffer-play)))

(defun sub-buffer--connect ()
  "Connect to the playing mpv, or signal that it can't take commands yet."
  (unless (process-live-p sub-buffer--process) (user-error "Nothing is playing"))
  (sub-buffer--poll (current-buffer))
  (unless (process-live-p sub-buffer--ipc) (user-error "Player is still starting; try again")))

(defun sub-buffer-pause ()
  "Toggle mpv's pause state."
  (interactive)
  (sub-buffer--connect)
  (sub-buffer--ipc-send (list "set_property" "pause" (if sub-buffer--paused :false t)))
  (setq sub-buffer--paused (not sub-buffer--paused))
  (force-mode-line-update))

(defun sub-buffer-seek (seconds)
  "Seek SECONDS relative to the current playback position."
  (interactive "nSeek seconds (negative for backward): ")
  (sub-buffer--connect)
  (sub-buffer--ipc-send (list "seek" seconds "relative+exact")))

(defun sub-buffer-seek-forward ()
  "Seek ten seconds forward."
  (interactive)
  (sub-buffer-seek 10))

(defun sub-buffer-seek-backward ()
  "Seek ten seconds backward."
  (interactive)
  (sub-buffer-seek -10))

(defvar sub-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "RET") #'sub-buffer-newline)
    (define-key map (kbd "C-c RET") #'sub-buffer-accept)
    (define-key map (kbd "C-c C-a") #'sub-buffer-search)
    (define-key map (kbd "C-c C-c") #'sub-buffer-play)
    (define-key map (kbd "C-c C-n") #'sub-buffer-next)
    (define-key map (kbd "C-c C-p") #'sub-buffer-previous)
    (define-key map (kbd "C-c SPC") #'sub-buffer-pause)
    (define-key map (kbd "C-c C-s") #'sub-buffer-stop)
    (define-key map (kbd "C-c <left>") #'sub-buffer-seek-backward)
    (define-key map (kbd "C-c <right>") #'sub-buffer-seek-forward)
    (define-key map (kbd "C-c C-k") #'sub-buffer-remove)
    (define-key map (kbd "C-c C-e") #'sub-buffer-edit)
    (define-key map (kbd "M-<up>") #'sub-buffer-move-up)
    (define-key map (kbd "M-<down>") #'sub-buffer-move-down)
    (define-key map (kbd "C-c C-b") #'sub-buffer-add-album)
    (define-key map (kbd "C-c C-o") #'sub-buffer-go-to-album)
    (define-key map (kbd "C-c C-l") #'sub-buffer-add-playlist)
    map))

(define-derived-mode sub-buffer-mode fundamental-mode "sub-buffer"
  "Edit a music queue.  Type a query and press TAB to insert a track.

Use M-x sub-buffer-switch-login to choose another auth-source account.
Use M-x sub-buffer-new for a fresh unsaved queue.

With Evil active, normal-state RET accepts a pending completion and
plays the entry, TAB completes a query, and K opens the entry's album.
< and > seek backward and forward by ten seconds.
In insert state, RET accepts a completion or opens a draft line.
Evil's usual editing keys apply: dd deletes, yy yanks, p/P paste,
o/O open draft lines, and u/C-r undo and redo.

Doom localleader bindings use your configured prefix when General
is available.  Use C-h b to list active state and localleader bindings.

\{sub-buffer-mode-map}"
  (setq-local font-lock-defaults '(((sub-buffer--font-lock-linked 0 'link))))
  (setq-local font-lock-extra-managed-props '(help-echo))
  (sub-buffer--setup-queue))

(defun sub-buffer--setup-queue ()
  "Set up completion and playback shared by plain and Org queues."
  (setq-local truncate-lines t)
  (setq-local completion-at-point-functions '(sub-buffer--completion-at-point))
  (setq-local completion-ignore-case t)
  (setq-local completion-category-overrides
              (cons '(sub-buffer-track (styles sub-buffer))
                    completion-category-overrides))
  (setq-local mode-line-process '(:eval (list " [" (sub-buffer--status) "]")))
  (setq-local header-line-format nil)
  ;; Refresh after the major mode has updated its parse caches.
  (add-hook 'after-change-functions #'sub-buffer--update-indicator t t)
  (add-hook 'kill-buffer-hook #'sub-buffer-stop nil t)
  (add-hook 'change-major-mode-hook #'sub-buffer-stop nil t)
  (sub-buffer--setup-keybindings))

;;;###autoload
(defun sub-buffer ()
  "Return to the most recently visited queue, or create one if none exists."
  (interactive)
  (let ((queue (car (sub-buffer--queues))))
    (if queue
        (if (sub-buffer--queue-p)
            (switch-to-buffer queue)
          (pop-to-buffer queue))
      (sub-buffer-new))))

(defun sub-buffer-switch ()
  "Choose a queue in the current window, from a queue buffer.
Default to the playing queue, or the most recently visited queue."
  (interactive)
  (unless (sub-buffer--queue-p)
    (user-error "Switch queues from a sub-buffer queue"))
  (let* ((queues (sub-buffer--queues))
         (playing (cl-find-if
                   (lambda (buffer) (buffer-local-value 'sub-buffer--current buffer))
                   queues))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (with-current-buffer name
                    (when sub-buffer--current
                      (if sub-buffer--paused " [paused]" " [playing]")))))))
    (switch-to-buffer
     (completing-read "Queue: " (mapcar #'buffer-name queues) nil t nil nil
                      (buffer-name (or playing (car queues)))))))

;;;###autoload
(defun sub-buffer-new ()
  "Show a fresh unsaved queue in the current window, preserving other queues."
  (interactive)
  (when sub-buffer-use-org (require 'sub-buffer-org))
  (switch-to-buffer (generate-new-buffer "*sub-buffer*"))
  (if sub-buffer-use-org (sub-buffer-org-mode) (sub-buffer-mode)))

;;;###autoload
(defun sub-buffer-open-playlist (file)
  "Open or create FILE as an Org queue in `sub-buffer-playlist-directory'."
  (interactive (list (read-file-name "Playlist: " (sub-buffer--playlist-directory)
                                     nil nil "playlist.org")))
  (unless sub-buffer-use-org (user-error "Enable sub-buffer-use-org first"))
  (require 'sub-buffer-org)
  (setq file (expand-file-name file (sub-buffer--playlist-directory)))
  (unless (string-suffix-p ".org" file) (setq file (concat file ".org")))
  (make-directory (file-name-directory file) t)
  (find-file file)
  (unless (derived-mode-p 'sub-buffer-org-mode) (sub-buffer-org-mode)))

(defun sub-buffer--playlist-p ()
  "Recognize Org playlists in the configured directory."
  (and sub-buffer-use-org buffer-file-name
       (string-suffix-p ".org" buffer-file-name)
       (file-in-directory-p buffer-file-name (sub-buffer--playlist-directory))))

(add-to-list 'magic-mode-alist '(sub-buffer--playlist-p . sub-buffer-org-mode))

(provide 'sub-buffer)
;;; sub-buffer.el ends here
