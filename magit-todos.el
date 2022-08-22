;;; magit-todos.el --- Show source file TODOs in Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/magit-todos
;; Version: 1.6-pre
;; Package-Requires: ((emacs "26.1") (async "1.9.2") (dash "2.13.0") (f "0.17.2") (hl-todo "1.9.0") (magit "2.13.0") (pcre2el "1.8") (s "1.12.0") (transient "0.2.0"))
;; Keywords: magit, vc

;;; Commentary:

;; This package displays keyword entries from source code comments and Org files
;; in the Magit status buffer.  Activating an item jumps to it in its file.  By
;; default, it uses keywords from `hl-todo', minus a few (like "NOTE").

;;;; Usage:

;; Run `magit-todos-mode', then open a Magit status buffer.

;;;; Tips:

;; + You can customize settings in the `magit-todos' group.

;;; Installation:

;;;; MELPA

;; If you installed from MELPA, you're done.

;;;; Manual

;; Install these required packages:

;; async
;; dash
;; f
;; hl-todo
;; magit
;; pcre2el
;; s

;; Then put this file in your load-path, and put this in your init file:

;;   (require 'magit-todos)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'grep)
(require 'seq)

(require 'async)
(require 'dash)
(require 'f)
(require 'hl-todo)
(require 'magit)
(require 'transient)
(require 'pcre2el)
(require 's)

;;;; Structs

(cl-defstruct magit-todos-item
  filename org-level line column position keyword suffix description)

;;;; Variables

(defvar magit-todos-keywords-list nil
  "List of to-do keywords.
Set automatically by `magit-todos-keywords' customization.")

(defvar magit-todos-grep-result-regexp nil
  "Regular expression for grep results.
This should be set automatically by customizing
`magit-todos-keywords'.")

(defvar magit-todos-ag-result-regexp nil
  "Regular expression for ag results.
This should be set automatically by customizing
`magit-todos-keywords'.")

(defvar magit-todos-rg-result-regexp nil
  "Regular expression for rg results.
This should be set automatically by customizing
`magit-todos-keywords'.")

(defvar magit-todos-git-grep-result-regexp nil
  "Regular expression for git-grep results.
This should be set automatically by customizing
`magit-todos-keywords'.")

(defvar magit-todos-search-regexp nil
  "Regular expression to match keyword items with rg, ag, and git-grep.
This should be set automatically by customizing
`magit-todos-keywords'.")

(defvar-local magit-todos-active-scan nil
  "The current scan's process.
Used to avoid running multiple simultaneous scans for a
magit-status buffer.")

(defvar magit-todos-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "jT" #'magit-todos-jump-to-todos)
    (define-key map "jl" #'magit-todos-list)
    (define-key map "b" #'magit-todos-branch-list-toggle)
    (define-key map "B" #'magit-todos-branch-list-set-commit)
    (define-key map [remap magit-visit-thing] #'magit-todos-list)
    map)
  "Keymap for `magit-todos' top-level section.")

(defvar magit-todos-item-section-map
  (let ((map (copy-keymap magit-todos-section-map)))
    (define-key map [remap magit-visit-thing] #'magit-todos-jump-to-item)
    (define-key map [remap magit-diff-show-or-scroll-up] #'magit-todos-peek-at-item)
    map)
  "Keymap for `magit-todos' individual to-do item sections.
See https://magit.vc/manual/magit/Creating-Sections.html for more
details about how section maps work.")

(defvar-local magit-todos-show-filenames nil
  "Whether to show filenames next to to-do items.
Set automatically depending on grouping.")

(defvar-local magit-todos-updating nil
  "Whether items are being updated now.")

(defvar-local magit-todos-last-update-time nil
  "When the items were last updated.
A time value as returned by `current-time'.")

(defvar-local magit-todos-item-cache nil
  "Items found by most recent scan.")

(defvar magit-todos-scanners nil
  "Scanners defined by `magit-todos-defscanner'.")

(defvar magit-todos-section-heading "TODOs"
  "Allows overriding of section heading.")

;;;; Customization

(defgroup magit-todos nil
  "Show TODO items in source code comments in repos' files."
  :group 'magit)

(defcustom magit-todos-scanner nil
  "File scanning method.
\"Automatic\" will attempt to use rg, ag, git-grep, and
find-grep, in that order. "
  :type '(choice (const :tag "Automatic" nil)
                 (function :tag "Custom function"))
  :set (lambda (option value)
         (when magit-todos-scanners
           ;; Only try to set when scanners are defined.
           (unless value
             ;; Choosing automatically
             (setq value (or (magit-todos--choose-scanner)
                             (progn
                               (display-warning 'magit-todos
                                                "`magit-todos' was unable to find a suitable scanner.  Please install \"rg\", or a PCRE-compatible version of \"git\" or \"grep\".  Disabling `magit-todos-mode'."
                                                :error)
                               (magit-todos-mode -1)
                               nil))))
           (set-default option value))))

(defcustom magit-todos-nice t
  "Run scanner with \"nice\"."
  :type 'boolean)

(defcustom magit-todos-ignore-case nil
  "Upcase keywords found in files.
If nil, a keyword like \"Todo:\" will not be shown.  `upcase' can
be a relatively expensive function, so this can be disabled if
necessary."
  :type 'boolean)

(defcustom magit-todos-update t
  "When or how often to scan for to-dos.
When set to manual updates, the list can be updated with the
command `magit-todos-update'.  When caching is enabled, scan for
items whenever the Magit status buffer is refreshed and at least
N seconds have passed since the last scan; otherwise, use cached
items."
  :type '(choice (const :tag "Automatically, when the Magit status buffer is refreshed" t)
                 (integer :tag "Automatically, but cache items for N seconds")
                 (const :tag "Manually" nil)))

(defcustom magit-todos-fontify-keyword-headers t
  "Apply keyword faces to group keyword headers."
  :type 'boolean)

(defcustom magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")") ":")
  "Regular expression matching suffixes after keywords.
e.g. to match a keyword like \"TODO(user):\", use \"([^)]+):\".

If the suffix should be optional, the entire regexp should be
made explicitly optional.  However, it is not necessary to
account for optional whitespace after the suffix, as this is done
automatically.

Note: the suffix applies only to non-Org files."
  :type `(choice (const :tag "Optional username in parens, then required colon (matching e.g. \"TODO:\" or \"TODO(user):\")"
                        ,(rx (optional "(" (1+ (not (any ")"))) ")") ":"))
                 (const :tag "Required colon (matching e.g. \"TODO:\"" ":")
                 (string :tag "Custom regexp"))
  :package-version '(magit-todos . "1.2"))

(defcustom magit-todos-ignored-keywords '("NOTE" "DONE")
  "Ignored keywords.  Automatically removed from `magit-todos-keywords'."
  :type '(repeat string)
  :set (lambda (option value)
         (set-default option value)
         (when (boundp 'magit-todos-keywords)
           ;; Avoid setting `magit-todos-keywords' before it's defined.

           ;; HACK: Testing with `fboundp' is the only way I have been able to find that fixes this
           ;; problem.  I tried using ":set-after '(magit-todos-ignored-keywords)" on
           ;; `magit-todos-keywords', but it had no effect.  I looked in the manual, which seems to
           ;; suggest that using ":initialize 'custom-initialize-safe-set" might fix it--but that
           ;; function is no longer to be found in the Emacs source tree.  It was committed in 2005,
           ;; and now it's gone, but the manual still mentions it. ???
           (custom-reevaluate-setting 'magit-todos-keywords))))

(defcustom magit-todos-keywords 'hl-todo-keyword-faces
  "To-do keywords to display in Magit status buffer.
If set to a list variable, may be a plain list or an alist in
which the keys are the keywords.

When set, sets `magit-todos-search-regexp' to the appropriate
regular expression."
  :type '(choice (repeat :tag "Custom list" string)
                 (const :tag "Keywords from `hl-todo'" hl-todo-keyword-faces)
                 (variable :tag "List variable"))
  :set (lambda (option value)
         (set-default option value)
         (let ((keywords (cl-typecase value
                           (null (user-error "Please add some keywords"))
                           (symbol (if (and (consp (symbol-value value))
                                            (consp (car (symbol-value value))))
                                       (mapcar #'car (symbol-value value))
                                     (symbol-value value)))
                           (list value))))
           (setq magit-todos-keywords-list (seq-difference keywords magit-todos-ignored-keywords)))))

(defcustom magit-todos-max-items 10
  "Automatically collapse the section if there are more than this many items."
  :type 'integer)

(defcustom magit-todos-auto-group-items 20
  "Whether or when to automatically group items."
  :type '(choice (integer :tag "When there are more than this many items")
                 (const :tag "Always" always)
                 (const :tag "Never" never)))

(defcustom magit-todos-buffer-item-factor 10
  "Multiply `magit-todos-auto-group-items' and `magit-todos-max-items' by this factor in dedicated `magit-todos' buffers."
  :type 'integer)

(defcustom magit-todos-group-by '(magit-todos-item-keyword magit-todos-item-filename)
  "How to group items.
One or more attributes may be chosen, and they will be grouped in
order."
  :type '(repeat (choice (const :tag "By filename" magit-todos-item-filename)
                         (const :tag "By keyword" magit-todos-item-keyword)
                         (const :tag "By suffix" magit-todos-item-suffix)
                         (const :tag "By first path component" magit-todos-item-first-path-component))))

(defcustom magit-todos-fontify-org t
  "Fontify items from Org files as Org headings."
  :type 'boolean)

(defcustom magit-todos-sort-order '(magit-todos--sort-by-keyword
                                    magit-todos--sort-by-filename
                                    magit-todos--sort-by-position)
  "Order in which to sort items."
  :type '(repeat (choice (const :tag "Keyword" magit-todos--sort-by-keyword)
                         (const :tag "Suffix" magit-todos--sort-by-suffix)
                         (const :tag "Filename" magit-todos--sort-by-filename)
                         (const :tag "Buffer position" magit-todos--sort-by-position)
                         (function :tag "Custom function"))))

(defcustom magit-todos-depth nil
  "Maximum depth of files in repo working tree to scan for to-dos.
A value of 0 means to search only the current directory, while a
value of 1 means to search directories one level deeper, etc.
Deeper scans can be slow in large projects.  You may wish to set
this in a directory-local variable for certain projects."
  :type '(choice (const :tag "Unlimited" nil)
                 (const :tag "Repo root directory only" 0)
                 (integer :tag "N levels below the repo root")))

(defcustom magit-todos-insert-at 'bottom
  "Insert the to-dos section after this section in the Magit status buffer.
Specific sections may be chosen, using the first symbol returned
by evaluating \"(magit-section-ident (magit-current-section))\"
in the status buffer with point on the desired section,
e.g. `recent' for the \"Recent commits\" section.  Note that this
may not work exactly as desired when the built-in scanner is
used."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "After untracked files" untracked)
                 (const :tag "After unstaged files" unstaged)
                 (symbol :tag "After selected section")))

(defcustom magit-todos-exclude-globs '(".git/")
  "Glob patterns to exclude from searches."
  :type '(repeat string))

(defcustom magit-todos-branch-list 'branch
  "Show branch diff to-do list.

This can be toggled locally in Magit buffers with command
`magit-todos-branch-list-toggle'."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "In non-master branches" branch)
                 (const :tag "Always" t)))

(defcustom magit-todos-branch-list-merge-base-ref "master"
  "Commit ref passed to command \"git merge-base HEAD\".
Determines the ancestor commit from which the current branch's
todos should be searched for.  May be overridden in the case that
a branch is branched off another branch rather than master.  For
example, consider the following commit graph:

---A1---A2---A3 (master)
    \
     B1---B2---B3 (topic)
           \
            C1---C2---C3 (topic2, HEAD)

By default, the branch todo list would show todos from both the
\"topic\" branch and the \"topic2\" branch.  To show only todos
from the \"topic2\" branch, this option could be set to
\"topic\"."
  :type 'string)

(defcustom magit-todos-submodule-list nil
  "Show submodule to-do list."
  :type 'boolean)

;;;; Commands

;;;###autoload
(define-minor-mode magit-todos-mode
  "Show list of to-do items in Magit status buffer for tracked files in repo."
  :require 'magit-todos
  :group 'magit-todos
  :global t
  (if magit-todos-mode
      (progn
        (transient-append-suffix #'magit-status-jump
          '(0 -1) '[("T" "Todos" magit-todos-jump-to-todos)
                    ("l" "List todos" magit-todos-list)])
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-todos--insert-todos
                                nil
                                'append)
        (add-hook 'magit-status-mode-hook #'magit-todos--add-to-status-buffer-kill-hook 'append))
    ;; Disable mode
    (transient-remove-suffix #'magit-status-jump '(0 -1))
    (remove-hook 'magit-status-sections-hook #'magit-todos--insert-todos)
    (remove-hook 'magit-status-mode-hook #'magit-todos--add-to-status-buffer-kill-hook)))

(defun magit-todos-update ()
  "Update the to-do list manually.
Only necessary when option `magit-todos-update' is nil."
  (interactive)
  (unless magit-todos-mode
    (user-error "Please activate `magit-todos-mode'"))
  (let ((inhibit-read-only t))
    ;; Delete twice since there might also be the branch-local section.
    (magit-todos--delete-section [* todos])
    (magit-todos--delete-section [* todos])
    ;; HACK: See other note on `magit-todos-updating'.
    (setq magit-todos-updating t)
    (magit-todos--insert-todos)))

(defun magit-todos-branch-list-toggle ()
  "Toggle branch diff to-do list in current Magit buffer."
  (interactive)
  (setq-local magit-todos-branch-list (not magit-todos-branch-list))
  (magit-todos-update))

(defun magit-todos-branch-list-set-commit (ref)
  "Set commit ref used in branch to-do list."
  (interactive (list (completing-read "Refname: " (magit-list-refnames))))
  (setq-local magit-todos-branch-list-merge-base-ref ref)
  (magit-todos-update))

(declare-function org-show-entry "org")
(cl-defun magit-todos-jump-to-item (&key peek (item (oref (magit-current-section) value)))
  "Show current item.
If PEEK is non-nil, keep focus in status buffer window."
  (interactive)
  (let* ((status-window (selected-window))
         (buffer (magit-todos--item-buffer item)))
    (pop-to-buffer buffer)
    (magit-todos--goto-item item)
    (when (derived-mode-p 'org-mode)
      (org-show-entry))
    (when peek
      (select-window status-window))))

(defun magit-todos-peek-at-item ()
  "Peek at current item."
  (interactive)
  (magit-todos-jump-to-item :peek t))

;;;;; Jump to section

(magit-define-section-jumper magit-jump-to-todos "TODOs" todos)

(defun magit-todos-jump-to-todos ()
  "Jump to TODOs section, and update it if empty."
  (interactive)
  (let ((already-in-section-p (magit-section-match [* todos])))
    (magit-jump-to-todos)
    (when (and (or (integerp magit-todos-update)
                   (not magit-todos-update))
               (or already-in-section-p
                   (= 0 (length (oref (magit-current-section) children)))))
      (magit-todos-update))))

;;;; Dedicated buffer

;;;###autoload
(defun magit-todos-list (&optional directory)
  "Show to-do list of the current Git repository in a buffer.
With prefix, prompt for repository."
  ;; Mostly copied from `magit-status'
  (interactive
   (let ((magit--refresh-cache (list (cons 0 0))))
     (list (and (or current-prefix-arg (not (magit-toplevel)))
                (magit-read-repository)))))
  (condition-case nil
      (let ((magit--refresh-cache (list (cons 0 0))))
        (setq directory (if directory
                            (file-name-as-directory (expand-file-name directory))
                          default-directory))
        (magit-todos-list-internal directory))
    ('magit-outside-git-repo (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) default-directory)))
                               (call-interactively #'magit-todos-list)))))

(put 'magit-todos-list 'interactive-only 'magit-todos-list-internal)

;;;###autoload
(defun magit-todos-list-internal (directory)
  "Open buffer showing to-do list of repository at DIRECTORY."
  (if (fboundp 'magit--tramp-asserts)
      (magit--tramp-asserts directory)
    (when (file-remote-p directory)
      (magit-git-version-assert)))

  (let ((default-directory directory))
    (magit-setup-buffer #'magit-todos-list-mode)))

(define-derived-mode magit-todos-list-mode magit-status-mode "Magit"
  "Mode for looking at repository to-do list.

\\<magit-todos-mode-map>\
Type \\[magit-refresh] to refresh the list.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the item at point.
Type \\[magit-diff-show-or-scroll-up] to peek at the item at point."
  :group 'magit-todos)

(defun magit-todos-list-refresh-buffer ()
  "Refresh the current `magit-todos-list-mode' buffer."
  (setq-local magit-todos-max-items (* magit-todos-max-items magit-todos-buffer-item-factor))
  (when (numberp magit-todos-auto-group-items)
    (setq-local magit-todos-auto-group-items (* magit-todos-auto-group-items magit-todos-buffer-item-factor)))
  (magit-section-show (magit-insert-section (type magit-root-section)
                        (magit-insert-status-headers)
                        (magit-todos--insert-todos))))

;;;; Functions

(defun magit-todos--coalesce-groups (groups)
  "Return GROUPS, coalescing any groups with `equal' keys.
GROUPS should be an alist.  Assumes that each group contains
unique items.  Intended for post-processing the result of
`-group-by'."
  (cl-loop with keys = (-uniq (-map #'car groups))
           for key in keys
           for matching-groups = (--select (equal key (car it)) groups)
           collect (cons key (apply #'append (-map #'cdr matching-groups)))))

(defun magit-todos--add-to-status-buffer-kill-hook ()
  "Add `magit-todos--kill-active-scan' to `kill-buffer-hook' locally."
  (add-hook 'kill-buffer-hook #'magit-todos--kill-active-scan 'append 'local))

(defun magit-todos--kill-active-scan ()
  "Kill `magit-todos-active-scan'.
To be called in status buffers' `kill-buffer-hook'."
  (when (and magit-todos-active-scan
             (process-live-p magit-todos-active-scan))
    (kill-process magit-todos-active-scan)
    (when-let* ((buffer (process-buffer magit-todos-active-scan))
                (alive (buffer-live-p buffer)))
      (kill-buffer buffer))))

(defun magit-todos--add-to-custom-type (symbol value)
  "Add VALUE to the end of SYMBOL's `custom-type' property."
  (declare (indent defun))
  (pcase-let* ((`(,type . ,choices) (get symbol 'custom-type))
               (choices (append (list value) choices)))
    (put symbol 'custom-type
         (append (list type) choices))))

(defun magit-todos--choose-scanner ()
  "Return function to call to scan for items with.
Chooses automatically in order defined in `magit-todos-scanners'."
  (cl-loop for scanner in magit-todos-scanners
           ;; I guess it would be better to avoid `eval', but it seems like the natural
           ;; way to do this.
           when (eval (alist-get 'test scanner))
           return (alist-get 'function scanner)))

(cl-defun magit-todos--scan-callback (&key callback magit-status-buffer results-regexp process &allow-other-keys)
  "Call CALLBACK with arguments MAGIT-STATUS-BUFFER and match items.
Match items are a list of `magit-todos-item' found in PROCESS's buffer for RESULTS-REGEXP."
  (funcall callback magit-status-buffer
           (with-current-buffer (process-buffer process)
             (magit-todos--buffer-items results-regexp))))

(defun magit-todos--buffer-items (results-regexp)
  "Return list of `magit-todos-item' found in current buffer for RESULTS-REGEXP."
  (let ((items))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (--when-let (condition-case err
                        (magit-todos--line-item results-regexp)
                      ;; Files with very, very long lines may cause Emacs's regexp matcher to overflow.
                      ;; Rather than abort the whole scan and raise an error, try to handle it gracefully.
                      ;; FIXME: This may raise multiple warnings per file.
                      (error (if (string= "Stack overflow in regexp matcher" (error-message-string err))
                                 (let ((filename (buffer-substring (point) (1- (re-search-forward ":")))))
                                   (display-warning 'magit-todos (concat "File has lines too long for Emacs to search.  Consider excluding it from scans: " filename)))
                               (signal (car err) (cdr err)))))
          (push it items))
        (forward-line 1)))
    (nreverse items)))

(cl-defun magit-todos--git-diff-callback (&key magit-status-buffer results-regexp search-regexp-elisp process heading
                                               exclude-globs &allow-other-keys)
  "Callback for git diff scanner output."
  ;; NOTE: Doesn't handle newlines in filenames or diff.mnemonicPrefix.
  (cl-macrolet ((next-diff () `(re-search-forward (rx bol "diff --git ") nil t))
                (next-filename () `(when (re-search-forward (rx bol "+++ b/" (group (1+ nonl))) nil t)
                                     (match-string 1)))
                (next-hunk-line-number () `(when (re-search-forward (rx bol "@@ -"
                                                                        (1+ digit) (optional "," (1+ digit)) (1+ space)
                                                                        "+" (group (1+ digit)) (optional "," (1+ digit)))
                                                                    file-end t)
                                             (string-to-number (match-string 1))))
                (file-end () `(or (save-excursion
                                    (when (re-search-forward (rx bol "diff --git ")
                                                             nil t)
                                      (match-beginning 0)))
                                  (point-max)))
                (hunk-end () `(or (save-excursion
                                    (when (re-search-forward (rx (or (seq bol "diff --git ")
                                                                     (seq bol "@@ ")))
                                                             nil t)
                                      (match-beginning 0)))
                                  (point-max))))
    (with-current-buffer (process-buffer process)
      (goto-char (point-min))
      (let ((glob-regexps (mapcar #'wildcard-to-regexp exclude-globs))
            items filename file-end hunk-end line-number)
        (while (next-diff)
          (while (setf filename (next-filename))
            (unless (--some? (string-match it filename) glob-regexps)
              (setf file-end (file-end))
              (while (setf line-number (next-hunk-line-number))
                (setf hunk-end (hunk-end))
                (while (re-search-forward (rx bol "+") hunk-end t)
                  ;; Since "git diff-index" doesn't accept PCREs to its "-G" option, we have to test the search regexp ourselves.
                  (when (re-search-forward search-regexp-elisp (line-end-position) t)
                    (when-let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                                (item (with-temp-buffer
                                        ;; NOTE: We fake grep output by inserting the filename, line number, position, etc.
                                        ;; This lets us use the same results regexp that's used for grep-like output.
                                        (save-excursion
                                          (insert filename ":" (number-to-string line-number) ":0: " line))
                                        (magit-todos--line-item results-regexp filename))))
                      (push item items)))
                  (cl-incf line-number))))))
        (let ((magit-todos-section-heading heading))
          (magit-todos--insert-items magit-status-buffer items :branch-p t))))))

(defun magit-todos--delete-section (condition)
  "Delete the section specified by CONDITION from the Magit status buffer.
See `magit-section-match'.  Also delete it from root section's children."
  (save-excursion
    (goto-char (point-min))
    (when-let ((section (cl-loop until (magit-section-match condition)
                                 ;; Use `forward-line' instead of `magit-section-forward' because
                                 ;; sometimes it skips our section.
                                 do (forward-line 1)
                                 when (eobp)
                                 return nil
                                 finally return (magit-current-section))))
      ;; Delete the section from root section's children.  This makes the section-jumper command
      ;; work when a replacement section is inserted after deleting this section.
      (object-remove-from-list magit-root-section 'children section)
      (with-slots (start end) section
        ;; NOTE: We delete 1 past the end because we insert a newline after the section.  I'm not
        ;; sure if this would generalize to all Magit sections.  But if the end is the same as
        ;; `point-max', which may be the case if todo items have not yet been inserted, we only
        ;; delete up to `point-max'.
        (delete-region start (if (= end (point-max))
                                 end
                               (1+ end)))))))

(defun magit-todos--item-buffer (item)
  "Return buffer visiting ITEM."
  (or (find-buffer-visiting (magit-todos-item-filename item))
      (find-file-noselect (magit-todos-item-filename item))))

(defun magit-todos--goto-item (item)
  "Move point to ITEM.
Assumes current buffer is ITEM's buffer."
  (pcase-let* (((cl-struct magit-todos-item position line column keyword) item))
    (if position
        (goto-char position)
      (goto-char (point-min))
      (forward-line (1- line))
      (if column
          (forward-char column)
        (when (re-search-forward (regexp-quote keyword) (line-end-position) t)
          (goto-char (match-beginning 0)))))))

(defun magit-todos--insert-todos ()
  "Insert to-do items into current buffer.
This function should be called from inside a ‘magit-status’ buffer."
  (declare (indent defun))
  (when magit-todos-active-scan
    ;; Avoid running multiple scans for a single magit-status buffer.
    (let ((buffer (process-buffer magit-todos-active-scan)))
      (when (process-live-p magit-todos-active-scan)
        (delete-process magit-todos-active-scan))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (setq magit-todos-active-scan nil))
  (pcase magit-todos-update
    ((or 't  ; Automatic
         ;; Manual and updating now
         (and 'nil (guard magit-todos-updating))
         ;; Caching and cache expired
         (and (pred integerp) (guard (or magit-todos-updating  ; Forced update
                                         (>= (float-time
                                              (time-subtract (current-time)
                                                             magit-todos-last-update-time))
                                             magit-todos-update)
                                         (null magit-todos-last-update-time)))))
     ;; Scan and insert.
     ;; HACK: I don't like setting a special var here, because it seems like lexically binding a
     ;; special var should follow down the chain, but it isn't working, so we'll do this.
     (setq magit-todos-updating t)
     (setq magit-todos-active-scan (funcall magit-todos-scanner
                                            :callback #'magit-todos--insert-items
                                            :magit-status-buffer (current-buffer)
                                            :directory default-directory
                                            :depth magit-todos-depth)))
    (_ ; Caching and cache not expired, or not automatic and not manually updating now
     (magit-todos--insert-items (current-buffer) magit-todos-item-cache)))
  (when (or (eq magit-todos-branch-list t)
            (and (eq magit-todos-branch-list 'branch)
                 (not (string= "master" (magit-get-current-branch)))))
    ;; Insert branch-local items.
    (magit-todos--scan-with-git-diff :magit-status-buffer (current-buffer)
                                     :directory default-directory
                                     :depth magit-todos-depth
                                     :heading (format "TODOs (branched from %s)" magit-todos-branch-list-merge-base-ref))))

(cl-defun magit-todos--insert-items (magit-status-buffer items &key branch-p)
  "Insert to-do ITEMS into MAGIT-STATUS-BUFFER.
If BRANCH-P is non-nil, do not update `magit-todos-item-cache',
`magit-todos-last-update-time', and `magit-todos-updating'."
  (declare (indent defun))
  ;; NOTE: This could be factored out into some kind of `magit-insert-section-async' macro if necessary.
  ;; MAYBE: Use `magit-insert-section-body'.
  (when (not (buffer-live-p magit-status-buffer))
    (message "`magit-todos--insert-items-callback': Callback called for deleted buffer"))
  (let* ((items (magit-todos--sort items))
         (num-items (length items))
         (group-fns (pcase magit-todos-auto-group-items
                      ('never nil)
                      ('always magit-todos-group-by)
                      ((pred integerp) (when (> num-items magit-todos-auto-group-items)
                                         magit-todos-group-by))
                      (_ (error "Invalid value for magit-todos-auto-group-items"))))
         (magit-todos-show-filenames (not (member 'magit-todos-item-filename group-fns)))
         (magit-section-show-child-count t)
         ;; HACK: "For internal use only."  But this makes collapsing the new section work!
         (magit-insert-section--parent magit-root-section)
         (inhibit-read-only t))
    (when (buffer-live-p magit-status-buffer)
      ;; Don't try to select a killed status buffer
      (with-current-buffer magit-status-buffer
        (unless branch-p
          ;; Don't do any of this for the branch-diff scanner.
          (when magit-todos-updating
            (when (or (null magit-todos-update)      ; Manual updates
                      (integerp magit-todos-update)) ; Caching
              (setq magit-todos-item-cache items)
              (setq magit-todos-last-update-time (current-time)))
            ;; HACK: I don't like setting this special var, but it works.  See other comment where
            ;; it's set t.
            (setq magit-todos-updating nil)))
        (save-excursion
          ;; Insert items
          (goto-char (point-min))
          ;; Go to insertion position
          (pcase magit-todos-insert-at
            ('top (cl-loop for ((this-section . _) . _) = (magit-section-ident (magit-current-section))
                           until (not (member this-section '(branch tags)))
                           do (magit-section-forward)))
            ('bottom (goto-char (oref (-last-item (oref magit-root-section children)) end)))
            (_ (magit-todos--skip-section (vector '* magit-todos-insert-at))))
          ;; Insert section
          (let ((reminder (if magit-todos-update
                              "" ; Automatic updates: no reminder
                            ;; Manual updates: remind user
                            " (update manually)")))
            (if (not items)
                (unless magit-todos-update
                  ;; Manual updates: Insert section to remind user
                  (let ((magit-insert-section--parent magit-root-section))
                    (magit-insert-section (todos)
                      (magit-insert-heading (concat (propertize magit-todos-section-heading 'face 'magit-section-heading)
                                                    " (0)" reminder "\n")))))
              (let ((section (magit-todos--insert-groups :type 'todos
                               :heading (format "%s (%s)%s"
                                                (propertize magit-todos-section-heading 'face 'magit-section-heading)
                                                num-items reminder)
                               :group-fns group-fns
                               :items items
                               :depth 0)))
                (insert "\n")
                (magit-todos--set-visibility :section section :num-items num-items)))))))))

(cl-defun magit-todos--insert-groups (&key depth group-fns heading type items)
  "Insert ITEMS into grouped Magit section and return the section.

DEPTH sets indentation and should be 0 for a top-level group.  It
is automatically incremented when this function calls itself.

GROUP-FNS may be a list of functions to which ITEMS are applied
with `-group-by' to group them.  Items are grouped
hierarchically, i.e. when GROUP-FNS has more than one function,
items are first grouped by the first function, then subgroups are
created which group items by subsequent functions.

HEADING is a string which is the group's heading.  The count of
items in each group is automatically appended.

TYPE is a symbol which is used by Magit internally to identify
sections."
  ;; NOTE: `magit-insert-section' seems to bind `magit-section-visibility-cache' to nil, so setting
  ;; visibility within calls to it probably won't work as intended.
  (declare (indent defun))
  (let* ((indent (propertize (s-repeat (* 2 depth) " ") 'face nil))
         (heading (concat indent heading))
         (magit-insert-section--parent (if (= 0 depth)
                                           magit-root-section
                                         magit-insert-section--parent)))
    (if (and (consp group-fns)
             (> (length group-fns) 0))
        ;; Insert more groups
        (let* ((groups (--> (-group-by (car group-fns) items)
                            (cl-loop for group in-ref it
                                     ;; HACK: Set ":" keys to nil so they'll be grouped together.
                                     do (pcase (car group)
                                          (":" (setf (car group) nil)))
                                     finally return it)
                            (magit-todos--coalesce-groups it)))
               (section (magit-insert-section ((eval type))
                          (magit-insert-heading heading)
                          (cl-loop for (group-type . items) in groups
                                   for group-name = (pcase group-type
                                                      ;; Use "[Other]" instead of empty group name.
                                                      ;; HACK: ":" is hard-coded, even though the
                                                      ;; suffix regexp could differ. If users change
                                                      ;; the suffix so this doesn't apply, it
                                                      ;; shouldn't cause any problems, it just won't
                                                      ;; look as pretty.
                                                      ((or "" ":" 'nil) "[Other]")
                                                      (_ (s-chop-suffix ":" group-type)))
                                   do (magit-todos--insert-groups
                                        :depth (1+ depth) :group-fns (cdr group-fns)
                                        :type (intern group-name) :items items
                                        :heading (concat
                                                  (if (and magit-todos-fontify-keyword-headers
                                                           (member group-name magit-todos-keywords-list))
                                                      (propertize group-name 'face (magit-todos--keyword-face group-name))
                                                    group-name)
                                                  ;; Item count
                                                  (if (= 1 (length group-fns))
                                                      ":" ; Let Magit add the count.
                                                    ;; Add count ourselves.
                                                    (concat " " (format "(%s)" (length items))))))))))
          (magit-todos--set-visibility :depth depth :num-items (length items) :section section)
          ;; Add top-level section to root section's children
          (when (= 0 depth)
            (push section (oref magit-root-section children)))
          ;; Don't forget to return the section!
          section)
      ;; Insert individual to-do items
      (magit-todos--insert-group :depth (1+ depth) :type type :items items :heading heading))))

(cl-defun magit-todos--insert-group (&key depth heading type items)
  "Insert ITEMS into Magit section and return the section.

DEPTH sets indentation and should be 0 for a top-level group.

HEADING is a string which is the group's heading.  The count of
items in each group is automatically appended.

TYPE is a symbol which is used by Magit internally to identify
sections."
  ;; NOTE: `magit-insert-section' seems to bind `magit-section-visibility-cache' to nil, so setting
  ;; visibility within calls to it probably won't work as intended.
  (declare (indent defun))
  (let* ((indent (propertize (s-repeat (* 2 depth) " ") 'face nil))
         (magit-insert-section--parent (if (= 0 depth)
                                           magit-root-section
                                         magit-insert-section--parent))
         (width (- (window-text-width) depth))
         (section (magit-insert-section ((eval type))
                    (magit-insert-heading heading)
                    (dolist (item items)
                      (let* ((filename (propertize (magit-todos-item-filename item) 'face 'magit-filename))
                             (string (--> (concat indent
                                                  (when magit-todos-show-filenames
                                                    (concat filename " "))
                                                  (funcall (if (s-suffix? ".org" filename)
                                                               #'magit-todos--format-org
                                                             #'magit-todos--format-plain)
                                                           item))
                                          (truncate-string-to-width it width))))
                        (magit-insert-section (todos-item item)
                          (insert string "\n")))))))
    (magit-todos--set-visibility :depth depth :num-items (length items) :section section)
    ;; Don't forget to return the section!
    section))

(cl-defun magit-todos--set-visibility (&key section num-items depth)
  "Set the visibility of SECTION.

If the section's visibility is cached by Magit, the cached
setting is applied.  Otherwise, visibility is set according to
whether NUM-ITEMS is greater than `magit-todos-max-items'.

When DEPTH is greater than 0, NUM-ITEMS is compared to
`magit-todos-max-items' divided by DEPTH multiplied by 2,
i.e. the max number of items which cause sections to be
automatically hidden halves at each deeper level."
  (declare (indent defun))
  (pcase (magit-section-cached-visibility section)
    ('hide (magit-section-hide section))
    ('show (magit-section-show section))
    (_ (if (> num-items (pcase depth
                          (0 magit-todos-max-items)
                          (_ (/ magit-todos-max-items (* depth 2)))))
           ;; HACK: We have to do this manually because the set-visibility-hook doesn't work.
           (magit-section-hide section)
         ;; Not hidden: show section manually (necessary for some reason)
         (magit-section-show section)))))

(defun magit-todos--skip-section (condition)
  "Move past the section matching CONDITION.
See `magit-section-match'."
  (goto-char (point-min))
  (cl-loop until (magit-section-match condition)
           do (magit-section-forward))
  (cl-loop until (not (magit-section-match condition))
           do (condition-case nil
                  ;; `magit-section-forward' raises an error when there are no more sections.
                  (magit-section-forward)
                (error (progn
                         (goto-char (1- (point-max)))
                         (cl-return))))))

(cl-defun magit-todos--line-item (regexp &optional filename)
  "Return item on current line, parsing current buffer with REGEXP.
FILENAME is added to the item as its filename.  Sets match data.
This should be called in a process's output buffer from one of
the async callback functions.  The calling function should
advance to the next line."
  (let ((case-fold-search magit-todos-ignore-case))
    (when (re-search-forward regexp (line-end-position) t)
      (make-magit-todos-item :filename (or filename
                                           (match-string 8))
                             :line (--when-let (match-string 2)
                                     (string-to-number it))
                             :column (--when-let (match-string 3)
                                       (string-to-number it))
                             :position (--when-let (match-string 9)
                                         (string-to-number it))
                             :org-level (match-string 1)
                             :keyword (match-string 4)
                             :suffix (match-string 6)
                             :description (match-string 5)))))

(defun magit-todos--keyword-face (keyword)
  "Return face for KEYWORD."
  ;; TODO(alphapapa): Instead of upcasing here, upcase in the lookup, so it can still be displayed
  ;; non-uppercase.  Preserving the distinction might be useful.
  (when magit-todos-ignore-case
    (setq keyword (upcase keyword)))
  (let ((face (assoc-default keyword hl-todo-keyword-faces #'string=)))
    (cl-typecase face
      (string (list :inherit 'hl-todo :foreground face))
      (t face))))

(defun magit-todos--fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer " *magit-todos--fontify-like-in-org-mode*")))
    (unless buffer
      (setq buffer (get-buffer-create " *magit-todos--fontify-like-in-org-mode*"))
      (with-current-buffer buffer
        (buffer-disable-undo)
        (org-mode)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert s)
      (let ((org-odd-levels-only odd-levels))
        (font-lock-ensure)
        (buffer-string)))))

(defun magit-todos-item-first-path-component (item)
  "Return ITEM's first directory.
This assumes that ITEM's filename is already set to a path
relative to the repo's directory (i.e. this would not be very
useful with absolute paths)."
  (car (f-split (magit-todos-item-filename item))))

(cl-defun magit-todos--async-start-process (name &key command finish-func)
  "Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

This is a copy of `async-start-process' that does not override
`process-connection-type'.  It also uses keyword arguments."
  (declare (indent defun))
  ;; TODO: Drop this function when possible.  See
  ;; <https://github.com/jwiegley/emacs-async/issues/102>.
  (let* ((args (cdr command))
         (command (car command))
         (buf (generate-new-buffer (concat " *" name "*")))
         (proc (apply #'start-file-process name buf command args)))
    (with-current-buffer buf
      (set-process-query-on-exit-flag proc nil)
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'magit-todos--async-when-done)
      (unless (string= name "emacs")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

(defun magit-todos--async-when-done (proc &optional _change)
  "Process sentinel used to retrieve the value from the child process.

This is a copy of `async-when-done' that does not raise an error
if the process's buffer has already been deleted."
  ;; I wish it weren't necessary to copy this function here, but at the moment it seems like the
  ;; only reasonable way to work around this problem of `async-when-done' trying to select deleted
  ;; buffers.  I already tried not deleting the buffers, but then I got bug reports about that and
  ;; had to revert it.  I don't know if I'm encountering a bug in my code, in `async', or in Emacs
  ;; itself, because it seems that `async-when-done' is being called on the wrong buffers: When I'm
  ;; connected with `matrix-client', and when I have a Magit status buffer opened with
  ;; `magit-todos-mode' active, then when one of the `url' process buffers from Matrix gets its
  ;; sentinel called, I get an error from `async-when-done' trying to select the already-deleted
  ;; `rg' scanner buffer!  It's as if Emacs is mixing up the process buffers.  I really don't know
  ;; what's going on.  But maybe I can work around it by copying this function and checking whether
  ;; the process's buffer is alive.
  ;; NOTE: TRAMP processes seem to have the status `signal' instead of
  ;; `exit'.  I can't find documentation as to why.
  (when (and (memq (process-status proc) '(exit signal))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((async-current-process proc))
        ;; TRAMP processes seem to have the exit status 9 instead of
        ;; 0.  I can't find documentation or code about it.
        (if (memq (process-exit-status proc) '(0 9))
            (if async-callback-for-process
                (if async-callback
                    (prog1
                        (funcall async-callback proc)
                      (unless async-debug
                        (kill-buffer (current-buffer))))
                  (set (make-local-variable 'async-callback-value) proc)
                  (set (make-local-variable 'async-callback-value-set) t))
              (goto-char (point-max))
              (backward-sexp)
              (async-handle-result async-callback (read (current-buffer))
                                   (current-buffer)))
          (set (make-local-variable 'async-callback-value)
               (list 'error
                     (format "Async process '%s' failed with exit code %d"
                             (process-name proc) (process-exit-status proc))))
          (set (make-local-variable 'async-callback-value-set) t))))))

;;;;; Formatters

(defun magit-todos--format-plain (item)
  "Return ITEM formatted as from a non-Org file."
  (format "%s%s %s"
          (propertize (magit-todos-item-keyword item)
                      'face (magit-todos--keyword-face (magit-todos-item-keyword item)))
          (or (magit-todos-item-suffix item) "")
          (or (magit-todos-item-description item) "")))

(defun magit-todos--format-org (item)
  "Return ITEM formatted as from an Org file."
  (require 'org)
  (magit-todos--fontify-like-in-org-mode
   (concat (magit-todos-item-org-level item) " "
           (magit-todos-item-keyword item) " "
           (magit-todos-item-description item))))

;;;;; Sorting

(defun magit-todos--sort (items)
  "Return ITEMS sorted according to `magit-todos-sort-order'."
  (dolist (fn (reverse magit-todos-sort-order) items)
    (setq items (sort items fn))))

(defun magit-todos--sort-by-keyword (a b)
  "Return non-nil if A's keyword is before B's in `magit-todos-keywords-list'."
  (cl-flet ((keyword-index (keyword)
                           (or (-elem-index keyword magit-todos-keywords-list) 0)))
    (< (keyword-index (magit-todos-item-keyword a))
       (keyword-index (magit-todos-item-keyword b)))))

(defun magit-todos--sort-by-position (a b)
  "Return non-nil if A's position in its file is before B's."
  (let ((a-position (or (magit-todos-item-position a)
                        (magit-todos-item-line a)))
        (b-position (or (magit-todos-item-position b)
                        (magit-todos-item-line b))))
    (< a-position b-position)))

(defun magit-todos--sort-by-filename (a b)
  "Return non-nil if A's filename is `string<' B's."
  (string< (magit-todos-item-filename a)
           (magit-todos-item-filename b)))

(defun magit-todos--sort-by-suffix (a b)
  "Return non-nil if A's suffix is `string<' B's."
  (string< (magit-todos-item-suffix a)
           (magit-todos-item-suffix b)))

;;;; Scanners

(cl-defmacro magit-todos-defscanner (name &key test command results-regexp
                                          (callback (function 'magit-todos--scan-callback)))
  "Define a `magit-todos' scanner named NAME.

NAME is a string, which may contain spaces.  It is only used for
descriptive purposes.

TEST is an unquoted sexp which is used to determine whether the
scanner is usable.  In most cases, it should use
`executable-find' to look for the scanner command.

COMMAND is a sexp which should evaluate to the scanner command,
i.e. a list of strings to be eventually passed to
`start-process'.  Nil elements are removed, numbers are converted
to strings, and nested lists are flattened into a single list.
It is evaluated each time the scanner is run.

Within the COMMAND list these variables are available:

`depth': When non-nil, an integer, which is the depth that should
be passed to the scanner's max-depth option (i.e. `magit-todos-depth').

`directory': The directory in which the scan should be run.

`extra-args': The value of the customization variable
\"magit-todos-NAME-extra-args\" (see below).

`keywords': List of item keywords defined in
`magit-todos-keywords-list'.

`search-regexp-pcre': PCRE-compatible regular expression to be passed
to the scanner process.

`search-regexp-elisp': Emacs regular expression, which may be
used for scanners written as Emacs Lisp functions.

RESULTS-REGEXP is an optional string or unquoted sexp which is
used to match results in the scanner process's output buffer.
Typically this will be a sexp which calls `rx-to-string'.  It is
evaluated each time the scanner is run.  If nil, the appropriate
default is used which matches results in the form:

  FILENAME:LINE:MATCH

Where MATCH may also match Org outline heading stars when
appropriate.  Custom regexps may also match column numbers or
byte offsets in the appropriate numbered groups; see
`make-magit-todos-item'.

CALLBACK is called to process the process's output buffer.
Normally the default should be used, which inserts items into the
Magit status buffer which is passed as an argument to the scanner
function.

The macro defines the following:

\"magit-todos-NAME-extra-args\": A customization setting, a list
of strings to be passed to the scanner as extra arguments.

\"magit-todos--scan-with-NAME\": The function which runs the
scanner command.

It also adds the scanner to the customization variable
`magit-todos-scanner', and to the variable
`magit-todos-scanners' (which is used to set
`magit-todos-scanner' by calling `magit-todos--choose-scanner')."
  ;; TODO: Try to obviate the -scanners variable, let --choose-scanner use the
  ;; custom-type of -scanner directly.  Maybe, anyway--I don't want to ugly up the UI
  ;; for users.
  (declare (indent defun) (debug (stringp [&rest &or [":test" def-form]
                                                 [":command" def-form]
                                                 [":results-regexp" [&or stringp def-form]]])))
  (let* ((name-without-spaces (s-replace " " "-" name))
         (scan-fn-name (concat "magit-todos--scan-with-" name-without-spaces))
         (scan-fn-symbol (intern scan-fn-name))
         (extra-args-var (intern (format "magit-todos-%s-extra-args" name-without-spaces))))
    `(progn
       (defcustom ,extra-args-var nil
         ,(format "Extra arguments passed to %s." name)
         :type '(repeat string))

       ;; NOTE: Both the macro and the macro-defined function have `callback' arguments.  Pay attention to unquoting.
       ;; FIXME: That is confusing.

       (cl-defun ,scan-fn-symbol (&key magit-status-buffer directory depth heading sync callback)
         ,(format "Scan for to-dos with %s, then call `%s'.
MAGIT-STATUS-BUFFER is what it says.  DIRECTORY is the directory in which to run the scan.  DEPTH should be an integer, typically the value of `magit-todos-depth'.  HEADING is passed to `%s'.

When SYNC is nil, the scanner process is returned, and CALLBACK
is a function which is called by the process sentinel with one
argument, a list of match items.

When SYNC is non-nil, match items are returned."
                  name callback callback)
         (let* ((process-connection-type 'pipe)
                (directory (f-relative directory default-directory))
                (extra-args (when ,extra-args-var
                              (--map (s-split (rx (1+ space)) it 'omit-nulls)
                                     ,extra-args-var)))
                (keywords magit-todos-keywords-list)
                (search-regexp-elisp (rx-to-string
                                      `(or
                                        ;; Org item
                                        (seq bol (group (1+ "*"))
                                             (1+ blank)
                                             (group (or ,@keywords))
                                             (1+ space)
                                             (group (1+ not-newline)))
                                        ;; Non-Org
                                        (seq (or bol (1+ blank))
                                             (group (or ,@keywords))
                                             (regexp ,magit-todos-keyword-suffix)
                                             (optional (1+ blank)
                                                       (group (1+ not-newline)))))))
                (search-regexp-pcre (rxt-elisp-to-pcre search-regexp-elisp))
                (results-regexp (or ,results-regexp
                                    (rx-to-string
                                     `(seq bol
                                           ;; Filename
                                           (group-n 8 (1+ (not (any ":")))) ":"
                                           ;; Line
                                           (group-n 2 (1+ digit)) ":"
                                           (or
                                            ;; Org item
                                            (seq (group-n 1 (1+ "*"))
                                                 (1+ blank)
                                                 (group-n 4 (or ,@keywords))
                                                 (1+ blank)
                                                 (group-n 5 (1+ not-newline)))
                                            ;; Non-Org
                                            (seq (optional (1+ not-newline))
                                                 (group-n 4 (or ,@keywords))
                                                 (optional (group-n 6 (regexp ,magit-todos-keyword-suffix)))
                                                 (optional (1+ blank))
                                                 (optional (group-n 5 (1+ not-newline)))))))))
                (command (-flatten
                          (-non-nil
                           (list (when magit-todos-nice
                                   (list "nice" "-n5"))
                                 ,command)))))
           ;; Convert any numbers in command to strings (e.g. depth).
           (cl-loop for elt in-ref command
                    when (numberp elt)
                    do (setf elt (number-to-string elt)))
           ;; Run command.
           (if sync
               ;; Synchronous: return matching items.
               (with-temp-buffer
                 (unless (= 0 (apply #'call-process (car command) nil (current-buffer) nil
                                     (cdr command)))
                   (user-error (concat (car command) " failed")))
                 (magit-todos--buffer-items results-regexp))
             ;; Async: return process.
             (magit-todos--async-start-process ,scan-fn-name
               :command command
               ;; NOTE: This callback chain.
               :finish-func (apply-partially ,callback
                                             :callback callback
                                             :magit-status-buffer magit-status-buffer
                                             :results-regexp results-regexp
                                             :search-regexp-elisp search-regexp-elisp
                                             :heading heading
                                             :exclude-globs magit-todos-exclude-globs
                                             :process))))) ; Process is appended to the list.
       (magit-todos--add-to-custom-type 'magit-todos-scanner
         (list 'const :tag ,name #',scan-fn-symbol))
       (add-to-list 'magit-todos-scanners
                    (list (cons 'name ,name)
                          (cons 'function #',scan-fn-symbol)
                          (cons 'test ',test))
                    'append))))

;; NOTE: These scanners handle the max-depth option differently.  git-grep seems to handle it in the
;; most useful way, with a setting of 0 meaning to look no deeper than the current directory, and a
;; setting of 1 searching directories one level deeper.  In comparison, rg and find effectively
;; subtract one from the value, as a setting of 0 returns no results, and a setting of 1 searches
;; only the current directory (which means that when the max-depth is set to 0, the whole command is
;; essentially a no-op, which is pointless).  Since we want `magit-todos-depth' to behave
;; consistently with all scanners, we will treat it the way git-grep does, and for the other
;; scanners we'll add one to its value.

(magit-todos-defscanner "rg"
  :test (executable-find "rg")
  :command (list "rg" "--no-heading" "--line-number"
                 (when depth
                   (list "--maxdepth" (1+ depth)))
                 (when magit-todos-ignore-case
                   "--ignore-case")
                 (when magit-todos-exclude-globs
                   (--map (list "--glob" (concat "!" it))
                          magit-todos-exclude-globs))
                 (unless magit-todos-submodule-list
                   (--map (list "--glob" (concat "!" it))
                          (magit-list-module-paths)))
                 extra-args search-regexp-pcre directory))

(magit-todos-defscanner "git grep"
  :test (string-match "--perl-regexp" (shell-command-to-string "git grep --magit-todos-testing-git-grep"))
  :command (list "git" "--no-pager" "grep"
                 "--full-name" "--no-color" "-n"
                 (when depth
                   (list "--max-depth" depth))
                 (when magit-todos-ignore-case
                   "--ignore-case")
                 "--perl-regexp"
                 "-e" search-regexp-pcre
                 extra-args "--" directory
                 (when magit-todos-exclude-globs
                   (--map (concat ":!" it)
                          magit-todos-exclude-globs))
                 (unless magit-todos-submodule-list
                   (--map (list "--glob" (concat "!" it))
                          (magit-list-module-paths)))))

(magit-todos-defscanner "git diff"
  ;; NOTE: This scanner implements the regexp *searching* in elisp rather than in the
  ;; external process because, unlike "git grep", "git diff" does not support PCRE.
  :test t
  :command (progn
             ;; Silence byte-compiler warnings about these vars we don't use in this scanner.
             (ignore search-regexp-elisp search-regexp-pcre extra-args directory depth)
             (list "git" "--no-pager" "diff" "--no-color" "-U0"
                   (-> "git merge-base HEAD "
                       (concat magit-todos-branch-list-merge-base-ref)
                       shell-command-to-string
                       string-trim)))
  :callback 'magit-todos--git-diff-callback)

(magit-todos-defscanner "find|grep"
  ;; NOTE: The filenames output by find|grep have a leading "./".  I don't expect this scanner to be
  ;; used much, if at all, so I'm not going to go to the trouble to fix this now.
  :test (string-match "--perl-regexp" (shell-command-to-string "grep --help"))
  :command (let* ((grep-find-template (progn
                                        (unless grep-find-template
                                          (grep-compute-defaults))
                                        (->> grep-find-template
                                             (s-replace " grep " " grep -b -E ")
                                             (s-replace " -nH " " -H "))))
                  (_ (when depth
                       (setq grep-find-template
                             (s-replace " <D> " (concat " <D> -maxdepth " (1+ depth) " ")
                                        grep-find-template)))))
             ;; Modified from `rgrep-default-command'
             (list "find" directory
                   (list (when grep-find-ignored-directories
                           (list "-type" "d"
                                 "(" "-path"
                                 (-interpose (list "-o" "-path")
                                             (-non-nil (--map (cond ((stringp it)
                                                                     (concat "*/" it))
                                                                    ((consp it)
                                                                     (and (funcall (car it) it)
                                                                          (concat "*/" (cdr it)))))
                                                              grep-find-ignored-directories)))
                                 ")" "-prune"))
                         (when grep-find-ignored-files
                           (list "-o" "-type" "f"
                                 "(" "-name"
                                 (-interpose (list "-o" "-name")
                                             (--map (cond ((stringp it) it)
                                                          ((consp it) (and (funcall (car it) it)
                                                                           (cdr it))))
                                                    grep-find-ignored-files))
                                 ")" "-prune"))
                         (when magit-todos-exclude-globs
                           (list "-o" "("
                                 (--map (list "-iname" it)
                                        magit-todos-exclude-globs)
                                 ")" "-prune"))
                         (unless magit-todos-submodule-list
                           (list "-o" "("
                                 (--map (list "-iname" it)
                                        (magit-list-module-paths))
                                 ")" "-prune")))
                   (list "-o" "-type" "f")
                   ;; NOTE: This uses "grep -P", i.e. "Interpret the pattern as a
                   ;; Perl-compatible regular expression (PCRE).  This is highly
                   ;; experimental and grep -P may warn of unimplemented features."  But it
                   ;; does seem to work properly, at least on GNU grep.  Using "grep -E"
                   ;; with this PCRE regexp doesn't work quite right, as it doesn't match
                   ;; all the keywords, but pcre2el doesn't convert to "extended regular
                   ;; expressions", so this will have to do.  Maybe we should test whether
                   ;; the version of grep installed supports "-P".
                   ;; NOTE: For some reason, when "-execdir" is used, the callback function is never
                   ;; called, even though the process terminates and its buffer has the correct
                   ;; output.  It works correctly when "-exec" is used, so we use that.
                   (list "-exec" "grep" "-bPH"
                         (when magit-todos-ignore-case
                           "--ignore-case")
                         extra-args
                         search-regexp-pcre "{}" "+"))))

;;;;; Set scanner default value

;; Now that all the scanners have been defined, we can set the value.
(custom-reevaluate-setting 'magit-todos-scanner)

;;;; Helm/Ivy

;; These add optional support for Helm and Ivy.  This code does not require
;; Helm or Ivy to be installed; it is only called after one of them is loaded.

(declare-function helm-make-source "ext:helm-source")
(declare-function helm "ext:helm")

(with-eval-after-load 'helm
  (defvar helm-magit-todos-source
    ;; We use `helm-make-source' instead of `helm-make-sync-source', which is a
    ;; macro, which won't be present if the user doesn't have Helm installed.
    (helm-make-source "helm-magit-todos" 'helm-source-sync
      :candidates #'magit-todos-candidates
      :action (lambda (item)
                (magit-todos-jump-to-item :item item))))

  (defun helm-magit-todos ()
    "Display `magit-todos' items with Helm.
Note that this uses `magit-todos-items-cache' when a Magit status
buffer is available for the repository directory, in which case
the cache is not updated from this command."
    (interactive)
    (helm :sources '(helm-magit-todos-source))))

(declare-function ivy-read "ext:ivy")

(with-eval-after-load 'ivy
  (defvar ivy-magit-todos-history nil
    "History for `ivy-magit-todos'.")

  (defun ivy-magit-todos ()
    "Display `magit-todos' items with Ivy.
Note that this uses `magit-todos-items-cache' when a Magit status
buffer is available for the repository directory, in which case
the cache is not updated from this command."
    (interactive)
    (ivy-read "TODOs: " (magit-todos-candidates)
              :action (lambda (item)
                        (magit-todos-jump-to-item :item (cdr item)))
              :history 'ivy-magit-todos-history
              :caller 'ivy-magit-todos)))

;;;;; Support

;; These functions are used by both Helm and Ivy.

(defsubst magit-todos-item-cons (item)
  "Return ITEM as a (DISPLAY . ITEM) pair.
Used for e.g. Helm and Ivy."
  (cons (concat (magit-todos-item-filename item) " "
                (funcall (if (s-suffix? ".org" (magit-todos-item-filename item))
                             #'magit-todos--format-org
                           #'magit-todos--format-plain)
                         item))
        item))

(defun magit-todos-candidates ()
  "Return list of (DISPLAY . ITEM) candidates for e.g. Helm and Ivy."
  ;; MAYBE: Update the cache appropriately from here.
  (if-let* ((magit-status-buffer (magit-get-mode-buffer 'magit-status-mode))
            (items (buffer-local-value 'magit-todos-item-cache magit-status-buffer)))
      ;; Use cached items.
      (cl-loop for item in items
               collect (magit-todos-item-cons item))
    ;; No cached items: run scan.
    (when-let* ((items (funcall magit-todos-scanner :sync t
                                :directory default-directory
                                :depth magit-todos-depth)))
      (cl-loop for item in items
               collect (magit-todos-item-cons item)))))

;;;; Footer

(provide 'magit-todos)

;;; magit-todos.el ends here
