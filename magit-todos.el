;;; magit-todos.el --- Show source file TODOs in Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/magit-todos
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (a "0.1.0") (anaphora "1.0.0") (async "1.9.2") (dash "2.13.0") (f "0.17.2") (hl-todo) (magit) (pcre2el "1.8") (s "1.12.0"))
;; Keywords: magit, vc

;;; Commentary:

;; This package displays keyword entries from source code comments and Org files
;; in the Magit status buffer.  Activating an item jumps to it in its file.  By
;; default, it uses keywords from `hl-todo', minus a few (like "NOTE").

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; a
;; anaphora
;; async
;; dash
;; f
;; hl-todo
;; magit
;; pcre2el
;; s

;; Then put this file in your load-path, and put this in your init file:

;;   (require 'magit-todos)

;;;; Usage

;; Run `magit-todos-mode', then open a Magit status buffer.

;;;; Tips

;; + You can customize settings in the `magit-todos' group.

;;;; Credits

;; This package was inspired by <https://github.com/danielma/magit-org-todos.el>.

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
(require 'org)
(require 'grep)
(require 'seq)

(require 'a)
(require 'anaphora)
(require 'async)
(require 'dash)
(require 'f)
(require 'hl-todo)
(require 'magit)
(require 'pcre2el)
(require 's)

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

(defvar magit-todos-ignored-directories nil
  "Automatically set by `magit-todos--repo-todos'.")

(defvar-local magit-todos-active-scan nil
  "The current scan's process.
Used to avoid running multiple simultaneous scans for a
magit-status buffer.")

(defvar magit-todos-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "jT" #'magit-todos-jump-to-todos)
    map)
  "Keymap for `magit-todos' top-level section.")

(defvar magit-todos-item-section-map
  (let ((map (make-sparse-keymap)))
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

;;;; Customization

(defgroup magit-todos nil
  "Show TODO items in source code comments in repos' files."
  :group 'magit)

(defcustom magit-todos-update t
  "When or how often to scan for to-dos.
When set to manual updates, the list can be updated with the
`magit-todos-update' command.  When caching is enabled, scan for
items whenever the Magit status buffer is refreshed and at least
N seconds have passed since the last scan; otherwise, use cached
items."
  :type '(choice (const :tag "Automatically, when the Magit status buffer is refreshed" t)
                 (integer :tag "Automatically, but cache items for N seconds")
                 (const :tag "Manually" nil)))

(defcustom magit-todos-fontify-keyword-headers t
  "Apply keyword faces to group keyword headers."
  :type 'boolean)

(defcustom magit-todos-require-colon t
  "Only show items whose keywords are followed by a colon.
i.e. when non-nil, only items like \"TODO: foo\" are shown, not
\"TODO foo\"."
  :type 'boolean
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
                           (symbol (if (a-associative-p (symbol-value value))
                                       (mapcar #'car (symbol-value value))
                                     (symbol-value value)))
                           (list value))))
           (setq keywords (seq-difference keywords magit-todos-ignored-keywords)
                 magit-todos-keywords-list keywords
                 ;; NOTE: The pcre2el library completely saves us here.  It is fantastic.
                 magit-todos-search-regexp (rxt-elisp-to-pcre (rx-to-string `(or
                                                                              ;; Org item
                                                                              (seq bol (group (1+ "*"))
                                                                                   (1+ blank)
                                                                                   (group (or ,@keywords))
                                                                                   (1+ space)
                                                                                   (group (1+ not-newline)))
                                                                              ;; Non-Org
                                                                              (seq (group (or bol (1+ blank)))
                                                                                   (group (or ,@keywords))
                                                                                   (eval (if magit-todos-require-colon
                                                                                             ":"
                                                                                           `(or eol blank (not (any alnum)))))
                                                                                   (optional (1+ blank)
                                                                                             (group (1+ not-newline)))))))
                 magit-todos-grep-result-regexp (rx-to-string `(seq bol
                                                                    ;; Filename
                                                                    (group-n 8 (1+ (not (any ":")))) ":"
                                                                    ;; Position
                                                                    (group-n 9 (1+ digit)) ":"
                                                                    ;; Org level
                                                                    (optional (group-n 1 (1+ "*")))
                                                                    (minimal-match (0+ not-newline))
                                                                    ;; Keyword
                                                                    (group-n 4 (or ,@keywords)) (optional ":")
                                                                    (optional (1+ blank)
                                                                              ;; Description
                                                                              (group-n 5 (1+ not-newline)))))
                 magit-todos-ag-result-regexp (rx-to-string `(seq bol
                                                                  ;; Line
                                                                  (group-n 2 (1+ digit)) ";"
                                                                  ;; Column
                                                                  (group-n 3 (1+ digit)) " "
                                                                  (1+ digit) ":"
                                                                  ;; Org level
                                                                  (optional (group-n 1 (1+ "*")))
                                                                  (minimal-match (0+ not-newline))
                                                                  ;; Keyword
                                                                  (group-n 4 (or ,@keywords)) (optional ":")
                                                                  (optional (1+ blank)
                                                                            ;; Description
                                                                            (group-n 5 (1+ not-newline)))))
                 magit-todos-rg-result-regexp (rx-to-string `(seq bol
                                                                  ;; Line
                                                                  (group-n 2 (1+ digit)) ":"
                                                                  ;; Column
                                                                  (group-n 3 (1+ digit)) ":"
                                                                  ;; Org level
                                                                  (optional (group-n 1 (1+ "*")))
                                                                  (minimal-match (0+ not-newline))
                                                                  ;; Keyword
                                                                  (group-n 4 (or ,@keywords)) (optional ":")
                                                                  (optional (1+ blank)
                                                                            ;; Description
                                                                            (group-n 5 (1+ not-newline)))))

                 magit-todos-git-grep-result-regexp (rx-to-string
                                                     `(seq bol
                                                           ;; Filename
                                                           (group-n 8 (1+ (not (any ":")))) ":"
                                                           ;; Line
                                                           (group-n 2 (1+ digit)) ":"
                                                           ;; Org level
                                                           (optional (group-n 1 (1+ "*")))
                                                           (minimal-match (0+ not-newline))
                                                           ;; Keyword
                                                           (group-n 4 (or ,@keywords)) (optional ":")
                                                           (optional (1+ blank)
                                                                     ;; Description
                                                                     (group-n 5 (1+ not-newline)))))))))

(defcustom magit-todos-scan-fn nil
  "File scanning method.
\"Automatic\" will attempt to use rg, ag, git-grep, and
find-grep, in that order. "
  :type '(choice (const :tag "Automatic" nil)
                 (const :tag "rg" magit-todos--rg-scan-async)
                 (const :tag "ag" magit-todos--ag-scan-async)
                 (const :tag "git-grep" magit-todos--git-grep-scan-async)
                 (const :tag "find-grep" magit-todos--grep-scan-async)
                 (function :tag "Custom function"))
  :set (lambda (option value)
         (unless value
           ;; Choosing automatically
           (setq value (cond ((executable-find "rg")
                              #'magit-todos--rg-scan-async)
                             ((executable-find "ag")
                              #'magit-todos--ag-scan-async)
                             ((not (string-match "Perl-compatible"
                                                 (shell-command-to-string "git grep --max-depth 0 --perl-regexp --no-index --q magit-todos-test-string")))
                              ;; If Git does not complain about Perl-compatible regexps, it should have been built with libpcre support.
                              #'magit-todos--git-grep-scan-async)
                             ((string-match (rx "-P, --perl-regexp") (shell-command-to-string "grep --help"))
                              #'magit-todos--grep-scan-async)
                             (t (error "magit-todos: Unable to find rg, ag, or a grep command that supports the --perl-regexp option")))))
         (set-default option value)))

(defcustom magit-todos-max-items 10
  "Automatically collapse the section if there are more than this many items."
  :type 'integer)

(defcustom magit-todos-auto-group-items 20
  "Whether or when to automatically group items."
  :type '(choice (integer :tag "When there are more than this many items")
                 (const :tag "Always" always)
                 (const :tag "Never" never)))

(defcustom magit-todos-group-by '(magit-todos-item-keyword magit-todos-item-filename)
  "How to group items.
One or more attributes may be chosen, and they will be grouped in
order."
  :type '(repeat (choice (const :tag "By filename" magit-todos-item-filename)
                         (const :tag "By keyword" magit-todos-item-keyword)
                         (const :tag "By first path component" magit-todos-item-first-path-component))))

(defcustom magit-todos-ignore-file-suffixes '(".org_archive" "#")
  "Ignore files with these suffixes."
  :type '(repeat string))

(defcustom magit-todos-ignore-case nil
  "Upcase keywords found in files.
If nil, a keyword like \"Todo:\" will not be shown.  `upcase' can
be a relatively expensive function, so this can be disabled if
necessary."
  :type 'boolean)

(defcustom magit-todos-fontify-org t
  "Fontify items from Org files as Org headings."
  :type 'boolean)

(defcustom magit-todos-sort-order '(magit-todos--sort-by-keyword
                                    magit-todos--sort-by-filename
                                    magit-todos--sort-by-position)
  "Order in which to sort items."
  :type '(repeat (choice (const :tag "Keyword" magit-todos--sort-by-keyword)
                         (const :tag "Filename" magit-todos--sort-by-filename)
                         (const :tag "Buffer position" magit-todos--sort-by-position)
                         (function :tag "Custom function"))))

(defcustom magit-todos-depth 0
  "Maximum depth of files in repo working tree to scan for to-dos.
Deeper scans can be slow in large projects.  You may wish to set
this in a directory-local variable for certain projects."
  :type '(choice (const :tag "Repo root directory only" 0)
                 integer))

(defcustom magit-todos-nice t
  "Run scanner with \"nice\"."
  :type 'boolean)

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

(defcustom magit-todos-ag-args nil
  "Extra arguments to pass to ag."
  :type '(repeat string))

(defcustom magit-todos-rg-args nil
  "Extra arguments to pass to rg."
  :type '(repeat string))

(defcustom magit-todos-git-grep-args nil
  "Extra arguments to pass to git-grep."
  :type '(repeat string))

;;;; Structs

(cl-defstruct magit-todos-item
  filename org-level line column position keyword description)

;;;; Commands

;;;###autoload
(define-minor-mode magit-todos-mode
  "Show list of to-do items in Magit status buffer for tracked files in repo."
  :require 'magit-todos
  :group 'magit-todos
  :global t
  (if magit-todos-mode
      (progn
        (if (lookup-key magit-status-mode-map "jT")
            (message "magit-todos: Not overriding bind of \"jT\" in `magit-status-mode-map'.")
          (define-key magit-status-mode-map "jT" #'magit-todos-jump-to-todos))
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-todos--insert-items
                                'magit-insert-staged-changes
                                'append))
    ;; Disable mode
    (when (equal (lookup-key magit-status-mode-map "jT") #'magit-jump-to-todos)
      (define-key magit-status-mode-map "jT" nil))
    (remove-hook 'magit-status-sections-hook #'magit-todos--insert-items)))

;;;###autoload
(defun magit-todos-update ()
  "Update the to-do list manually.
Only necessary when `magit-todos-update' is nil."
  (interactive)
  (let ((inhibit-read-only t))
    (magit-todos--delete-section [* todos])
    ;; HACK: See other note on `magit-todos-updating'.
    (setq magit-todos-updating t)
    (magit-todos--insert-items)))

(defun magit-todos-jump-to-item (&optional peek)
  "Show current item.
If PEEK is non-nil, keep focus in status buffer window."
  (interactive)
  (let* ((status-window (selected-window))
         (item (oref (magit-current-section) value))
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
  (magit-todos-jump-to-item 'peek))

;;;;; Jump to section

(magit-define-section-jumper magit-jump-to-todos "TODOs" todos)

(defun magit-todos-jump-to-todos ()
  "Jump to TODOs section, and update it if empty."
  (interactive)
  (let ((already-in-section (magit-section-match [* todos])))
    (magit-jump-to-todos)
    (when (or
           ;; Cached and forcing update
           (and already-in-section
                (integerp magit-todos-update))
           ;; Manual updates
           (not magit-todos-update)
           ;; Section is empty
           (= 0 (length (oref (magit-current-section) children))))
      (magit-todos-update))))

;;;; Functions

(defun magit-todos--delete-section (condition)
  "Delete the section specified by CONDITION from the Magit status buffer.
See `magit-section-match'.  Also delete it from root section's children."
  (save-excursion
    (goto-char (point-min))
    (when-let ((section (cl-loop until (magit-section-match condition)
                                 ;; Use `forward-line' instead of `magit-section-forward' because
                                 ;; sometimes it skips our section.
                                 do (forward-line 1)
                                 finally return (magit-current-section))))
      ;; Delete the section from root section's children.  This makes the section-jumper command
      ;; work when a replacement section is inserted after deleting this section.
      (object-remove-from-list magit-root-section 'children section)
      (with-slots (start end) section
        ;; NOTE: We delete 1 past the end because we insert a newline after the section.  I'm not
        ;; sure if this would generalize to all Magit sections.
        (delete-region start (1+ end))))))

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
        (re-search-forward keyword (line-end-position) t)
        (goto-char (match-beginning 0))))))

(defun magit-todos--insert-items ()
  "Insert to-do items into current buffer.
This function should be called from inside a ‘magit-status’ buffer."
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
     (setq magit-todos-active-scan (funcall magit-todos-scan-fn
                                            :magit-status-buffer (current-buffer)
                                            :directory default-directory
                                            :depth magit-todos-depth)))
    (_  ; Caching and cache not expired, or not automatic and not manually updating now
     (magit-todos--insert-items-callback (current-buffer) magit-todos-item-cache))))

(defun magit-todos--insert-items-callback (magit-status-buffer items)
  "Insert to-do ITEMS into MAGIT-STATUS-BUFFER."
  (declare (indent defun))
  ;; NOTE: This could be factored out into some kind of `magit-insert-section-async' macro if necessary.
  (when (not (buffer-live-p magit-status-buffer))
    (error "`magit-todos--insert-items-callback': Callback called for deleted buffer"))
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
        (when magit-todos-updating
          (when (or (null magit-todos-update) ; Manual updates
                    (integerp magit-todos-update)) ; Caching
            (setq magit-todos-item-cache items)
            (setq magit-todos-last-update-time (current-time)))
          ;; HACK: I don't like setting this special var, but it works.  See other comment where
          ;; it's set t.
          (setq magit-todos-updating nil))
        (save-excursion
          ;; Insert items
          (goto-char (point-min))
          ;; Go to insertion position
          (pcase magit-todos-insert-at
            ('top (cl-loop for ((this-section . _) . _) = (magit-section-ident (magit-current-section))
                           until (not (member this-section '(branch tags)))
                           do (magit-section-forward)))
            ('bottom (goto-char (point-max)))
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
                      (magit-insert-heading (concat "TODOs (0)" reminder)))
                    (insert "\n")))
              (aprog1
                  (magit-todos--insert-group :type 'todos
                    :heading (format "TODOs (%s)%s" num-items reminder)
                    :group-fns group-fns
                    :items items
                    :depth 0)
                (insert "\n")
                (magit-todos--set-visibility :section it :num-items num-items)))))))))

(cl-defun magit-todos--insert-group (&key depth group-fns heading type items)
  "Insert ITEMS into grouped Magit section and return the section.

DEPTH sets indentation and should be 0 for a top-level group.  It
is automatically incremented by 2 when this function calls
itself.

GROUP-FNS may be a list of functions to which ITEMS are applied
with `-group-by' to group them.  Items are grouped
hierarchically, i.e. when GROUP-FNS has more than one function,
items are first grouped by the first function, then subgroups are
created which group items by subsequent functions.

HEADING is a string which is the group's heading.  The count of
items in each group is automatically appended.

TYPE is a symbol which is used by Magit internally to identify
sections."
  ;; FIXME: Visibility caching doesn't work :( It seems that the `magit-section-visibility-cache'
  ;; variable gets filled with extra entries with incorrect visibility states, and then `alist-get'
  ;; gets the wrong value.  Need to see if that happens when magit-todos-mode is off.

  ;; NOTE: `magit-insert-section' seems to bind `magit-section-visibility-cache' to nil, so setting
  ;; visibility within calls to it probably won't work as intended.
  (declare (indent defun))
  (let* ((indent (s-repeat depth " "))
         (heading (concat indent heading))
         (magit-insert-section--parent (if (= 0 depth)
                                           magit-root-section
                                         magit-insert-section--parent)))
    (if (and (consp group-fns)
             (> (length group-fns) 0))
        ;; Insert more sections
        (aprog1                         ; `aprog1' is really handy here.
            (magit-insert-section ((eval type))
              (magit-insert-heading heading)
              (cl-loop for (group-type . items) in (-group-by (car group-fns) items)
                       do (magit-todos--insert-group :type (make-symbol group-type)
                            :heading (concat
                                      (if (and magit-todos-fontify-keyword-headers
                                               (member group-type magit-todos-keywords-list))
                                          (propertize group-type 'face (magit-todos--keyword-face group-type))
                                        group-type)
                                      ;; Item count
                                      (if (= 1 (length group-fns))
                                          ":" ; Let Magit add the count.
                                        ;; Add count ourselves.
                                        (concat " " (format "(%s)" (length items)))))
                            :group-fns (cdr group-fns)
                            :depth (+ 2 depth)
                            :items items)))
          (magit-todos--set-visibility :depth depth :num-items (length items) :section it)
          ;; Add top-level section to root section's children
          (when (= 0 depth)
            (push it (oref magit-root-section children))))
      ;; Insert individual to-do items
      (let ((width (window-text-width)))
        (aprog1
            (magit-insert-section ((eval type))
              (magit-insert-heading heading)
              (dolist (item items)
                (let* ((filename (propertize (magit-todos-item-filename item) 'face 'magit-filename))
                       (string (--> (concat indent
                                            (when (> depth 0)
                                              ;; NOTE: We indent the item for both the group level and the item level.
                                              "  ")
                                            (when magit-todos-show-filenames
                                              (concat filename " "))
                                            (funcall (if (s-suffix? ".org" filename)
                                                         #'magit-todos--format-org
                                                       #'magit-todos--format-plain)
                                                     item))
                                    (truncate-string-to-width it (- width depth)))))
                  (magit-insert-section (todos-item item)
                    (insert string))
                  (insert "\n"))))
          (magit-todos--set-visibility :depth depth :num-items (length items) :section it))))))

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

(cl-defun magit-todos--next-item (regexp &optional filename)
  "Return item on current line, parsing current buffer with REGEXP.
FILENAME is added to the item as its filename.  Sets match data.
This should be called in a process's output buffer from one of
the async callback functions.  The calling function should
advance to the next line."
  (let ((case-fold-search magit-todos-ignore-case))
    (when (re-search-forward regexp (line-end-position) t)
      (make-magit-todos-item :filename (or filename
                                           (match-string 8))
                             :org-level (match-string 1)
                             :line (awhen (match-string 2)
                                     (string-to-number it))
                             :column (awhen (match-string 3)
                                       (string-to-number it))
                             :position (awhen (match-string 9)
                                         (string-to-number it))
                             :keyword (match-string 4)
                             :description (match-string 5)))))

(defun magit-todos--keyword-face (keyword)
  "Return face for KEYWORD."
  ;; TODO: Instead of upcasing here, upcase in the lookup, so it can still be displayed
  ;; non-uppercase.  Preserving the distinction might be useful.
  (when magit-todos-ignore-case
    (setq keyword (upcase keyword)))
  (atypecase (a-get hl-todo-keyword-faces keyword)
    (string (list :inherit 'hl-todo :foreground it))
    (t it)))

(defun magit-todos--fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer " *magit-todos-fontify*")))
    (unless buffer
      (setq buffer (get-buffer-create " *magit-todos-fontify*"))
      (with-current-buffer buffer
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
         (buf (generate-new-buffer (concat "*" name "*")))
         (proc (apply #'start-process name buf command args)))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'async-when-done)
      (unless (string= name "emacs")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

;;;;; grep

(cl-defun magit-todos--grep-scan-async (&key magit-status-buffer directory depth)
  "Return to-dos in DIRECTORY, scanning with grep."
  ;; NOTE: When dir-local variables are used, `with-temp-buffer' seems to reset them, so we must
  ;; capture them and pass them in.
  (let* ((depth (number-to-string (1+ depth)))
         (process-connection-type 'pipe)
         (grep-find-template (->> grep-find-template
                                  (s-replace " <D> "
                                             (concat " <D> -maxdepth " depth " "))
                                  (s-replace " grep " " grep -b -E ")
                                  (s-replace " -nH " " -H ")))
         ;; Modified from `rgrep-default-command'
         (command (-flatten
                   (append (list "find" directory)
                           (-non-nil (list (when grep-find-ignored-directories
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
                                                   ")" "-prune"))))
                           (list "-o" "-type" "f")
                           ;; NOTE: This uses "grep -P", i.e. "Interpret the pattern as a
                           ;; Perl-compatible regular expression (PCRE).  This is highly
                           ;; experimental and grep -P may warn of unimplemented features."  But it
                           ;; does seem to work properly, at least on GNU grep.  Using "grep -E"
                           ;; with this PCRE regexp doesn't work quite right, as it doesn't match
                           ;; all the keywords, but pcre2el doesn't convert to "extended regular
                           ;; expressions", so this will have to do.  Maybe we should test whether
                           ;; the version of grep installed supports "-P".
                           (list "-exec" "grep" "-bPH" magit-todos-search-regexp "{}" "+")))))
    (when magit-todos-nice
      (setq command (append (list "nice" "-n5") command)))
    (magit-todos--async-start-process "magit-todos--grep-scan-async"
      :command command
      :finish-func (apply-partially #'magit-todos--grep-scan-async-callback magit-status-buffer))))

(defun magit-todos--grep-scan-async-callback (magit-status-buffer process)
  "Callback for `magit-todos--grep-scan-async'."
  ;; See <https://github.com/jwiegley/emacs-async/issues/101>
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (magit-todos--insert-items-callback
      magit-status-buffer
      (cl-loop for item = (magit-todos--next-item magit-todos-grep-result-regexp)
               while item
               unless (cl-loop for suffix in (-list magit-todos-ignore-file-suffixes)
                               thereis (s-suffix? suffix (magit-todos-item-filename item)))
               do (cl-callf f-relative (magit-todos-item-filename item) default-directory)
               and collect item
               do (forward-line 1)))))

;;;;; ag

(cl-defun magit-todos--ag-scan-async (&key magit-status-buffer directory depth)
  "Return to-dos in DIRECTORY, scanning with ag."
  ;; NOTE: When dir-local variables are used, `with-temp-buffer' seems to reset them, so we must
  ;; capture them and pass them in.
  (let* ((depth (number-to-string (1+ depth)))
         (process-connection-type 'pipe)
         (command (list "--ackmate" "--depth" depth
                        magit-todos-search-regexp directory)))
    (when magit-todos-ag-args
      (setq command (append (-flatten (--map (s-split (rx (1+ space)) it 'omit-nulls)
                                             magit-todos-ag-args))
                            command)))
    (push "ag" command)
    (when magit-todos-nice
      (setq command (append (list "nice" "-n5") command)))
    (magit-todos--async-start-process "magit-todos--ag-scan-async"
      :command command
      :finish-func (apply-partially #'magit-todos--ag-scan-async-callback magit-status-buffer))))

(defun magit-todos--ag-scan-async-callback (magit-status-buffer process)
  "Callback for `magit-todos--ag-scan-async'."
  ;; See <https://github.com/jwiegley/emacs-async/issues/101>
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (magit-todos--insert-items-callback
      magit-status-buffer
      (cl-loop while (looking-at (rx bol (1+ not-newline) eol))
               append (let ((filename (f-relative (buffer-substring (1+ (point-at-bol)) (point-at-eol)) default-directory)))
                        (forward-line 1)
                        (cl-loop for item = (magit-todos--next-item magit-todos-ag-result-regexp filename)
                                 while item
                                 unless (cl-loop for suffix in (-list magit-todos-ignore-file-suffixes)
                                                 thereis (s-suffix? suffix (magit-todos-item-filename item)))
                                 collect item
                                 do (forward-line 1)))
               do (forward-line 1)))))

;;;;; rg

(cl-defun magit-todos--rg-scan-async (&key magit-status-buffer directory depth)
  "Return to-dos in DIRECTORY, scanning with rg."
  ;; NOTE: When dir-local variables are used, `with-temp-buffer' seems to reset them, so we must
  ;; capture them and pass them in.
  (let* ((depth (number-to-string (1+ depth)))
         (process-connection-type 'pipe)
         (command (list "--column" "--maxdepth" depth
                        magit-todos-search-regexp directory)))
    (when magit-todos-rg-args
      (setq command (append (-flatten (--map (s-split (rx (1+ space)) it 'omit-nulls)
                                             magit-todos-rg-args))
                            command)))
    (push "rg" command)
    (when magit-todos-nice
      (setq command (append (list "nice" "-n5") command)))
    (magit-todos--async-start-process "magit-todos--rg-scan-async"
      :command command
      :finish-func (apply-partially #'magit-todos--rg-scan-async-callback magit-status-buffer))))

(defun magit-todos--rg-scan-async-callback (magit-status-buffer process)
  "Callback for `magit-todos--rg-scan-async'."
  ;; See <https://github.com/jwiegley/emacs-async/issues/101>
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (magit-todos--insert-items-callback
      magit-status-buffer
      (cl-loop while (looking-at (rx bol (1+ not-newline) eol))
               append (let ((filename (f-relative (buffer-substring (point-at-bol) (point-at-eol)) default-directory)))
                        (forward-line 1)
                        (cl-loop for item = (magit-todos--next-item magit-todos-rg-result-regexp filename)
                                 while item
                                 unless (cl-loop for suffix in (-list magit-todos-ignore-file-suffixes)
                                                 thereis (s-suffix? suffix (magit-todos-item-filename item)))
                                 collect item
                                 do (forward-line 1)))
               do (forward-line 1)))))

;;;;; git-grep

(cl-defun magit-todos--git-grep-scan-async (&key magit-status-buffer directory depth _timeout)
  "Return to-dos in DIRECTORY, scanning with git-grep."
  ;; NOTE: When dir-local variables are used, `with-temp-buffer' seems to reset them, so we must
  ;; capture them and pass them in.
  (let* ((depth (number-to-string (1+ depth)))
         (process-connection-type 'pipe)
         (command (list "--no-pager" "grep" "--full-name"
                        "--no-color" "-n" "--max-depth" depth
                        "--perl-regexp" "-e" magit-todos-search-regexp
                        "--" directory)))
    (when magit-todos-git-grep-args
      (setq command (append (-flatten (--map (s-split (rx (1+ space)) it 'omit-nulls)
                                             magit-todos-git-grep-args))
                            command)))
    (push magit-git-executable command)
    (when magit-todos-nice
      (setq command (append (list "nice" "-n5") command)))
    (magit-todos--async-start-process "magit-todos--git-grep-scan-async"
      :command command
      :finish-func (apply-partially #'magit-todos--git-grep-scan-async-callback magit-status-buffer))))

(defun magit-todos--git-grep-scan-async-callback (magit-status-buffer process)
  "Callback for `magit-todos--git-grep-scan-async'."
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    (magit-todos--insert-items-callback
      magit-status-buffer
      (cl-loop for item = (magit-todos--next-item magit-todos-git-grep-result-regexp)
               while item
               unless (cl-loop for suffix in (-list magit-todos-ignore-file-suffixes)
                               thereis (s-suffix? suffix (magit-todos-item-filename item)))
               collect item
               do (forward-line 1)))))

;;;;; Formatters

(defun magit-todos--format-plain (item)
  "Return ITEM formatted as from a non-Org file."
  (format "%s: %s"
          (propertize (magit-todos-item-keyword item)
                      'face (magit-todos--keyword-face (magit-todos-item-keyword item)))
          (or (magit-todos-item-description item) "")))

(defun magit-todos--format-org (item)
  "Return ITEM formatted as from an Org file."
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

;;;; Footer

(provide 'magit-todos)

;;; magit-todos.el ends here
