;;; magit-todos.el --- Show source file TODOs in Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/magit-todos
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (a) (anaphora) (dash) (f) (hl-todo) (kv) (magit))
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
;; dash
;; f
;; hl-todo
;; kv
;; magit

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
(require 'seq)

(require 'a)
(require 'anaphora)
(require 'dash)
(require 'f)
(require 'hl-todo)
(require 'kv)
(require 'magit)

;;;; Variables

(defvar magit-todos-keywords-list nil
  "List of to-do keywords.
Set automatically by `magit-todos-keywords' customization.")

(defvar magit-todos-keywords-regexp nil
  "Regular expression matching desired to-do keywords in source and Org files.
This should generally be set automatically by customizing
`magit-todos-keywords'.")

(defvar magit-todos-ignored-directories nil
  "Automatically set by `magit-todos--repo-todos'.")

(defvar magit-todo-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] #'magit-todos--goto-item)
    m))

;;;; Customization

(defgroup magit-todos nil
  "Show TODO items in source code comments in repos' files."
  :group 'magit)

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

When set, sets `magit-todos-keywords-regexp' to the appropriate
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
                 magit-todos-keywords-regexp (rx-to-string `(or
                                                             ;; Org item
                                                             (seq bol (group-n 1 (1+ "*"))
                                                                  (1+ blank)
                                                                  (group-n 2 (or ,@keywords))
                                                                  (1+ space)
                                                                  (group-n 3 (1+ not-newline)))
                                                             ;; Non-Org
                                                             (seq (or bol (1+ blank))
                                                                  (group-n 2 (or ,@keywords))
                                                                  ;; Require the : to avoid spurious items
                                                                  ":"
                                                                  (optional (1+ blank)
                                                                            (group-n 3 (1+ not-newline))))))))))

(defcustom magit-todos-max-items 20
  "Automatically collapse the section if there are more than this many items."
  :type 'integer)

(defcustom magit-todos-recursive nil
  "Recurse into subdirectories when looking for to-do items.
This can take a long time in a large repo, so this defaults to
nil.  You might like to use a dir-local variable to set this in
certain repos."
  :type 'boolean)

(defcustom magit-todos-scan-files #'magit-list-files
  "Function that returns a list of files in a repo to scan for to-do items.
See `magit-todos-recursive' for recursive settings.  The function
should take one argument, the path to the repo's working tree.
Note that if it recurses into subdirectories, it is responsible
for skipping the \".git\" directory."
  :type '(choice (const :tag "Files tracked by git" #'magit-list-files)
                 (const :tag "All files in working tree" #'magit-todos--working-tree-files)
                 (function :tag "Custom function")))

(defvar magit-todos-ignore-directories-always
  '(".git" ".cask")
  ;; Must be defined before `magit-todos-ignore-directories'.
  "Directories which should always be ignored.
Users should customize `magit-todos-ignore-directories'.")

(defcustom magit-todos-ignore-directories nil
  "Directories to ignore in repos.
Directories listed in `magit-todos-ignore-directories-always' are
always ignored, even if not listed here."
  :type '(repeat string)
  :set (lambda (option value)
         (set-default option value)
         (setq-default magit-todos-ignored-directories (seq-uniq (append magit-todos-ignore-directories-always value)))))

(defcustom magit-todos-ignore-file-suffixes '(".org_archive")
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

(defcustom magit-todos-depth nil
  "Maximum depth of files in repo working tree to scan for to-dos."
  ;; TODO: Make depth setting work
  ;; TODO: Automatic depth setting that works well in large repos
  :type '(choice (const :tag "No limit" nil)
                 integer))

;;;; Commands

(define-minor-mode magit-todos-mode
  "Show list of to-do items in Magit status buffer for tracked files in repo."
  :group 'magit-todos
  :global t
  (if magit-todos-mode
      (magit-add-section-hook 'magit-status-sections-hook
                              #'magit-todos--insert-items
                              'magit-insert-staged-changes
                              'append)
    (remove-hook 'magit-status-sections-hook #'magit-todos--insert-items)))

(defun magit-todos--goto-item ()
  "Go to to-do item at point."
  (interactive)
  (pcase-let* ((item (magit-current-section))
               ((eieio value) item)
               ((map (:filename file) (:position position)) value))
    (switch-to-buffer (or (find-buffer-visiting file)
                          (find-file-noselect file)))
    (goto-char position)))

;;;; Functions

(defun magit-todos--repo-todos (&optional path)
  "Return to-do items for repo at PATH.
PATH defaults to `default-directory'."
  (let* ((magit-todos-ignored-directories (seq-uniq (append magit-todos-ignore-directories-always magit-todos-ignore-directories)))
         (default-directory (or path default-directory))
         (files (->> (funcall magit-todos-scan-files)
                     magit-todos--filter-files)))
    (--> files
         (-map #'magit-todos--file-todos it)
         (-non-nil it)
         (-flatten-n 1 it)
         (magit-todos--sort it))))

(defun magit-todos--working-tree-files ()
  "Return list of all files in working tree."
  (f-files default-directory nil magit-todos-recursive))

(defun magit-todos--filter-files (files)
  "Return FILES without ignored ones.
FILES should be a list of files, already flattened."
  (--> files
       (--remove (cl-loop for suffix in (-list magit-todos-ignore-file-suffixes)
                          thereis (s-suffix? suffix it))
                 it)
       (--remove (cl-loop for dir in magit-todos-ignored-directories
                          thereis (string= dir (f-base it)))
                 it)))

(defun magit-todos--file-todos (file)
  "Return to-do items for FILE.
If FILE is not being visited, it is visited and then its buffer
is killed."
  (cl-symbol-macrolet ((position (match-beginning 0))
                       (org-level (match-string 1))
                       (keyword (match-string 2))
                       (description (or (match-string 3) "")))
    (let ((case-fold-search magit-todos-ignore-case)
          (string-fn (lambda ()
                       (format "%s: %s"
                               (propertize keyword 'face (magit-todos--keyword-face keyword))
                               description)))
          (base-directory default-directory)
          (enable-local-variables nil)
          kill-buffer filename)
      (catch 'not-a-file
        ;; `magit-list-files' seems to return directories sometimes, and we should skip those, so we
        ;; check here when it's not open in a buffer to avoid calling `f-file?' on every "file".
        ;; NOTE: I think this will work, but I'm not sure if `find-buffer-visiting' could return
        ;; e.g. a dired buffer, so we might need to check every file after all, but I'd like to
        ;; avoid that since it means a lot of calls.
        (with-current-buffer (cl-typecase file
                               (buffer file)
                               (string (or (find-buffer-visiting file)
                                           (progn
                                             (unless (f-file? file)
                                               (throw 'not-a-file nil))
                                             (setq kill-buffer t)
                                             ;; NOTE: Not sure if nowarn is needed, but it seems
                                             ;; like a good idea, because we don't want this to
                                             ;; raise any errors.
                                             (find-file-noselect file 'nowarn 'raw)))))
          (setq filename (f-relative (buffer-file-name) base-directory))
          (when (and magit-todos-fontify-org
                     (string= "org" (f-ext (buffer-file-name))))
            ;; TODO: Capture Org priority and allow sorting by it.
            (setq string-fn (lambda ()
                              (org-fontify-like-in-org-mode
                               (format "%s %s %s" org-level keyword description)))))
          (prog1 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (cl-loop while (re-search-forward magit-todos-keywords-regexp nil 'noerror)
                              ;; TODO: Move string formatting to end of process and experiment with alignment
                              collect (a-list :filename filename
                                              :keyword keyword
                                              :position position
                                              :string (funcall string-fn)))))
            (when kill-buffer
              (kill-buffer))))))))

(defun magit-todos--insert-items ()
  "Insert to-do items into current buffer."
  (when-let ((items (magit-todos--repo-todos))
             (magit-section-show-child-count t)
             (magit-section-set-visibility-hook (cons (with-no-warnings
                                                        (lambda (&rest ignore)
                                                          (when (> (length items) magit-todos-max-items)
                                                            'hide)))
                                                      magit-section-set-visibility-hook)))
    (magit-insert-section (todos)
      (magit-insert-heading "TODOs:")
      (dolist (item items)
        (-let* (((&alist :filename filename :string string) item)
                (filename (propertize filename 'face 'magit-filename))
                (string (format "%s %s" filename string)))
          (magit-insert-section (todo item)
            (insert string)))
        (insert "\n"))
      (insert "\n"))))

(defun magit-todos--keyword-face (keyword)
  "Return face for KEYWORD."
  ;; TODO: Instead of upcasing here, upcase in the lookup, so it can still be displayed
  ;; non-uppercase.  Preserving the distinction might be useful.
  (when magit-todos-ignore-case
    (setq keyword (upcase keyword)))
  (atypecase (a-get hl-todo-keyword-faces keyword)
    (string (list :inherit 'hl-todo :foreground it))
    (t it)))

;;;;; Sorting

(defun magit-todos--sort (items)
  "Return ITEMS sorted according to `magit-todos-sort-order'."
  (dolist (fn (reverse magit-todos-sort-order) items)
    (setq items (sort items fn))))

(defun magit-todos--sort-by-keyword (a b)
  "Return non-nil if A's keyword is before B's in `magit-todos-keywords-list'."
  (cl-flet ((item-keyword (item)
                          (a-get item :keyword))
            (keyword-index (keyword)
                           (or (-elem-index keyword magit-todos-keywords-list) 0)))
    (< (keyword-index (item-keyword a))
       (keyword-index (item-keyword b)))))

(defun magit-todos--sort-by-position (a b)
  "Return non-nil if A's position in its file is before B's."
  (let ((a-position (a-get a :position))
        (b-position (a-get b :position)))
    (< a-position b-position)))

(defun magit-todos--sort-by-filename (a b)
  "Return non-nil if A's filename is `string<' B's."
  (let ((a-filename (a-get a :filename))
        (b-filename (a-get b :filename)))
    (string< a-filename b-filename)))

;;;; Footer

(provide 'magit-todos)

;;; magit-todos.el ends here
