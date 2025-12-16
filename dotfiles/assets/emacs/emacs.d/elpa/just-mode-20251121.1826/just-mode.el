;;; just-mode.el --- Justfile editing mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Sony Corporation of America and Leon Barrett

;; Author: Leon Barrett (leon@barrettnexus.com)
;; Maintainer: Leon Barrett (leon@barrettnexus.com)
;; Package-Version: 20251121.1826
;; Package-Revision: b6173c7bf4d8
;; Package-Requires: ((emacs "26.1"))
;; Keywords: files languages tools
;; URL: https://github.com/leon-barrett/just-mode.el

;; This file is *NOT* part of GNU Emacs

;; This package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing justfiles, as defined by the tool "just":
;; https://github.com/casey/just

;;; Code:



;; NOTE: This depends on Emacs 26.1 for `prog-first-column'.

(require 'prog-mode)
(require 'subr-x)

;; TODO Use nested modes for rule bodies so e.g. we can have Python mode for a Python script.

(defgroup just nil
  "Major mode for editing just files"
  :group 'languages
  :prefix "just-"
  :link '(url-link :tag "Site" "https://github.com/leon-barrett/just-mode.el")
  :link '(url-link :tag "Repository" "https://github.com/leon-barrett/just-mode.el"))

(defconst just-builtin-functions
  '(
    ;; Built-in functions from https://github.com/casey/just/blob/9f03441eef28fd662b33a8f1961e2ee97b60f7ff/src/function.rs#L22
    "absolute_path"
    "arch"
    "capitalize"
    "clean"
    "env_var"
    "env_var_or_default"
    "error"
    "extension"
    "file_name"
    "file_stem"
    "invocation_directory"
    "invocation_directory_native"
    "join"
    "just_executable"
    "justfile"
    "justfile_directory"
    "kebabcase"
    "lowercamelcase"
    "lowercase"
    "os"
    "os_family"
    "parent_directory"
    "path_exists"
    "quote"
    "replace"
    "replace_regex"
    "sha256"
    "sha256_file"
    "shoutykebabcase"
    "shoutysnakecase"
    "snakecase"
    "titlecase"
    "trim"
    "trim_end"
    "trim_end_match"
    "trim_end_matches"
    "trim_start"
    "trim_start_match"
    "trim_start_matches"
    "uppercamelcase"
    "uppercase"
    "uuid"
    "without_extension"))

(defun just-keyword-regex (keywords)
  "Create a regex for a list of keywords.
Argument KEYWORDS the list of keywords"
  (concat "\\<\\(" (string-join keywords "\\|") "\\)\\>"))

(defconst just-font-lock-keywords
  `(;; Variable interpolation looks like "{{varname}}"
    ("{{[^}\n]*}}" . font-lock-variable-name-face)
    ;; File includes
    ("^\\(!include\\) \\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ;; Setting, exporting, and aliasing
    ("^\\(alias\\|set\\|export\\) +\\([^ \n]*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ;; Variable assignment looks like "varname :="
    ("^\\([^ \n]*\\) *:=" 1 font-lock-variable-name-face)
    ;; Highlight variable interpolation in shell scripts like "${varname}"
    ("\\${\\([^}\n]*\\)}" 1 font-lock-variable-name-face)
    ;; Highlight rules like "rulename:"
    ;; TODO highlight arguments to rules. I would have done it, but it was hard so I gave up for now.
    ("^\\(@?\\)\\([^ @:\n]+\\).*:\\([^=\n]\\|$\\)"
     (1 font-lock-negation-char-face)
     (2 font-lock-function-name-face))
    ;; Keywords
    (,(just-keyword-regex '("if" "else")) . font-lock-keyword-face)
    ;; Built-in functions
    (,(just-keyword-regex just-builtin-functions) . font-lock-constant-face)))

(defconst just-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; # starts comments
    (modify-syntax-entry ?# "<" syntax-table)
    ;; endline ends comments
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; underscores and dashes don't break words
    (modify-syntax-entry ?_ "w" syntax-table)
    (modify-syntax-entry ?- "w" syntax-table)
    ;; backticks are like quotes in shell
    (modify-syntax-entry ?` "\"" syntax-table)
    ;; single quotes are allowed
    (modify-syntax-entry ?' "\"" syntax-table)
    syntax-table))

(defun just-untab-region (N)
  "Untab a selected region using `indent-rigidly'.
Argument N number of untabs to perform"
  (interactive "p")
  (let ((begin (if (use-region-p)
                 (region-beginning)
                 (line-beginning-position)))
        (end (if (use-region-p)
                 (region-end)
                 (line-end-position))))
    (indent-rigidly begin end (* N -4))))

(defcustom just-executable "just"
  "Location of just executable."
  :type 'file
  :group 'just
  :safe 'stringp)

(defun just-format-buffer ()
  "Formats your buffer containing justfile."
  (interactive)
  (let ((exit-code (call-process just-executable nil nil nil "--unstable" "--fmt")))
    (if (eq exit-code 0)
        (revert-buffer :ignore-auto :noconfirm)
        (message "Formatted")
      (message "Format failed with exit code %s" exit-code))))

;; from https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
;; (which is licensed GPL 2 or later)
(defvar just-indent-offset 4 "My indentation offset.")
(defun just-backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively #'backward-delete-char-untabify)
    (let ((movement (% (current-column) just-indent-offset))
          (p (point)))
      (when (= movement 0) (setq movement just-indent-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively #'backward-delete-char))))))

(defun just-indent-line ()
  "Indent bodies of rules by the previous indent, or by `just-indent-offset'."
  (interactive)
  (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
       (expand-abbrev))
  (if (> (current-column) (current-indentation))
      ;; Don't indent when hitting tab in the middle of a line.
      'noindent
    (skip-chars-forward " \t")
    (indent-to
     (if (= (line-number-at-pos) (prog-first-column))
         (prog-first-column)
       (save-excursion
         (forward-line -1)
         (skip-chars-forward " \t")
         (let* ((previous-indentation (current-column))
                (previous-line-is-empty (and (bolp) (eolp)))
                (previous-line-contents (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (previous-line-is-rule (string-match "^[^ \t#:][^#:]*:\\([^=].*\\|$\\)" previous-line-contents)))
           (cond (previous-line-is-empty (prog-first-column))
                 (previous-line-is-rule (+ (prog-first-column) just-indent-offset))
                 (t previous-indentation))))))))

;;;###autoload
(define-derived-mode just-mode prog-mode "Justfile"
  "Major mode for editing standard Justfiles."

  :syntax-table just-mode-syntax-table

  ;; Font lock.
  (setq font-lock-defaults '(just-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")

  ;; Tabs
  (setq-local tab-width 4)
  (setq-local tab-stop-list (number-sequence 0 120 4))

  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width 4))

  ;; Imenu
  (setq-local imenu-generic-expression
              '(("setting" "^set +\\([A-Z_a-z][0-9A-Z_a-z-]*\\)\\(?:\\'\\|$\\| \\|:=\\)" 1)
                ("variable" "^\\(?:export +\\)?\\([A-Z_a-z][0-9A-Z_a-z-]*\\) *:=" 1)
                ("task" "^\\(?:alias +\\)?@?\\([A-Z_a-z][0-9A-Z_a-z-]*\\).*:[^=]" 1)))

  ;; Indentation
  (setq-local indent-line-function 'just-indent-line)
  (local-set-key (kbd "DEL") #'just-backspace-whitespace-to-tab-stop)
  (local-set-key (kbd "<backtab>") #'just-untab-region)
  (local-set-key (kbd "C-c '") #'just-src-edit))

;;; Task body editing

(defvar just-src-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") #'just-src-edit-save)
    (define-key map (kbd "C-c '") #'just-src-edit-sync-and-exit)
    (define-key map (kbd "C-c C-k") #'just-src-edit-abort)
    map)
  "Keymap for `just-src-edit-mode'.")

(define-minor-mode just-src-edit-mode
  "Minor mode for editing Just task bodies."
  :init-value nil
  :lighter " Just-Edit"
  :keymap just-src-edit-mode-map)

(defvar-local just-src-edit--original-buffer nil)
(put 'just-src-edit--original-buffer 'permanent-local t)

(defvar-local just-src-edit--beg-marker nil)
(put 'just-src-edit--beg-marker 'permanent-local t)

(defvar-local just-src-edit--end-marker nil)
(put 'just-src-edit--end-marker 'permanent-local t)

(defvar-local just-src-edit--indentation nil)
(put 'just-src-edit--indentation 'permanent-local t)

(defvar-local just-src-edit--task-name nil)
(put 'just-src-edit--task-name 'permanent-local t)

(defun just-src-edit--find-task-bounds ()
  "Find the bounds of the task at point.
Returns (task-name body-start body-end) or nil if not in a task."
  (save-excursion
    (let ((start-pos (point))
          (task-regexp "^@?\\([A-Z_a-z][0-9A-Z_a-z-]*\\).*:[^=]")
          task-name body-start body-end)

      ;; Find the task we're currently in using the same regex as imenu
      ;; Go back to find a task line
      (while (and (not (bobp))
                  (not (looking-at task-regexp)))
        (forward-line -1))

      ;; Task line found
      (when (looking-at task-regexp)
        (setq task-name (match-string 1))
        (setq body-start (line-beginning-position 2))

        ;; Find the next line with no indentation (column 0)
        (forward-line 1)
        (while (and (not (eobp))
                    (not (and (not (looking-at "^\\s-*$")) ; not empty line
                              (looking-at "^[^ \t\n]"))))  ; starts at column 0
          (forward-line 1))

        ;; Go back to find the last non-empty line before the unindented line
        (forward-line -1)
        (while (and (> (point) body-start)
                    (looking-at "^\\s-*$"))
          (forward-line -1))
        (setq body-end (line-end-position))

        ;; Ensure body-end is at least at body-start
        (when (<= body-end body-start)
          (setq body-end (save-excursion
                           (goto-char body-start)
                           (line-end-position))))

        ;; Check if original point was within the task body
        (when (and (>= start-pos body-start)
                   (<= start-pos body-end))
          (list task-name body-start body-end))))))

(defun just-src-edit--calculate-indentation (start end)
  "Calculate the common indentation for task body between START and END."
  (save-excursion
    (goto-char start)
    (let ((min-indent most-positive-fixnum))
      (while (< (point) end)
        (when (not (looking-at "^\\s-*$"))  ; skip empty lines
          (setq min-indent (min min-indent (current-indentation))))
        (forward-line 1))
      (if (= min-indent most-positive-fixnum) 0 min-indent))))

(defun just-src-edit--remove-indentation (content indentation)
  "Remove INDENTATION spaces from each line of CONTENT."
  (let ((lines (split-string content "\n")))
    (mapconcat
     (lambda (line)
       (if (string-match-p "^\\s-*$" line)
           line
         (if (and (>= (length line) indentation)
                  (string-prefix-p (make-string indentation ?\s) line))
             (substring line indentation)
           line)))
     lines
     "\n")))

(defun just-src-edit--add-indentation (content indentation)
  "Add INDENTATION spaces to each line of CONTENT."
  (let ((lines (split-string content "\n"))
        (indent-str (make-string indentation ?\s)))
    (mapconcat
     (lambda (line)
       (if (string-match-p "^\\s-*$" line)
           line
         (concat indent-str line)))
     lines
     "\n")))

(defun just-src-edit ()
  "Edit the task body at point in a dedicated buffer."
  (interactive)
  (let ((task-info (just-src-edit--find-task-bounds)))
    (unless task-info
      (user-error "Not in a task body"))

    (let* ((task-name (nth 0 task-info))
           (body-start (nth 1 task-info))
           (body-end (nth 2 task-info))
           (original-buffer (current-buffer))
           (indentation (just-src-edit--calculate-indentation body-start body-end))
           (content (buffer-substring-no-properties body-start body-end))
           (clean-content (just-src-edit--remove-indentation content indentation))
           (edit-buffer (generate-new-buffer (format "*Just Task: %s*" task-name))))

      ;; Switch to edit buffer
      (pop-to-buffer edit-buffer)

      ;; Insert content and set up buffer
      (insert clean-content)
      (goto-char (point-min))

      ;; Ensure buffer ends with newline
      (goto-char (point-max))
      (unless (and (> (point-max) (point-min))
                   (= (char-before) ?\n))
        (insert "\n"))
      (goto-char (point-min))

      ;; Detect and set appropriate mode
      (if (string-match "^#!" clean-content)
          (normal-mode)
        (sh-mode))

      ;; Set up buffer-local variables
      (setq just-src-edit--original-buffer original-buffer)
      (setq just-src-edit--beg-marker (with-current-buffer original-buffer
                                        (copy-marker body-start)))
      (setq just-src-edit--end-marker (with-current-buffer original-buffer
                                        (copy-marker body-end t)))
      (setq just-src-edit--indentation indentation)
      (setq just-src-edit--task-name task-name)

      ;; Enable the minor mode for keybindings
      (just-src-edit-mode 1)

      ;; Show helpful message
      (message "Edit task '%s'. %s" task-name (just-src-edit--describe-bindings)))))

(defun just-src-edit--describe-bindings ()
  "Return a string describing the current key bindings for just-src-edit-mode."
  (let ((sync-key (substitute-command-keys "\\[just-src-edit-sync-and-exit]"))
        (save-key (substitute-command-keys "\\[just-src-edit-save]"))
        (abort-key (substitute-command-keys "\\[just-src-edit-abort]")))
    (format "%s to sync and exit, %s to save, %s to abort"
            sync-key save-key abort-key)))

(defun just-src-edit-save ()
  "Save the edited task back to the original buffer and file."
  (interactive)
  (just-src-edit--sync-to-original t))

(defun just-src-edit-sync-and-exit ()
  "Sync changes to original buffer and exit edit buffer."
  (interactive)
  (just-src-edit--sync-to-original nil)
  (quit-window t)
  (message "Synced changes to original buffer"))

(defun just-src-edit-abort ()
  "Abort editing without saving changes."
  (interactive)
  (quit-window t))

(defun just-src-edit--sync-to-original (save-file)
  "Sync the edited content back to the original buffer.
If SAVE-FILE is non-nil, also save the original buffer."
  (unless (and just-src-edit--original-buffer
               (buffer-live-p just-src-edit--original-buffer))
    (error "Original buffer no longer exists"))

  (let ((content (string-trim-right (buffer-string) "\n+"))
        (original-buf just-src-edit--original-buffer)
        (beg just-src-edit--beg-marker)
        (end just-src-edit--end-marker)
        (indent just-src-edit--indentation))

    (with-current-buffer original-buf
      (save-excursion
        (let ((indented-content (just-src-edit--add-indentation content indent)))
          (goto-char beg)
          (delete-region beg end)
          (insert indented-content)
          (set-marker end (point))))

      (when save-file
        (save-buffer)))

    (set-buffer-modified-p nil)))

(provide 'just-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("/\\(?:\\.\\)?[jJ][uU][sS][tT][fF][iI][lL][eE]\\'" . just-mode))

;;; just-mode.el ends here
